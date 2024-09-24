mod process_response;
use axum::{
    http::{Method, Request, Uri},
    middleware::Next,
};
use indexmap::IndexMap;
use metadata_resolve::{Metadata, ModelExpressionType, ModelWithPermissions, Qualified};
use open_dds::{
    identifier,
    identifier::{Identifier, SubgraphName},
    models::ModelName,
    types::CustomTypeName,
};
use std::collections::{BTreeMap, HashMap};
use tracing_util::{ErrorVisibility, SpanVisibility, TraceableError, TraceableHttpResponse};

#[derive(Debug)]
pub struct State {
    pub routes: HashMap<String, ModelWithPermissions>,
}

impl State {
    pub fn new(metadata: &metadata_resolve::Metadata) -> Self {
        let routes = metadata
            .models
            .iter()
            // TODO: remove model.clone()
            .map(|(model_name, model)| {
                (
                    format!("/{}/{}", model_name.subgraph, model_name.name),
                    model.clone(),
                )
            })
            .collect::<HashMap<_, _>>();
        Self { routes }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum RequestError {
    NotFound,
    BadRequest(String),
    InternalError(InternalError),
    PlanError(plan::PlanError),
    ExecuteError(execute::FieldError),
}

#[derive(Debug, derive_more::Display)]
pub enum InternalError {
    EmptyQuerySet,
}

#[allow(clippy::unused_async)]
pub async fn handler_internal<'metadata>(
    http_context: &execute::HttpContext,
    state: &State,
    metadata: &'metadata Metadata,
    http_method: Method,
    uri: Uri,
    query_string: jsonapi_library::query::Query,
) -> Result<jsonapi_library::api::DocumentData, RequestError> {
    // route matching/validation
    match validate_route(state, &uri) {
        None => Err(RequestError::NotFound),
        Some(model) => {
            // create the query IR
            let query_ir = create_query_ir(model, &http_method, &uri, &query_string)?;
            // execute the query with the query-engine
            let result = query_engine_execute(http_context, &query_ir, metadata).await?;
            // process result to JSON:API compliant response
            Ok(process_response::process_result(result))
        }
    }
}

fn validate_route<'a>(state: &'a State, uri: &'a Uri) -> Option<&'a ModelWithPermissions> {
    // TODO: to_string() maybe not optimal. Optimize later
    let uri_s = uri.to_string();
    for (route, model) in &state.routes {
        if uri_s.starts_with(route) {
            return Some(model);
        }
    }
    None
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

async fn query_engine_execute(
    http_context: &execute::HttpContext,
    query_ir: &open_dds::query::QueryRequest,
    metadata: &Metadata,
) -> Result<QueryResult, RequestError> {
    let (query_execution_plan, query_context) =
        plan::plan_query_request(query_ir, metadata).map_err(RequestError::PlanError)?;

    let rowsets = resolve_ndc_query_execution(http_context, query_execution_plan)
        .await
        .map_err(RequestError::ExecuteError)?;

    Ok(QueryResult {
        rowsets,
        type_name: query_context.type_name,
    })
}

// run ndc query, do any joins, and process result
async fn resolve_ndc_query_execution<'s, 'ir>(
    http_context: &execute::HttpContext,
    query_execution_plan: execute::UnresolvedQueryExecutionPlan<'s>,
) -> Result<Vec<ndc_models::RowSet>, execute::FieldError> {
    execute::plan::execute_ndc_query(
        http_context,
        query_execution_plan,
        "jsonapi",
        "jsonapi",
        None,
    )
    .await
}

fn create_query_ir(
    model: &ModelWithPermissions,
    _http_method: &Method,
    uri: &Uri,
    query_string: &jsonapi_library::query::Query,
) -> Result<open_dds::query::QueryRequest, RequestError> {
    // get model info from parsing URI
    let ModelInfo {
        subgraph,
        name: model_name,
        unique_identifier: _,
        relationship: _,
    } = parse_url(uri)?;

    // create the selection fields; include all fields of the model output type
    // TODO: parse 'sparse fieldsets' to include specific fields only
    let mut selection = IndexMap::new();
    for (field_name, _field_def) in &model.model.type_fields {
        let field_name_ident = Identifier::new(field_name.as_str()).unwrap();
        let field_name = open_dds::types::FieldName::new(field_name_ident.clone());
        let field_alias = open_dds::query::Alias::new(field_name_ident);
        let sub_sel =
            open_dds::query::ObjectSubSelection::Field(open_dds::query::ObjectFieldSelection {
                target: open_dds::query::ObjectFieldTarget {
                    arguments: IndexMap::new(),
                    field_name,
                },
                selection: None,
            });
        selection.insert(field_alias, sub_sel);
    }

    // create filters
    let filter_query = query_string
        .filter
        .as_ref()
        .map(|filter| build_boolean_expression(model, filter));

    // create sorts
    let sort_query = match &query_string.sort {
        None => Ok(vec![]),
        Some(sort) => sort
            .iter()
            .map(|elem| Ok(build_order_by_element(elem)?))
            .collect::<Result<Vec<_>, RequestError>>(),
    }?;

    // pagination
    // spec: <https://jsonapi.org/format/#fetching-pagMetadata>
    // FIXME: unwrap
    let limit = query_string
        .page
        .as_ref()
        .map(|page| usize::try_from(page.size).unwrap());

    let offset = query_string
        .page
        .as_ref()
        .map(|page| usize::try_from(page.number).unwrap());

    // form the model selection
    let model_selection = open_dds::query::ModelSelection {
        selection,
        target: open_dds::query::ModelTarget {
            arguments: IndexMap::new(),
            filter: filter_query,
            order_by: sort_query,
            limit,
            offset,
            model_name,
            subgraph,
        },
    };
    let queries = IndexMap::from_iter([(
        open_dds::query::Alias::new(identifier!("jsonapi_model_query")),
        open_dds::query::Query::Model(model_selection),
    )]);
    Ok(open_dds::query::QueryRequest::V1(
        open_dds::query::QueryRequestV1 { queries },
    ))
}

// Sorting spec: <https://jsonapi.org/format/#fetching-sorting>
fn build_order_by_element(elem: &String) -> Result<open_dds::query::OrderByElement, ParseError> {
    let (field_name, direction) = if elem.starts_with('-') {
        (
            elem.split_at(1).1.to_string(),
            open_dds::models::OrderByDirection::Desc,
        )
    } else {
        (elem.to_string(), open_dds::models::OrderByDirection::Asc)
    };

    let operand = open_dds::query::Operand::Field(open_dds::query::ObjectFieldOperand {
        target: Box::new(open_dds::query::ObjectFieldTarget {
            field_name: open_dds::types::FieldName::new(Identifier::new(field_name)?),
            arguments: IndexMap::new(),
        }),
        nested: None,
    });
    Ok(open_dds::query::OrderByElement { operand, direction })
}

/* Example filter as query string - /movies?sort=...&filter=<json>
{
  "$or": [
    {
      "name": {
        "$eq": "Braveheart"
      }
    },
    {
      "name": {
        "$eq": "Gladiator"
      }
    },
    {
      "$and": [
        {
          "director": {
            "last_name": {
              "$eq": "Scorcese"
            }
          }
        },
        {
          "director": {
            "age": {
              "$gt": 50
            }
          }
        }
      ]
    }
  ],
  "name": {
    "$eq": "Foobar"
  }
}
*/
fn build_boolean_expression(
    model: &ModelWithPermissions,
    _filter: &BTreeMap<String, Vec<String>>,
) -> open_dds::query::BooleanExpression {
    // TODO: actually parse and create a BooleanExpression
    if let Some(filter_exp_type) = &model.filter_expression_type {
        match filter_exp_type {
            ModelExpressionType::BooleanExpressionType(_x) => {}
            ModelExpressionType::ObjectBooleanExpressionType(_x) => {}
        }
    }
    // dummy return
    open_dds::query::BooleanExpression::IsNull(open_dds::query::Operand::Field(
        open_dds::query::ObjectFieldOperand {
            target: Box::new(open_dds::query::ObjectFieldTarget {
                field_name: open_dds::types::FieldName::new(identifier!("dummy")),
                arguments: IndexMap::new(),
            }),
            nested: None,
        },
    ))
}

/// Model related info derived from URI path
#[allow(dead_code)]
struct ModelInfo {
    subgraph: SubgraphName,
    name: ModelName,
    /// value of the unique identifier of the model.
    // TODO: this won't be a string always
    unique_identifier: Option<String>,
    /// path to a relationship; like `["artist", "albums", "tracks"]`
    relationship: Vec<String>,
}

struct ParseError(String);

impl From<&str> for ParseError {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<ParseError> for RequestError {
    fn from(value: ParseError) -> Self {
        Self::BadRequest(format!("Parse Error: {}", value.0))
    }
}

fn parse_url(uri: &Uri) -> Result<ModelInfo, ParseError> {
    let path = uri.path();
    let paths = path
        .split('/')
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>();
    if paths.len() < 2 {
        return Err("Path length MUST BE >=2".into());
    }
    let subgraph = paths[0];
    let model_name = paths[1];
    let unique_identifier = paths.get(2).map(|x| (*x).to_string());
    let mut relationship = Vec::new();
    if paths.get(3).is_some() {
        relationship = paths[3..].iter().map(|i| (*i).to_string()).collect();
    }
    Ok(ModelInfo {
        name: ModelName::new(Identifier::new(model_name)?),
        subgraph: SubgraphName::try_new(subgraph)?,
        unique_identifier,
        relationship,
    })
}

// this is not the correct output type, we should be outputting a JSONAPI document instead
struct QueryResult {
    pub type_name: Qualified<CustomTypeName>,
    pub rowsets: Vec<ndc_models::RowSet>,
}

/// Middleware to start tracing of the `/v1/jsonapi` request. This middleware
/// must be active for the entire duration of the request i.e. this middleware
/// should be the entry point and the exit point of the JSON:API request.
pub async fn rest_request_tracing_middleware<B: Send>(
    request: Request<B>,
    next: Next<B>,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = "/v1/jsonapi";
    tracer
        .in_span_async_with_parent_context(
            path,
            path,
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                Box::pin(async move {
                    let response = next.run(request).await;
                    TraceableHttpResponse::new(response, path)
                })
            },
        )
        .await
        .response
}
