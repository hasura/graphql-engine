use std::sync::Arc;

use super::parse;
use super::process_response;
use super::types::{Catalog, Model, QueryResult, RequestError, State};
use axum::http::{HeaderMap, Method, Uri};
use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use tracing_util::SpanVisibility;

#[allow(clippy::unused_async)]
pub async fn handler_internal<'metadata>(
    request_headers: Arc<HeaderMap>,
    http_context: Arc<execute::HttpContext>,
    session: Arc<Session>,
    catalog: &Catalog,
    metadata: &'metadata Metadata,
    http_method: Method,
    uri: Uri,
    query_string: jsonapi_library::query::Query,
) -> Result<jsonapi_library::api::DocumentData, RequestError> {
    let tracer = tracing_util::global_tracer();

    let state = catalog
        .state_per_role
        .get(&session.role)
        .ok_or_else(|| RequestError::NotFound)?;

    // route matching/validation
    match validate_route(state, &uri) {
        None => Err(RequestError::NotFound),
        Some(model) => {
            // create the query IR
            let query_ir = tracer.in_span(
                "create_query_ir",
                "Create query IR",
                SpanVisibility::User,
                || parse::create_query_ir(model, &http_method, &uri, &query_string),
            )?;

            // execute the query with the query-engine
            let result = tracer
                .in_span_async(
                    "query_engine_execute",
                    "Execute query",
                    SpanVisibility::User,
                    || {
                        Box::pin(query_engine_execute(
                            &query_ir,
                            metadata,
                            &session,
                            &http_context,
                            &request_headers,
                        ))
                    },
                )
                .await?;

            // process result to JSON:API compliant response
            tracer.in_span(
                "process_response",
                "Process response",
                SpanVisibility::User,
                || Ok(process_response::process_result(result)),
            )
        }
    }
}

fn validate_route<'a>(state: &'a State, uri: &'a Uri) -> Option<&'a Model> {
    // TODO: to_string() maybe not optimal. Optimize later
    let uri_s = uri.to_string();
    for (route, model) in &state.routes {
        if uri_s.starts_with(route) {
            return Some(model);
        }
    }
    None
}

async fn query_engine_execute(
    query_ir: &open_dds::query::QueryRequest,
    metadata: &Metadata,
    session: &Arc<Session>,
    http_context: &Arc<execute::HttpContext>,
    request_headers: &HeaderMap,
) -> Result<QueryResult, RequestError> {
    let (query_execution_plan, query_context) =
        plan::plan_query_request(query_ir, metadata, session, http_context, request_headers)
            .await
            .map_err(RequestError::PlanError)?;
    match query_execution_plan {
        plan::SingleNodeExecutionPlan::Query(plan) => {
            let rowsets = resolve_ndc_query_execution(http_context, plan)
                .await
                .map_err(RequestError::ExecuteError)?;

            Ok(QueryResult {
                rowsets,
                type_name: query_context.type_name,
            })
        }
        plan::SingleNodeExecutionPlan::Mutation(_) => {
            todo!("Executing mutations not implemented in JSONAPI yet")
        }
    }
}

// run ndc query, do any joins, and process result
async fn resolve_ndc_query_execution<'ir>(
    http_context: &execute::HttpContext,
    query_execution_plan: execute::ResolvedQueryExecutionPlan,
) -> Result<Vec<ndc_models::RowSet>, execute::FieldError> {
    let data_connector = query_execution_plan.data_connector.clone();
    let query_request = execute::plan::ndc_request::make_ndc_query_request(query_execution_plan)?;

    let response = execute::ndc::execute_ndc_query(
        http_context,
        &query_request,
        &data_connector,
        "jsonapi",
        "jsonapi".to_owned(),
        None, // TODO: plumb in project id
    )
    .await?;

    Ok(response.as_latest_rowsets())
}
