use std::sync::Arc;

use super::parse;
use super::process_response;
use super::types::{QueryResult, RelationshipTree, RequestError};
use crate::catalog::{Catalog, Model, State};
use axum::http::{HeaderMap, Method, Uri};
use engine_types::HttpContext;
use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use plan_types::{NDCQueryExecution, ProcessResponseAs};
use tracing_util::SpanVisibility;

#[allow(clippy::unused_async)]
pub async fn handler_internal<'metadata>(
    request_headers: Arc<HeaderMap>,
    http_context: Arc<HttpContext>,
    session: Arc<Session>,
    catalog: &Catalog,
    metadata: Arc<Metadata>,
    http_method: Method,
    uri: Uri,
    query_string: jsonapi_library::query::Query,
) -> Result<jsonapi_library::api::DocumentData, RequestError> {
    let tracer = tracing_util::global_tracer();

    let state = catalog
        .state_per_role
        .get(&session.role)
        .ok_or_else(|| RequestError::NotFound)?;

    // relationship tree for processing the response
    let mut relationship_tree = RelationshipTree::default();

    // route matching/validation
    match validate_route(state, &uri) {
        None => Err(RequestError::NotFound),
        Some(model) => {
            // create the query IR
            let query_ir = tracer.in_span(
                "create_query_ir",
                "Create query IR",
                SpanVisibility::User,
                || {
                    parse::create_query_ir(
                        model,
                        &state.object_types,
                        &http_method,
                        &uri,
                        &mut relationship_tree,
                        &query_string,
                    )
                },
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
                            &metadata,
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
                || Ok(process_response::process_result(result, &relationship_tree)),
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
    http_context: &Arc<HttpContext>,
    request_headers: &HeaderMap,
) -> Result<QueryResult, RequestError> {
    let execution_plan = plan::plan_query_request(query_ir, metadata, session, request_headers)
        .map_err(RequestError::PlanError)?;
    match execution_plan {
        plan::ExecutionPlan::Queries(queries) => match queries.first() {
            Some((_alias, query_execution)) => {
                let ndc_query_execution = NDCQueryExecution {
                    execution_span_attribute: "REST",
                    execution_tree: query_execution.execution_tree.clone(),
                    field_span_attribute: "REST".into(),
                    process_response_as: ProcessResponseAs::Array { is_nullable: false },
                };
                let rowsets =
                    execute::resolve_ndc_query_execution(http_context, ndc_query_execution, None)
                        .await
                        .map_err(RequestError::ExecuteError)?;

                Ok(QueryResult {
                    rowsets,
                    type_name: query_execution.query_context.type_name.clone(),
                })
            }
            None => todo!("handle empty query result in JSONAPI"),
        },
        plan::ExecutionPlan::Mutation(_) => {
            todo!("Executing mutations not implemented in JSONAPI yet")
        }
    }
}
