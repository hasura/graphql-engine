use std::sync::Arc;

use super::parse;
use super::process_response;
use super::types::{RelationshipTree, RequestError};
use crate::catalog::{Catalog, Model, State};
use axum::http::{HeaderMap, Method, Uri};
use engine_types::HttpContext;
use hasura_authn_core::Session;
use metadata_resolve::LifecyclePluginConfigs;
use metadata_resolve::Metadata;
use plan_types::{NDCQueryExecution, ProcessResponseAs};
use tracing_util::SpanVisibility;

#[allow(clippy::unused_async)]
pub async fn handler_internal(
    request_headers: Arc<HeaderMap>,
    http_context: Arc<HttpContext>,
    plugins: Arc<LifecyclePluginConfigs>,
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
            let rowsets = tracer
                .in_span_async(
                    "query_engine_execute",
                    "Execute query",
                    SpanVisibility::User,
                    || {
                        Box::pin(query_engine_execute(
                            &query_ir.query_request,
                            &metadata,
                            &session,
                            &http_context,
                            &plugins,
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
                || {
                    process_response::process_result(
                        rowsets,
                        &query_ir.root_type_name,
                        &relationship_tree,
                        &query_string,
                        &state.object_types,
                    )
                },
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
    session: &Session,
    http_context: &Arc<HttpContext>,
    plugins: &LifecyclePluginConfigs,
    request_headers: &HeaderMap,
) -> Result<Vec<ndc_models::RowSet>, RequestError> {
    let execution_plan = plan::plan_query_request(query_ir, metadata, session, request_headers)
        .map_err(RequestError::PlanError)?;
    match execution_plan {
        plan::ExecutionPlan::Queries(queries) => match queries.first() {
            Some((_alias, execution_tree)) => {
                let ndc_query_execution = NDCQueryExecution {
                    execution_span_attribute: "REST",
                    execution_tree: execution_tree.clone(),
                    field_span_attribute: "REST".into(),
                    process_response_as: ProcessResponseAs::Array { is_nullable: false },
                };
                Ok(execute::resolve_ndc_query_execution(
                    http_context,
                    plugins,
                    session,
                    &axum::http::HeaderMap::new(), // TODO: Pass actual request headers
                    ndc_query_execution,
                    None,
                )
                .await
                .map_err(RequestError::ExecuteError)?)
            }
            None => todo!("handle empty query result in JSONAPI"),
        },
        plan::ExecutionPlan::Mutation(_) => {
            todo!("Executing mutations not implemented in JSONAPI yet")
        }
    }
}
