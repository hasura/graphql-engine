//! everything from `plan` slowly moves here, binning as much as possible along the way
// ideally we'll bin off everything around GraphQL-specific nodes and leave those to the GraphQL
// frontend
mod ndc_request;
mod remote_joins;
use super::ndc;
use super::process_response::process_response;
use super::{HttpContext, ProjectId};
use crate::error::FieldError;
use crate::process_response::ProcessedResponse;
use gql::normalized_ast;
use graphql_schema::GDS;
use lang_graphql as gql;
use plan_types::{JoinLocations, NDCQueryExecution, ProcessResponseAs, QueryExecutionPlan};

// run ndc query, do any joins, and process result
pub async fn resolve_ndc_query_execution<'ir>(
    http_context: &HttpContext,
    ndc_query: NDCQueryExecution,
    selection_set: &normalized_ast::SelectionSet<'ir, GDS>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    let NDCQueryExecution {
        execution_tree,
        execution_span_attribute,
        ref field_span_attribute,
        process_response_as,
    } = ndc_query;

    let response_rowsets = execute_ndc_query(
        http_context,
        execution_tree.query_execution_plan,
        field_span_attribute,
        execution_span_attribute,
        project_id,
    )
    .await?;

    process_ndc_query_response(
        http_context,
        execution_tree.remote_join_executions,
        execution_span_attribute,
        selection_set,
        process_response_as,
        project_id,
        response_rowsets,
    )
    .await
}

async fn execute_ndc_query<'s, 'ir>(
    http_context: &HttpContext,
    query_execution_plan: QueryExecutionPlan,
    field_span_attribute: &str,
    execution_span_attribute: &'static str,
    project_id: Option<&ProjectId>,
) -> Result<Vec<ndc_models::RowSet>, FieldError> {
    let data_connector = query_execution_plan.data_connector.clone();
    let query_request = ndc_request::make_ndc_query_request(query_execution_plan)?;

    let response = ndc::execute_ndc_query(
        http_context,
        &query_request,
        &data_connector,
        execution_span_attribute,
        field_span_attribute.to_owned(),
        project_id,
    )
    .await?;

    Ok(response.as_latest_rowsets())
}

// given results of ndc query, do any joins, and process result
async fn process_ndc_query_response<'s, 'ir>(
    http_context: &HttpContext,
    remote_join_executions: JoinLocations,
    execution_span_attribute: &'static str,
    selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
    process_response_as: ProcessResponseAs,
    project_id: Option<&ProjectId>,
    mut response_rowsets: Vec<ndc_models::RowSet>,
) -> Result<ProcessedResponse, FieldError> {
    // TODO: Failures in remote joins should result in partial response
    // https://github.com/hasura/v3-engine/issues/229
    remote_joins::execute_join_locations(
        http_context,
        execution_span_attribute,
        &mut response_rowsets,
        &process_response_as,
        &remote_join_executions,
        project_id,
    )
    .await?;

    // this ties all of this to GraphQL, let's not do this
    process_response(selection_set, response_rowsets, &process_response_as)
}
