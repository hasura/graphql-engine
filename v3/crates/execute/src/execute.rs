//! functions for executing queries, mutations, joins and remote predicates
//! should not contain frontend-specific logic
use crate::error;
use std::sync::Arc;
mod ndc_request;
mod remote_joins;
mod remote_predicates;
use crate::error::FieldError;
use crate::ndc;
use crate::FieldInternalError;
use async_recursion::async_recursion;
use engine_types::{HttpContext, ProjectId};
use indexmap::IndexMap;
pub use ndc_request::{
    make_ndc_mutation_request, make_ndc_query_request, v01::NdcV01CompatibilityError,
};
use plan_types::{
    JoinLocations, NDCMutationExecution, NDCQueryExecution, NDCSubscriptionExecution,
    PredicateQueryTrees, ProcessResponseAs, QueryExecutionPlan, QueryExecutionTree,
    RemotePredicateKey, ResolvedFilterExpression, FUNCTION_IR_VALUE_COLUMN_NAME,
};
pub use remote_predicates::replace_predicates_in_query_execution_plan;
use std::collections::BTreeMap;

// run ndc query, do any joins, and process result
pub async fn resolve_ndc_query_execution(
    http_context: &HttpContext,
    ndc_query: NDCQueryExecution,
    project_id: Option<&ProjectId>,
) -> Result<Vec<ndc_models::RowSet>, FieldError> {
    let NDCQueryExecution {
        execution_tree,
        execution_span_attribute,
        ref field_span_attribute,
        process_response_as,
    } = ndc_query;

    execute_query_execution_tree(
        http_context,
        execution_tree,
        field_span_attribute,
        execution_span_attribute,
        &process_response_as,
        project_id,
        &BTreeMap::new(), // no remote predicate context to begin with
    )
    .await
}

// run a PredicateQueryTree, turning it into a `ResolvedFilterExpression`
#[async_recursion]
pub async fn execute_remote_predicates(
    remote_predicates: &PredicateQueryTrees,
    http_context: &HttpContext,
    field_span_attribute: &str,
    execution_span_attribute: &'static str,
    process_response_as: &ProcessResponseAs,
    project_id: Option<&ProjectId>,
) -> Result<BTreeMap<RemotePredicateKey, ResolvedFilterExpression>, FieldError> {
    // resolve all our filter expressions into here, ready to && them in
    // at the appropriate moment
    let mut filter_expressions = BTreeMap::new();

    for (uuid, remote_predicate) in &remote_predicates.0 {
        // don't bother recursing if it's empty
        if !remote_predicate.children.0.is_empty() {
            let child_filter_expressions = execute_remote_predicates(
                &remote_predicate.children,
                http_context,
                field_span_attribute,
                execution_span_attribute,
                process_response_as,
                project_id,
            )
            .await?;
            filter_expressions.extend(child_filter_expressions);
        }

        // execute our remote predicate, including everything we have learned
        // from the child predicates
        let result_row_set = execute_query_execution_tree(
            http_context,
            remote_predicate.query.clone(),
            field_span_attribute,
            execution_span_attribute,
            process_response_as,
            project_id,
            &filter_expressions,
        )
        .await?;

        // Assume a single row set is returned
        let single_rowset = get_single_rowset(result_row_set)?;

        // Turn the results into a `ResolvedFilterExpression`
        let column_comparison = remote_predicates::build_source_column_comparisons(
            single_rowset.rows.unwrap_or_default(),
            &remote_predicate.ndc_column_mapping,
        )?;

        filter_expressions.insert(*uuid, column_comparison);
    }

    Ok(filter_expressions)
}

pub(crate) fn get_single_rowset(
    rows_sets: Vec<ndc_models::RowSet>,
) -> Result<ndc_models::RowSet, error::FieldError> {
    Ok(rows_sets
        .into_iter()
        .next()
        .ok_or(error::NDCUnexpectedError::BadNDCResponse {
            summary: "missing rowset".into(),
        })?)
}

#[async_recursion]
async fn execute_query_execution_tree<'s>(
    http_context: &HttpContext,
    execution_tree: QueryExecutionTree,
    field_span_attribute: &str,
    execution_span_attribute: &'static str,
    process_response_as: &ProcessResponseAs,
    project_id: Option<&ProjectId>,
    child_filter_expressions: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<Vec<ndc_models::RowSet>, FieldError> {
    // given an execution tree
    // 1) run remote predicates
    // 2) update predicates in `QueryExecutionPlan` with results of remote predicates
    // 3) run `QueryExecutionPlan` query
    // 4) run any remote joins

    // resolve all our filter expressions
    let mut filter_expressions = execute_remote_predicates(
        &execution_tree.remote_predicates,
        http_context,
        field_span_attribute,
        execution_span_attribute,
        process_response_as,
        project_id,
    )
    .await?;

    // add them to any that have been passed from child remote predicates
    filter_expressions.extend(child_filter_expressions.clone());

    // traverse `QueryExecutionPlan`, adding results of remote predicates
    let query_execution_plan_with_predicates = replace_predicates_in_query_execution_plan(
        execution_tree.query_execution_plan,
        &filter_expressions,
    )?;

    // create our `main` NDC request
    let response_rowsets = execute_ndc_query(
        http_context,
        query_execution_plan_with_predicates,
        field_span_attribute,
        execution_span_attribute,
        project_id,
    )
    .await?;

    // run any remote joins for the main request, combining
    // the results with the original rowsets
    run_remote_joins(
        http_context,
        execution_tree.remote_join_executions,
        execution_span_attribute,
        process_response_as,
        project_id,
        response_rowsets,
    )
    .await
}

// construct an NDC query request and execute it
async fn execute_ndc_query(
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
async fn run_remote_joins(
    http_context: &HttpContext,
    remote_join_executions: JoinLocations,
    execution_span_attribute: &'static str,
    process_response_as: &ProcessResponseAs,
    project_id: Option<&ProjectId>,
    mut response_rowsets: Vec<ndc_models::RowSet>,
) -> Result<Vec<ndc_models::RowSet>, FieldError> {
    // TODO: Failures in remote joins should result in partial response
    // https://github.com/hasura/v3-engine/issues/229
    remote_joins::execute_join_locations(
        http_context,
        execution_span_attribute,
        &mut response_rowsets,
        process_response_as,
        &remote_join_executions,
        project_id,
    )
    .await?;

    // `response_rowsets` has been mutated by `execute_join_locations` to
    // include the new data
    Ok(response_rowsets)
}

pub async fn resolve_ndc_mutation_execution(
    http_context: &HttpContext,
    ndc_mutation_execution: NDCMutationExecution,
    project_id: Option<&ProjectId>,
) -> Result<ndc_models::MutationResponse, FieldError> {
    let NDCMutationExecution {
        execution_tree,
        data_connector,
        execution_span_attribute,
        field_span_attribute,
        process_response_as,
    } = ndc_mutation_execution;

    let mutation_request =
        ndc_request::make_ndc_mutation_request(execution_tree.mutation_execution_plan)?;

    let mutation_response = ndc::execute_ndc_mutation(
        http_context,
        &mutation_request,
        &data_connector,
        execution_span_attribute,
        field_span_attribute,
        project_id,
    )
    .await?
    .as_latest();

    let mutation_response_as_query_response =
        mutation_response_to_query_response(mutation_response);
    let response_rowsets = run_remote_joins(
        http_context,
        execution_tree.remote_join_executions,
        execution_span_attribute,
        &process_response_as,
        project_id,
        mutation_response_as_query_response.0,
    )
    .await?;
    let mutation_response =
        query_response_back_to_mutation_response(ndc_models::QueryResponse(response_rowsets))?;

    Ok(mutation_response)
}

/// Kludge-ily make a `MutationResponse` look like a `QueryResponse` so we can pass it to the
/// remote join execution code. `query_response_back_to_mutation_response` does the
/// inverse operation
/// TODO refactor (ENG-1587)
fn mutation_response_to_query_response(
    mutation_response: ndc_models::MutationResponse,
) -> ndc_models::QueryResponse {
    let row_sets: Vec<ndc_models::RowSet> = mutation_response
        .operation_results
        .into_iter()
        .map(
            |ndc_models::MutationOperationResults::Procedure { result }| {
                // make this the same shape as the result of a Function
                let rows = Some(vec![IndexMap::from_iter([(
                    ndc_models::FieldName::from(FUNCTION_IR_VALUE_COLUMN_NAME),
                    ndc_models::RowFieldValue(result),
                )])]);
                // TODO it looks possible to refactor `run_remote_joins` so it takes a mutable
                // reference to `rows`, so we could avoid this. Pass a mutable slice to ensure the
                // length doesn't change?
                ndc_models::RowSet {
                    aggregates: None,
                    rows,
                    groups: None,
                }
            },
        )
        .collect::<Vec<_>>();

    ndc_models::QueryResponse(row_sets)
}
fn query_response_back_to_mutation_response(
    query_response: ndc_models::QueryResponse,
) -> Result<ndc_models::MutationResponse, FieldError> {
    let operation_results = query_response
        .0
        .into_iter()
        .map(|row_set| {
            match row_set
                .rows
                .as_ref()
                .ok_or_else(|| {
                    FieldError::InternalError(FieldInternalError::InternalGeneric {
                        description: "Unexpected structure: rows were None".to_string(),
                    })
                })?
                .as_slice()
            {
                [singleton_map]
                    if singleton_map.len() == 1
                        && singleton_map.contains_key(&ndc_models::FieldName::from(
                            FUNCTION_IR_VALUE_COLUMN_NAME,
                        )) =>
                {
                    let result = singleton_map
                        [&ndc_models::FieldName::from(FUNCTION_IR_VALUE_COLUMN_NAME)]
                        .0
                        .clone();
                    Ok(ndc_models::MutationOperationResults::Procedure { result })
                }
                _ => Err(FieldError::InternalError(
                    FieldInternalError::InternalGeneric {
                        description: "Unexpected structure: non-singleton rowset".to_string(),
                    },
                )),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ndc_models::MutationResponse { operation_results })
}

/// A subscription NDC query.
/// Contains required information to execute a NDC query for a subscription in a polling loop.
pub struct NDCSubscriptionQuery {
    pub query_request: ndc::NdcQueryRequest,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
    pub process_response_as: ProcessResponseAs,
    pub polling_interval_ms: u64,
}

/// Resolve a subscription execution plan to a NDC query.
pub async fn resolve_ndc_subscription_execution(
    execution: NDCSubscriptionExecution,
) -> Result<NDCSubscriptionQuery, FieldError> {
    let NDCSubscriptionExecution {
        query_execution_plan,
        execution_span_attribute: _,
        field_span_attribute: _,
        process_response_as,
        polling_interval_ms,
    } = execution;
    // Remote relationships and relationships without NDC comparison capability are not allowed in predicates for subscriptions.
    // Only allow local relationships and fields that can be pushed down to NDC.
    let data_connector = query_execution_plan.data_connector.clone();
    let query_request = make_ndc_query_request(query_execution_plan)?;
    Ok(NDCSubscriptionQuery {
        query_request,
        data_connector,
        process_response_as,
        polling_interval_ms,
    })
}
