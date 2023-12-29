use crate::execute::query_plan::{NodeQueryPlan, ProcessResponseAs};
use crate::execute::remote_joins::types::{JoinId, JoinLocations, RemoteJoin};
use crate::schema::GDS;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::{http::RawRequest, schema::Schema};
use nonempty::NonEmpty;

use crate::execute::{error, query_plan};

use super::ExecuteOrExplainResponse;
pub mod types;

pub async fn execute_explain(
    http_client: &reqwest::Client,
    schema: &Schema<GDS>,
    session: &Session,
    request: RawRequest,
) -> types::ExplainResponse {
    execute_explain_internal(http_client, schema, session, request)
        .await
        .unwrap_or_else(|e| types::ExplainResponse::error(e.to_graphql_error(None)))
}

/// Explains a GraphQL query
pub async fn execute_explain_internal(
    http_client: &reqwest::Client,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    raw_request: gql::http::RawRequest,
) -> Result<types::ExplainResponse, error::Error> {
    let query_response = super::execute_request_internal(
        http_client,
        schema,
        session,
        raw_request,
        types::RequestMode::Explain,
    )
    .await?;
    match query_response {
        ExecuteOrExplainResponse::Explain(response) => Ok(response),
        ExecuteOrExplainResponse::Execute(_response) => Err(error::Error::InternalError(
            error::InternalError::Developer(
                error::InternalDeveloperError::ExplainReturnedExecuteResponse,
            ),
        )),
    }
}

pub fn explain_query_plan(
    query_plan: query_plan::QueryPlan<'_, '_, '_>,
) -> Result<types::Step, error::Error> {
    let mut parallel_root_steps = vec![];
    // Here, we are assuming that all root fields are executed in parallel.
    for (alias, node) in query_plan {
        match node {
            NodeQueryPlan::NDCQueryExecution(ndc_query_execution) => {
                let sequence_steps = get_execution_steps(
                    alias,
                    &ndc_query_execution.process_response_as,
                    ndc_query_execution.execution_tree.remote_executions,
                    ndc_query_execution.execution_tree.root_node.query,
                );
                parallel_root_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
            }
            NodeQueryPlan::RelayNodeSelect(Some(ndc_query_execution)) => {
                let sequence_steps = get_execution_steps(
                    alias,
                    &ndc_query_execution.process_response_as,
                    ndc_query_execution.execution_tree.remote_executions,
                    ndc_query_execution.execution_tree.root_node.query,
                );
                parallel_root_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
            }
            NodeQueryPlan::TypeName { .. } => {
                return Err(error::Error::ExplainError(
                    "cannot explain introspection queries".to_string(),
                ));
            }
            NodeQueryPlan::SchemaField { .. } => {
                return Err(error::Error::ExplainError(
                    "cannot explain introspection queries".to_string(),
                ));
            }
            NodeQueryPlan::TypeField { .. } => {
                return Err(error::Error::ExplainError(
                    "cannot explain introspection queries".to_string(),
                ));
            }
            NodeQueryPlan::RelayNodeSelect(None) => {
                return Err(error::Error::ExplainError(
                    "cannot explain relay queries with no execution plan".to_string(),
                ));
            }
            NodeQueryPlan::NDCMutationExecution(_) => {
                return Err(error::Error::ExplainError(
                    "cannot explain mutations".to_string(),
                ));
            }
        }
    }
    // simplify the steps
    match NonEmpty::from_vec(parallel_root_steps) {
        Some(parallel_root_steps) => {
            let simplified_step =
                simplify_step(Box::new(types::Step::Parallel(parallel_root_steps)));
            Ok(*simplified_step)
        }
        None => Err(error::Error::ExplainError(
            "cannot explain query as there are no explainable root field".to_string(),
        )),
    }
}

fn get_execution_steps<'s>(
    alias: gql::ast::common::Alias,
    process_response_as: &ProcessResponseAs<'s>,
    join_locations: JoinLocations<(RemoteJoin<'s>, JoinId)>,
    ndc_query_request: ndc_client::models::QueryRequest,
) -> NonEmpty<Box<types::Step>> {
    let mut sequence_steps = match process_response_as {
        ProcessResponseAs::CommandResponse { .. } => {
            // A command execution node
            NonEmpty::new(Box::new(types::Step::CommandSelect(
                types::CommandSelectIR {
                    command_name: alias.to_string(),
                    query_request: ndc_query_request,
                },
            )))
        }
        ProcessResponseAs::Array { .. } | ProcessResponseAs::Object { .. } => {
            // A model execution node
            NonEmpty::new(Box::new(types::Step::ModelSelect(types::ModelSelectIR {
                model_name: alias.to_string(),
                query_request: ndc_query_request,
            })))
        }
    };
    if let Some(join_steps) = get_join_steps(alias.to_string(), join_locations) {
        sequence_steps.push(Box::new(types::Step::Parallel(join_steps)));
        sequence_steps.push(Box::new(types::Step::HashJoin));
    };
    sequence_steps
}

fn get_join_steps(
    _root_field_name: String,
    join_locations: JoinLocations<(RemoteJoin<'_>, JoinId)>,
) -> Option<NonEmpty<Box<types::Step>>> {
    let mut parallel_join_steps = vec![];
    for (alias, location) in join_locations.locations {
        let mut sequence_steps = vec![];
        if let Some((remote_join, _join_id)) = location.join_node {
            // We only support remote joins to Model for now
            // TODO (paritosh): Fix this when we support model to command relationships
            let mut query_request = remote_join.target_ndc_ir;
            query_request.variables = Some(vec![]);
            sequence_steps.push(Box::new(types::Step::ForEach(
                types::ForEachStep::ModelSelect(types::ModelSelectIR {
                    model_name: alias.clone(),
                    query_request,
                }),
            )))
        };
        if let Some(rest_join_steps) = get_join_steps(alias, location.rest) {
            sequence_steps.push(Box::new(types::Step::Parallel(rest_join_steps)));
            sequence_steps.push(Box::new(types::Step::HashJoin));
        };
        if let Some(sequence_steps) = NonEmpty::from_vec(sequence_steps) {
            parallel_join_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
        };
    }
    NonEmpty::from_vec(parallel_join_steps)
}

fn simplify_steps(steps: NonEmpty<Box<types::Step>>) -> NonEmpty<Box<types::Step>> {
    steps.map(simplify_step)
}

fn simplify_step(step: Box<types::Step>) -> Box<types::Step> {
    match *step {
        types::Step::Parallel(steps) => {
            let simplified_steps = simplify_steps(steps);
            if simplified_steps.len() == 1 {
                simplified_steps.head
            } else {
                Box::new(types::Step::Parallel(simplified_steps))
            }
        }
        types::Step::Sequence(steps) => {
            let simplified_steps = simplify_steps(steps);
            if simplified_steps.len() == 1 {
                simplified_steps.head
            } else {
                Box::new(types::Step::Sequence(simplified_steps))
            }
        }
        types::Step::ModelSelect(_) => step,
        types::Step::CommandSelect(_) => step,
        types::Step::HashJoin => step,
        types::Step::ForEach(_) => step,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simplify_steps() {
        let step = types::Step::HashJoin;

        let simplified_steps = simplify_step(Box::new(types::Step::Parallel(nonempty::nonempty![
            Box::new(step.clone())
        ])));
        assert_eq!(*simplified_steps, step.clone());

        let simplified_steps = simplify_step(Box::new(types::Step::Sequence(nonempty::nonempty![
            Box::new(step.clone())
        ])));
        assert_eq!(*simplified_steps, step.clone());

        let nested_step = types::Step::Parallel(nonempty::nonempty![Box::new(
            types::Step::Sequence(nonempty::nonempty![Box::new(types::Step::Parallel(
                nonempty::nonempty![Box::new(types::Step::Sequence(nonempty::nonempty![
                    Box::new(step.clone())
                ]))]
            ))])
        )]);
        let simplified_steps = simplify_step(Box::new(nested_step));
        assert_eq!(*simplified_steps, step.clone());
    }
}
