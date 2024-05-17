use super::remote_joins::types::{JoinNode, RemoteJoinType};
use super::HttpContext;
use crate::ndc::client as ndc_client;
use crate::plan::{ApolloFederationSelect, NodeQueryPlan, ProcessResponseAs};
use crate::remote_joins::types::{JoinId, JoinLocations, RemoteJoin};
use crate::{error, plan};
use async_recursion::async_recursion;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::{http::RawRequest, schema::Schema};
use nonempty::NonEmpty;
use schema::GDS;
use tracing_util::SpanVisibility;
pub mod types;
use lang_graphql::ast::common as ast;

pub async fn execute_explain(
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    session: &Session,
    request: RawRequest,
) -> types::ExplainResponse {
    super::explain_query_internal(http_context, schema, session, request)
        .await
        .unwrap_or_else(|e| types::ExplainResponse::error(e.to_graphql_error()))
}

/// Produce an /explain plan for a given GraphQL query.
pub(crate) async fn explain_query_plan(
    http_context: &HttpContext,
    query_plan: plan::QueryPlan<'_, '_, '_>,
) -> Result<types::Step, error::RequestError> {
    let mut parallel_root_steps = vec![];
    // Here, we are assuming that all root fields are executed in parallel.
    for (alias, node) in query_plan {
        match node {
            NodeQueryPlan::NDCQueryExecution(ndc_query_execution) => {
                let sequence_steps = get_execution_steps(
                    http_context,
                    alias,
                    &ndc_query_execution.process_response_as,
                    ndc_query_execution.execution_tree.remote_executions,
                    types::NDCRequest::Query(ndc_query_execution.execution_tree.root_node.query),
                    ndc_query_execution.execution_tree.root_node.data_connector,
                )
                .await;
                parallel_root_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
            }
            NodeQueryPlan::RelayNodeSelect(Some(ndc_query_execution)) => {
                let sequence_steps = get_execution_steps(
                    http_context,
                    alias,
                    &ndc_query_execution.process_response_as,
                    ndc_query_execution.execution_tree.remote_executions,
                    types::NDCRequest::Query(ndc_query_execution.execution_tree.root_node.query),
                    ndc_query_execution.execution_tree.root_node.data_connector,
                )
                .await;
                parallel_root_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
            }
            NodeQueryPlan::ApolloFederationSelect(ApolloFederationSelect::EntitiesSelect(
                parallel_ndc_query_executions,
            )) => {
                let mut parallel_steps = Vec::new();
                for ndc_query_execution in parallel_ndc_query_executions {
                    let sequence_steps = get_execution_steps(
                        http_context,
                        alias.clone(),
                        &ndc_query_execution.process_response_as,
                        ndc_query_execution.execution_tree.remote_executions,
                        types::NDCRequest::Query(
                            ndc_query_execution.execution_tree.root_node.query,
                        ),
                        ndc_query_execution.execution_tree.root_node.data_connector,
                    )
                    .await;
                    parallel_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
                }
                match NonEmpty::from_vec(parallel_steps) {
                    None => {}
                    Some(parallel_steps) => {
                        parallel_root_steps.push(Box::new(types::Step::Parallel(parallel_steps)));
                    }
                }
            }
            NodeQueryPlan::TypeName { .. }
            | NodeQueryPlan::SchemaField { .. }
            | NodeQueryPlan::TypeField { .. }
            | NodeQueryPlan::ApolloFederationSelect(ApolloFederationSelect::ServiceField {
                ..
            }) => {
                return Err(error::RequestError::ExplainError(
                    "cannot explain introspection queries".to_string(),
                ));
            }
            NodeQueryPlan::RelayNodeSelect(None) => {
                return Err(error::RequestError::ExplainError(
                    "cannot explain relay queries with no execution plan".to_string(),
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
        None => Err(error::RequestError::ExplainError(
            "cannot explain query as there are no explainable root field".to_string(),
        )),
    }
}

/// Produce an /explain plan for a given GraphQL mutation.
pub(crate) async fn explain_mutation_plan(
    http_context: &HttpContext,
    mutation_plan: plan::MutationPlan<'_, '_, '_>,
) -> Result<types::Step, error::RequestError> {
    let mut root_steps = vec![];

    if !mutation_plan.type_names.is_empty() {
        return Err(error::RequestError::ExplainError(
            "cannot explain introspection queries".to_string(),
        ));
    }

    for (_, mutation_group) in mutation_plan.nodes {
        for (alias, ndc_mutation_execution) in mutation_group {
            let sequence_steps = get_execution_steps(
                http_context,
                alias,
                &ndc_mutation_execution.process_response_as,
                ndc_mutation_execution.join_locations,
                types::NDCRequest::Mutation(ndc_mutation_execution.query),
                ndc_mutation_execution.data_connector,
            )
            .await;
            root_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
        }
    }

    // simplify the steps
    match NonEmpty::from_vec(root_steps) {
        Some(root_steps) => {
            let simplified_step = simplify_step(Box::new(types::Step::Sequence(root_steps)));
            Ok(*simplified_step)
        }
        None => Err(error::RequestError::ExplainError(
            "cannot explain mutation as there are no explainable root fields".to_string(),
        )),
    }
}

async fn get_execution_steps<'s>(
    http_context: &HttpContext,
    alias: gql::ast::common::Alias,
    process_response_as: &ProcessResponseAs<'s>,
    join_locations: JoinLocations<(RemoteJoin<'s, '_>, JoinId)>,
    ndc_request: types::NDCRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
) -> NonEmpty<Box<types::Step>> {
    let mut sequence_steps = match process_response_as {
        ProcessResponseAs::CommandResponse { .. } => {
            // A command execution node
            let data_connector_explain =
                fetch_explain_from_data_connector(http_context, &ndc_request, data_connector).await;
            NonEmpty::new(Box::new(types::Step::CommandSelect(
                types::CommandSelectIR {
                    command_name: alias.to_string(),
                    ndc_request,
                    ndc_explain: data_connector_explain,
                },
            )))
        }
        ProcessResponseAs::Array { .. } | ProcessResponseAs::Object { .. } => {
            // A model execution node
            let data_connector_explain =
                fetch_explain_from_data_connector(http_context, &ndc_request, data_connector).await;
            NonEmpty::new(Box::new(types::Step::ModelSelect(types::ModelSelectIR {
                model_name: alias.to_string(),
                ndc_request,
                ndc_explain: data_connector_explain,
            })))
        }
    };
    if let Some(join_steps) = get_join_steps(join_locations, http_context).await {
        sequence_steps.push(Box::new(types::Step::Sequence(join_steps)));
        sequence_steps.push(Box::new(types::Step::HashJoin));
    };
    sequence_steps
}

/// Get the join steps for a given join location. This should be used to get the join steps for a remote relationship.
/// It also supports nested remote relationships.
///
/// TODO: Currently the steps are sequential, we should make them parallel once the executor supports it.
#[async_recursion]
async fn get_join_steps(
    join_locations: JoinLocations<(RemoteJoin<'async_recursion, 'async_recursion>, JoinId)>,
    http_context: &HttpContext,
) -> Option<NonEmpty<Box<types::Step>>> {
    let mut sequence_join_steps = vec![];
    for (alias, location) in join_locations.locations {
        let mut sequence_steps = vec![];
        if let JoinNode::Remote((remote_join, _join_id)) = location.join_node {
            let mut query_request = remote_join.target_ndc_ir;
            query_request.variables = Some(vec![]);
            let ndc_request = types::NDCRequest::Query(query_request);
            let data_connector_explain = fetch_explain_from_data_connector(
                http_context,
                &ndc_request,
                remote_join.target_data_connector,
            )
            .await;
            sequence_steps.push(Box::new(types::Step::ForEach(
                // We don't support ndc_explain for for-each steps yet
                match remote_join.remote_join_type {
                    RemoteJoinType::ToModel => {
                        types::ForEachStep::ModelSelect(types::ModelSelectIR {
                            model_name: alias.clone(),
                            ndc_request,
                            ndc_explain: data_connector_explain,
                        })
                    }
                    RemoteJoinType::ToCommand => {
                        types::ForEachStep::CommandSelect(types::CommandSelectIR {
                            command_name: alias.clone(),
                            ndc_request,
                            ndc_explain: data_connector_explain,
                        })
                    }
                },
            )))
        };
        if let Some(rest_join_steps) = get_join_steps(location.rest, http_context).await {
            sequence_steps.push(Box::new(types::Step::Sequence(rest_join_steps)));
            sequence_steps.push(Box::new(types::Step::HashJoin));
        };
        if let Some(sequence_steps) = NonEmpty::from_vec(sequence_steps) {
            sequence_join_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
        };
    }
    NonEmpty::from_vec(sequence_join_steps)
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

async fn fetch_explain_from_data_connector(
    http_context: &HttpContext,
    ndc_request: &types::NDCRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
) -> types::NDCExplainResponse {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "fetch_explain_from_data_connector",
            format!("Execute explain on data connector {}", data_connector.name),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let ndc_config = ndc_client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Query),
                        user_agent: None,
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers: data_connector.headers.0.clone(),
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    // TODO: use capabilities from the data connector context
                    let capabilities = ndc_client::capabilities_get(&ndc_config).await?;
                    match ndc_request {
                        types::NDCRequest::Query(query_request) => {
                            if capabilities.capabilities.query.explain.is_some() {
                                ndc_client::explain_query_post(&ndc_config, query_request)
                                    .await
                                    .map(Some)
                                    .map_err(error::FieldError::from)
                            } else {
                                Ok(None)
                            }
                        }
                        types::NDCRequest::Mutation(mutation_request) => {
                            if capabilities.capabilities.mutation.explain.is_some() {
                                ndc_client::explain_mutation_post(&ndc_config, mutation_request)
                                    .await
                                    .map(Some)
                                    .map_err(error::FieldError::from)
                            } else {
                                Ok(None)
                            }
                        }
                    }
                })
            },
        )
        .await;
    match response {
        Ok(Some(response)) => types::NDCExplainResponse::success(response),
        Ok(None) => types::NDCExplainResponse::not_supported(),
        Err(e) => types::NDCExplainResponse::error(&e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simplify_steps() {
        let simplified_steps = simplify_step(Box::new(types::Step::Parallel(nonempty::nonempty![
            Box::new(types::Step::HashJoin)
        ])));
        assert_eq!(*simplified_steps, types::Step::HashJoin);

        let simplified_steps = simplify_step(Box::new(types::Step::Sequence(nonempty::nonempty![
            Box::new(types::Step::HashJoin)
        ])));
        assert_eq!(*simplified_steps, types::Step::HashJoin);

        let nested_step = types::Step::Parallel(nonempty::nonempty![Box::new(
            types::Step::Sequence(nonempty::nonempty![Box::new(types::Step::Parallel(
                nonempty::nonempty![Box::new(types::Step::Sequence(nonempty::nonempty![
                    Box::new(types::Step::HashJoin)
                ]))]
            ))])
        )]);
        let simplified_steps = simplify_step(Box::new(nested_step));
        assert_eq!(*simplified_steps, types::Step::HashJoin);
    }
}
