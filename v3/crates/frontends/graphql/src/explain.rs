mod predicate;
pub mod types;
use super::steps;

use std::borrow::Cow;

use async_recursion::async_recursion;
use execute::ndc::client as ndc_client;
use execute::plan::{
    self, ApolloFederationSelect, NDCQueryExecution, NodeQueryPlan, ProcessResponseAs,
    ResolveFilterExpressionContext,
};
use execute::HttpContext;
use execute::{JoinLocations, JoinNode, RemoteJoinType};
use graphql_schema::GDS;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use lang_graphql::{http::RawRequest, schema::Schema};
use nonempty::NonEmpty;
use tracing_util::{AttributeVisibility, SpanVisibility};

pub async fn execute_explain(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    request: RawRequest,
) -> (Option<ast::OperationType>, types::ExplainResponse) {
    explain_query_internal(
        expose_internal_errors,
        http_context,
        schema,
        session,
        request_headers,
        request,
    )
    .await
    .map_or_else(
        |e| {
            (
                None,
                types::ExplainResponse::error(e.to_graphql_error(expose_internal_errors)),
            )
        },
        |(operation_type, response)| (Some(operation_type), response),
    )
}

/// Explains (query plan) a GraphQL query
async fn explain_query_internal(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    raw_request: gql::http::RawRequest,
) -> Result<(ast::OperationType, types::ExplainResponse), execute::RequestError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_query",
            "Execute explain request",
            SpanVisibility::User,
            || {
                tracing_util::set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "session.role",
                    session.role.to_string(),
                );
                tracing_util::set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "request.graphql_query",
                    raw_request.query.to_string(),
                );
                Box::pin(async {
                    // parse the raw request into a GQL query
                    let query = steps::parse_query(&raw_request.query)?;

                    // normalize the parsed GQL query
                    let normalized_request =
                        steps::normalize_request(schema, session, query, &raw_request)?;

                    // generate IR
                    let ir =
                        steps::build_ir(schema, session, request_headers, &normalized_request)?;

                    // construct a plan to execute the request
                    let request_plan = steps::build_request_plan(&ir)?;

                    // explain the query plan
                    let response = tracer
                        .in_span_async(
                            "explain",
                            "Explain request plan",
                            SpanVisibility::Internal,
                            || {
                                Box::pin(async {
                                    let request_result = match request_plan {
                                        plan::RequestPlan::MutationPlan(mutation_plan) => {
                                            explain_mutation_plan(
                                                expose_internal_errors,
                                                http_context,
                                                mutation_plan,
                                            )
                                            .await
                                        }
                                        plan::RequestPlan::QueryPlan(query_plan) => {
                                            explain_query_plan(
                                                expose_internal_errors,
                                                http_context,
                                                query_plan,
                                            )
                                            .await
                                        }
                                        plan::RequestPlan::SubscriptionPlan(
                                            _alias,
                                            _subscription_plan,
                                        ) => {
                                            // subscriptions are not supported over HTTP
                                            Err(execute::RequestError::ExplainError(
                                                "Subscriptions are not supported in explain API"
                                                    .to_string(),
                                            ))
                                        }
                                    };
                                    // convert the query plan to explain step
                                    match request_result {
                                        Ok(step) => step.make_explain_response(),
                                        Err(e) => types::ExplainResponse::error(
                                            e.to_graphql_error(expose_internal_errors),
                                        ),
                                    }
                                })
                            },
                        )
                        .await;
                    Ok((normalized_request.ty, response))
                })
            },
        )
        .await
}

/// Produce an /explain plan for a given GraphQL query.
pub(crate) async fn explain_query_plan(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    query_plan: plan::QueryPlan<'_, '_, '_>,
) -> Result<types::Step, execute::RequestError> {
    let mut parallel_root_steps = vec![];
    // Allow resolving in-engine relationship predicates
    let resolve_context =
        ResolveFilterExpressionContext::new_allow_in_engine_resolution(http_context);
    // Here, we are assuming that all root fields are executed in parallel.
    for (alias, node) in query_plan {
        match node {
            NodeQueryPlan::NDCQueryExecution {
                query_execution: ndc_query_execution,
                ..
            }
            | NodeQueryPlan::RelayNodeSelect(Some((ndc_query_execution, _))) => {
                let NDCQueryExecution {
                    execution_tree,
                    process_response_as,
                    ..
                } = ndc_query_execution;

                let mut predicate_explain_steps = vec![];
                predicate::explain_query_predicate_node(
                    &expose_internal_errors,
                    http_context,
                    &execution_tree.query_execution_plan.query_node,
                    &mut predicate_explain_steps,
                )
                .await?;

                let resolved_execution_plan = execution_tree
                    .query_execution_plan
                    .resolve(&resolve_context)
                    .await
                    .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

                let data_connector = resolved_execution_plan.data_connector.clone();
                let ndc_request =
                    plan::ndc_request::make_ndc_query_request(resolved_execution_plan)
                        .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

                let sequence_steps = get_execution_steps(
                    expose_internal_errors,
                    http_context,
                    &resolve_context,
                    alias,
                    &process_response_as,
                    execution_tree.remote_join_executions,
                    types::NDCRequest::Query(ndc_request),
                    &data_connector,
                )
                .await?;
                parallel_root_steps.push(Box::new(types::Step::Sequence(prepend_vec_to_nonempty(
                    predicate_explain_steps.into_iter().map(Box::new).collect(),
                    sequence_steps,
                ))));
            }
            NodeQueryPlan::ApolloFederationSelect(ApolloFederationSelect::EntitiesSelect(
                parallel_ndc_query_executions,
            )) => {
                let mut parallel_steps = Vec::new();
                for (ndc_query_execution, _) in parallel_ndc_query_executions {
                    let NDCQueryExecution {
                        execution_tree,
                        process_response_as,
                        ..
                    } = ndc_query_execution;
                    let mut predicate_explain_steps = vec![];
                    predicate::explain_query_predicate_node(
                        &expose_internal_errors,
                        http_context,
                        &execution_tree.query_execution_plan.query_node,
                        &mut predicate_explain_steps,
                    )
                    .await?;

                    let resolved_execution_plan = execution_tree
                        .query_execution_plan
                        .resolve(&resolve_context)
                        .await
                        .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

                    let data_connector = resolved_execution_plan.data_connector.clone();
                    let ndc_request =
                        plan::ndc_request::make_ndc_query_request(resolved_execution_plan)
                            .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

                    let sequence_steps = get_execution_steps(
                        expose_internal_errors,
                        http_context,
                        &resolve_context,
                        alias.clone(),
                        &process_response_as,
                        execution_tree.remote_join_executions,
                        types::NDCRequest::Query(ndc_request),
                        &data_connector,
                    )
                    .await?;
                    parallel_steps.push(Box::new(types::Step::Sequence(prepend_vec_to_nonempty(
                        predicate_explain_steps.into_iter().map(Box::new).collect(),
                        sequence_steps,
                    ))));
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
                return Err(execute::RequestError::ExplainError(
                    "cannot explain introspection queries".to_string(),
                ));
            }
            NodeQueryPlan::RelayNodeSelect(None) => {
                return Err(execute::RequestError::ExplainError(
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
        None => Err(execute::RequestError::ExplainError(
            "cannot explain query as there are no explainable root field".to_string(),
        )),
    }
}

/// Produce an /explain plan for a given GraphQL mutation.
pub(crate) async fn explain_mutation_plan(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    mutation_plan: plan::MutationPlan<'_, '_>,
) -> Result<types::Step, execute::RequestError> {
    let mut root_steps = vec![];

    if !mutation_plan.type_names.is_empty() {
        return Err(execute::RequestError::ExplainError(
            "cannot explain introspection queries".to_string(),
        ));
    }

    for (_, mutation_group) in mutation_plan.nodes {
        for (alias, ndc_mutation_execution) in mutation_group {
            let mut predicate_explain_steps = vec![];
            predicate::explain_query_predicate_nested_field(
                &expose_internal_errors,
                http_context,
                ndc_mutation_execution
                    .execution_node
                    .procedure_fields
                    .as_ref(),
                &mut predicate_explain_steps,
            )
            .await?;

            let resolve_context =
                ResolveFilterExpressionContext::new_allow_in_engine_resolution(http_context);
            let resolved_execution_plan = ndc_mutation_execution
                .execution_node
                .resolve(&resolve_context)
                .await
                .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

            let mutation_request =
                plan::ndc_request::make_ndc_mutation_request(resolved_execution_plan)
                    .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

            let sequence_steps = get_execution_steps(
                expose_internal_errors,
                http_context,
                &resolve_context,
                alias,
                &ndc_mutation_execution.process_response_as,
                ndc_mutation_execution.join_locations,
                types::NDCRequest::Mutation(mutation_request),
                &ndc_mutation_execution.data_connector,
            )
            .await?;
            let field_steps = prepend_vec_to_nonempty(
                predicate_explain_steps.into_iter().map(Box::new).collect(),
                sequence_steps,
            );
            root_steps.push(Box::new(types::Step::Sequence(field_steps)));
        }
    }

    // simplify the steps
    match NonEmpty::from_vec(root_steps) {
        Some(root_steps) => {
            let simplified_step = simplify_step(Box::new(types::Step::Sequence(root_steps)));
            Ok(*simplified_step)
        }
        None => Err(execute::RequestError::ExplainError(
            "cannot explain mutation as there are no explainable root fields".to_string(),
        )),
    }
}

async fn get_execution_steps<'s>(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    resolve_context: &ResolveFilterExpressionContext<'_>,
    alias: gql::ast::common::Alias,
    process_response_as: &ProcessResponseAs,
    join_locations: JoinLocations<'s>,
    ndc_request: types::NDCRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
) -> Result<NonEmpty<Box<types::Step>>, execute::RequestError> {
    let mut sequence_steps = match process_response_as {
        ProcessResponseAs::CommandResponse { .. } => {
            // A command execution node
            let data_connector_explain = fetch_explain_from_data_connector(
                expose_internal_errors,
                http_context,
                &ndc_request,
                data_connector,
            )
            .await;
            NonEmpty::new(Box::new(types::Step::CommandSelect(
                types::CommandSelectIR {
                    command_name: alias.to_string(),
                    ndc_request,
                    ndc_explain: data_connector_explain,
                },
            )))
        }
        ProcessResponseAs::Array { .. }
        | ProcessResponseAs::Object { .. }
        | ProcessResponseAs::Aggregates { .. } => {
            // A model execution node
            let data_connector_explain = fetch_explain_from_data_connector(
                expose_internal_errors,
                http_context,
                &ndc_request,
                data_connector,
            )
            .await;
            NonEmpty::new(Box::new(types::Step::ModelSelect(types::ModelSelectIR {
                model_name: alias.to_string(),
                ndc_request,
                ndc_explain: data_connector_explain,
            })))
        }
    };
    if let Some(join_steps) = get_join_steps(
        expose_internal_errors,
        join_locations,
        http_context,
        resolve_context,
    )
    .await?
    {
        sequence_steps.push(Box::new(types::Step::Sequence(join_steps)));
        sequence_steps.push(Box::new(types::Step::HashJoin));
    };
    Ok(sequence_steps)
}

/// Get the join steps for a given join location. This should be used to get the join steps for a remote relationship.
/// It also supports nested remote relationships.
///
/// TODO: Currently the steps are sequential, we should make them parallel once the executor supports it.
#[async_recursion]
async fn get_join_steps(
    expose_internal_errors: execute::ExposeInternalErrors,
    join_locations: JoinLocations<'async_recursion>,
    http_context: &HttpContext,
    resolve_context: &ResolveFilterExpressionContext,
) -> Result<Option<NonEmpty<Box<types::Step>>>, execute::RequestError> {
    let mut sequence_join_steps = vec![];
    for (alias, location) in join_locations.locations {
        let mut sequence_steps = vec![];
        if let JoinNode::Remote(remote_join) = location.join_node {
            let mut resolved_execution_plan = remote_join
                .target_ndc_execution
                .resolve(resolve_context)
                .await
                .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

            resolved_execution_plan.variables = Some(vec![]);
            let target_data_connector = resolved_execution_plan.data_connector.clone();
            let query_request = plan::ndc_request::make_ndc_query_request(resolved_execution_plan)
                .map_err(|e| execute::RequestError::ExplainError(e.to_string()))?;

            let ndc_request = types::NDCRequest::Query(query_request);

            let data_connector_explain = fetch_explain_from_data_connector(
                expose_internal_errors,
                http_context,
                &ndc_request,
                &target_data_connector,
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
            )));
        };
        if let Some(rest_join_steps) = get_join_steps(
            expose_internal_errors,
            location.rest,
            http_context,
            resolve_context,
        )
        .await?
        {
            sequence_steps.push(Box::new(types::Step::Sequence(rest_join_steps)));
            sequence_steps.push(Box::new(types::Step::HashJoin));
        };
        if let Some(sequence_steps) = NonEmpty::from_vec(sequence_steps) {
            sequence_join_steps.push(Box::new(types::Step::Sequence(sequence_steps)));
        };
    }
    Ok(NonEmpty::from_vec(sequence_join_steps))
}

fn simplify_steps(steps: NonEmpty<Box<types::Step>>) -> NonEmpty<Box<types::Step>> {
    steps.map(simplify_step)
}

#[allow(clippy::unnecessary_box_returns)] // helper for the above function
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
        types::Step::ModelSelect(_)
        | types::Step::CommandSelect(_)
        | types::Step::HashJoin
        | types::Step::ForEach(_) => step,
    }
}

pub(crate) async fn fetch_explain_from_data_connector(
    expose_internal_errors: execute::ExposeInternalErrors,
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
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers: Cow::Borrowed(&data_connector.headers.0),
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    match ndc_request {
                        types::NDCRequest::Query(query_request) => {
                            if data_connector.capabilities.supports_explaining_queries {
                                ndc_client::explain_query_post(ndc_config, query_request)
                                    .await
                                    .map(Some)
                                    .map_err(execute::FieldError::from)
                            } else {
                                Ok(None)
                            }
                        }
                        types::NDCRequest::Mutation(mutation_request) => {
                            if data_connector.capabilities.supports_explaining_mutations {
                                ndc_client::explain_mutation_post(ndc_config, mutation_request)
                                    .await
                                    .map(Some)
                                    .map_err(execute::FieldError::from)
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
        Err(e) => types::NDCExplainResponse::error(&e, expose_internal_errors),
    }
}

pub(crate) fn prepend_vec_to_nonempty<T>(vec: Vec<T>, nonempty: NonEmpty<T>) -> NonEmpty<T> {
    match NonEmpty::from_vec(vec) {
        None => nonempty,
        Some(mut vec_nonempty) => {
            vec_nonempty.extend(nonempty);
            vec_nonempty
        }
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

    #[test]
    fn test_prepend_vec_to_nonempty() {
        let vec = vec![1, 2, 3];
        let nonempty_list = nonempty::nonempty![4, 5, 6];
        assert_eq!(
            nonempty::nonempty![1, 2, 3, 4, 5, 6],
            prepend_vec_to_nonempty(vec, nonempty_list)
        );
    }
}
