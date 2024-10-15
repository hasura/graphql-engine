use axum::http;
use blake2::{Blake2b, Digest};
use execute::{self, plan, ExposeInternalErrors};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use nonempty::NonEmpty;
use pre_parse_plugin::execute as pre_parse_plugin;
use pre_response_plugin::execute as pre_response_plugin;

use super::types::{ConnectionInitState, OperationId, ServerMessage};
use crate::poller;
use crate::websocket::types as ws;

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("graphql-ws protocol is not initialized")]
    NotInitialized,
    #[error("poller with operation_id {} already exists", operation_id.0)]
    PollerAlreadyExists { operation_id: OperationId },
    #[error("error in pre-parse plugin: {0}")]
    PreParsePlugin(#[from] pre_parse_plugin::Error),
}

impl tracing_util::TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::User
    }
}

/// Handles the subscription message from the client.
/// It either starts a new poller or sends a close message if the poller with given operation_id already exists.
pub async fn handle_subscribe(
    connection: ws::Connection,
    operation_id: OperationId,
    payload: lang_graphql::http::RawRequest,
) {
    let tracer = tracing_util::global_tracer();
    let result = tracer
        .in_span_async(
            "handle_subscribe",
            "Handling subscribe protocol message",
            tracing_util::SpanVisibility::User,
            || {
                // Set operation_id attribute
                tracing_util::set_attribute_on_active_span(
                    tracing_util::AttributeVisibility::Default,
                    "graphql.operation.id",
                    operation_id.0.clone(),
                );
                Box::pin(async {
                    match *connection.protocol_init_state.read().await {
                        ConnectionInitState::NotInitialized => Err(Error::NotInitialized),
                        ConnectionInitState::Initialized {
                            ref session,
                            ref headers,
                        } => {
                            if connection.poller_exists(&operation_id).await {
                                Err(Error::PollerAlreadyExists {
                                    operation_id: operation_id.clone(),
                                })
                            } else {
                                // Execute pre-parse plugins
                                let plugin_response = match NonEmpty::from_slice(
                                    &connection.context.plugin_configs.pre_parse_plugins,
                                ) {
                                    Some(pre_parse_plugins) => {
                                        pre_parse_plugin::execute_pre_parse_plugins(
                                            &pre_parse_plugins,
                                            &connection.context.http_context.client,
                                            session,
                                            headers,
                                            &payload,
                                        )
                                        .await
                                    }
                                    None => {
                                        Ok(pre_parse_plugin::PreExecutePluginResponse::Continue)
                                    }
                                }?;
                                match plugin_response {
                                    pre_parse_plugin::PreExecutePluginResponse::Continue => {
                                        // Start a new poller for the operation and insert it into the connection.
                                        let poller = tracing_util::get_active_span(|span| {
                                            start_poller(
                                                operation_id.clone(),
                                                session.clone(),
                                                headers.clone(),
                                                connection.clone(),
                                                payload,
                                                span.span_context().clone(),
                                            )
                                        });
                                        connection
                                            .insert_poller(operation_id.clone(), poller)
                                            .await;
                                    }
                                    pre_parse_plugin::PreExecutePluginResponse::Return(bytes) => {
                                        // Send the plugin response to the client
                                        connection
                                            .send(ws::Message::Raw(
                                                axum::extract::ws::Message::Binary(bytes),
                                            ))
                                            .await;
                                    }
                                    pre_parse_plugin::PreExecutePluginResponse::ReturnError {
                                        plugin_name,
                                        error,
                                    } => {
                                        // Send the plugin error response to the client
                                        let graphql_error = error.into_graphql_error(&plugin_name);
                                        connection
                                            .send(ws::Message::Protocol(ServerMessage::Error {
                                                id: operation_id.clone(),
                                                payload: NonEmpty::new(graphql_error),
                                            }))
                                            .await;
                                    }
                                }
                                Ok(())
                            }
                        }
                    }
                })
            },
        )
        .await;
    if let Err(err) = result {
        match err {
            Error::NotInitialized => {
                // If the connection is not initialized, send an unauthorized message.
                connection.send(ws::Message::unauthorized()).await;
            }
            Error::PollerAlreadyExists { operation_id } => {
                // If the poller for this operation already exists, send a close message.
                connection
                    .send(ws::Message::subscriber_already_exists(&operation_id))
                    .await;
            }
            Error::PreParsePlugin(e) => {
                // If the pre-parse plugin fails, send an error message.
                connection
                    .send(ws::Message::Protocol(ServerMessage::Error {
                        id: operation_id,
                        payload: NonEmpty::new(e.into_graphql_error()),
                    }))
                    .await;
            }
        }
    }
}

/// Starts a new poller to handle GraphQL operations (queries, mutations, subscriptions).
fn start_poller(
    operation_id: OperationId,
    session: Session,
    headers: http::HeaderMap,
    connection: ws::Connection,
    raw_request: lang_graphql::http::RawRequest,
    parent_span_context: tracing_util::SpanContext,
) -> poller::Poller {
    poller::Poller::new(|| {
        Box::pin(async move {
            // Executes the GraphQL request and handles any errors.
            execute_request(
                operation_id.clone(),
                session,
                headers,
                &connection,
                raw_request,
                parent_span_context,
            )
            .await;
        })
    })
}

/// Executes the GraphQL request, handling queries, mutations, and subscriptions.
async fn execute_request(
    operation_id: OperationId,
    session: Session,
    headers: http::HeaderMap,
    connection: &ws::Connection,
    raw_request: lang_graphql::http::RawRequest,
    parent_span_context: tracing_util::SpanContext,
) {
    let tracer = tracing_util::global_tracer();
    // Execute the GraphQL request and handle any errors.
    let result = tracer
        .new_trace_async_with_link(
            "websocket_execute_request",
            "Executing a GraphQL request over WebSocket",
            tracing_util::SpanVisibility::User,
            parent_span_context,
            || {
                // Set websocket_id and operation_id as attributes
                tracing_util::set_attribute_on_active_span(
                    tracing_util::AttributeVisibility::Default,
                    "websocket.id",
                    connection.id.to_string(),
                );
                tracing_util::set_attribute_on_active_span(
                    tracing_util::AttributeVisibility::Default,
                    "graphql.operation.id",
                    operation_id.0.clone(),
                );
                Box::pin(async {
                    execute_request_internal(
                        operation_id.clone(),
                        session,
                        headers,
                        connection,
                        raw_request,
                    )
                    .await
                })
            },
        )
        .await;
    match result {
        Ok(()) => {}
        Err(e) => {
            // If an error occurs, send an error message.
            send_request_error(
                e,
                connection.context.expose_internal_errors,
                operation_id,
                connection,
            )
            .await;
        }
    }
}

pub async fn send_request_error(
    error: execute::RequestError,
    expose_internal_errors: ExposeInternalErrors,
    operation_id: OperationId,
    connection: &ws::Connection,
) {
    let gql_error = error.to_graphql_error(expose_internal_errors);
    let message = ServerMessage::Error {
        id: operation_id,
        payload: NonEmpty::new(gql_error),
    };
    connection.send(ws::Message::Protocol(message)).await;
}

// Exported for testing purpose
pub async fn execute_request_internal(
    operation_id: OperationId,
    session: Session,
    headers: http::HeaderMap,
    connection: &ws::Connection,
    raw_request: lang_graphql::http::RawRequest,
) -> Result<(), execute::RequestError> {
    let schema = &connection.context.schema;
    let project_id = connection.context.project_id.as_ref();
    let http_context = &connection.context.http_context;
    let expose_internal_errors = connection.context.expose_internal_errors;

    // Parse the raw GraphQL request.
    let query = graphql_frontend::parse_query(&raw_request.query)?;
    // Normalize the parsed GraphQL query.
    let normalized_request =
        graphql_frontend::normalize_request(schema, &session, query, &raw_request)?;
    // Generate Intermediate Representation (IR) from the query.
    let ir = graphql_frontend::build_ir(schema, &session, &headers, &normalized_request)?;
    // Build a request plan based on the IR.
    let request_plan = graphql_frontend::build_request_plan(&ir)?;

    match request_plan {
        // Handle mutations.
        plan::RequestPlan::MutationPlan(mutation_plan) => {
            let execute_query_result =
                plan::execute_mutation_plan(http_context, mutation_plan, project_id).await;
            send_single_result_operation_response(
                operation_id,
                &raw_request,
                session,
                headers,
                execute_query_result,
                expose_internal_errors,
                connection,
            )
            .await;
        }
        // Handle queries.
        plan::RequestPlan::QueryPlan(query_plan) => {
            let execute_query_result =
                plan::execute_query_plan(http_context, query_plan, project_id).await;
            send_single_result_operation_response(
                operation_id,
                &raw_request,
                session,
                headers,
                execute_query_result,
                expose_internal_errors,
                connection,
            )
            .await;
        }
        // Handle subscriptions by starting a polling loop to repeatedly fetch data.
        plan::RequestPlan::SubscriptionPlan(alias, plan) => {
            match plan::resolve_ndc_subscription_execution(plan).await {
                Ok(ndc_subscription) => {
                    let query_request = ndc_subscription.query_request;
                    let data_connector = ndc_subscription.data_connector;
                    let selection_set = ndc_subscription.selection_set;
                    let process_response_as = ndc_subscription.process_response_as;
                    let is_nullable = process_response_as.is_nullable();
                    let polling_interval_duration =
                        tokio::time::Duration::from_millis(ndc_subscription.polling_interval_ms);

                    // Initialize a response hash to track changes in the response.
                    let mut response_hash = ResponseHash::new();

                    let tracer = tracing_util::global_tracer();
                    let this_span_context =
                        tracing_util::get_active_span(|span_ref| span_ref.span_context().clone());

                    // A loop to periodically wait for the polling interval, then fetch data from NDC.
                    loop {
                        let result: Result<_, execute::FieldError> = tracer
                            .new_trace_async_with_link(
                                "websocket_poll_subscription",
                                "Polling a subscription query",
                                tracing_util::SpanVisibility::User,
                                this_span_context.clone(),
                                || {
                                    // Set websocket_id and operation_id as attributes
                                    tracing_util::set_attribute_on_active_span(
                                        tracing_util::AttributeVisibility::Default,
                                        "websocket.id",
                                        connection.id.to_string(),
                                    );
                                    tracing_util::set_attribute_on_active_span(
                                        tracing_util::AttributeVisibility::Default,
                                        "graphql.operation.id",
                                        operation_id.0.clone(),
                                    );
                                    Box::pin(async {
                                        // Fetch response from the connector
                                        let response = execute::fetch_from_data_connector(
                                            http_context,
                                            &query_request,
                                            &data_connector,
                                            None,
                                        )
                                        .await?;
                                        // Process response
                                        let response_rowsets = response.as_latest_rowsets();
                                        let processed_response = execute::process_response(
                                            selection_set,
                                            response_rowsets,
                                            &process_response_as,
                                        );
                                        let root_fields = IndexMap::from([(
                                            alias.clone(),
                                            plan::RootFieldResult::from_processed_response(
                                                is_nullable,
                                                processed_response,
                                            ),
                                        )]);
                                        // Generate a single root field query response
                                        let query_result =
                                            execute::ExecuteQueryResult { root_fields };

                                        let graphql_response =
                                            graphql_frontend::GraphQLResponse::from_result(
                                                query_result,
                                                expose_internal_errors,
                                            )
                                            .inner();
                                        // Send the response
                                        let stop_subscription =
                                            send_subscription_operation_response(
                                                &mut response_hash,
                                                operation_id.clone(),
                                                &raw_request,
                                                &session,
                                                &headers,
                                                graphql_response,
                                                connection,
                                            )
                                            .await;
                                        Ok(stop_subscription)
                                    })
                                },
                            )
                            .await;

                        match result {
                            Ok(stop_subscription) => {
                                // Stop the subscription, if only errors sent in the current response
                                if stop_subscription {
                                    break;
                                }
                            }
                            Err(err) => {
                                // Send the exception as a GraphQL error and stop polling
                                let graphql_error =
                                    err.to_graphql_error(expose_internal_errors, None);
                                send_graphql_errors(
                                    operation_id,
                                    NonEmpty::new(graphql_error),
                                    connection,
                                )
                                .await;
                                break;
                            }
                        }
                        // Wait for the polling interval
                        tokio::time::sleep(polling_interval_duration).await;
                    }
                }
                // Send an error message if the subscription fails to resolve.
                Err(e) => {
                    let graphql_error = e.to_graphql_error(expose_internal_errors, None);
                    send_graphql_errors(operation_id, NonEmpty::new(graphql_error), connection)
                        .await;
                }
            }
        }
    }
    Ok(())
}

#[derive(PartialEq, Eq)]
struct ResponseHash(Option<[u8; 64]>);

impl ResponseHash {
    fn new() -> Self {
        Self(None)
    }

    fn matches(&mut self, response: &lang_graphql::http::Response) -> bool {
        let serialized = serde_json::to_vec(&response.data).unwrap_or_default();
        let mut hasher = Blake2b::new();
        hasher.update(&serialized);
        let new_hash = hasher.finalize().into();
        let matches = self
            .0
            .as_ref()
            .map_or(false, |old_hash| old_hash == &new_hash);
        self.0 = Some(new_hash);
        matches
    }
}

/// Sends a GraphQL response with no errors.
async fn send_graphql_ok(
    operation_id: OperationId,
    response: lang_graphql::http::Response,
    connection: &ws::Connection,
) {
    connection
        .send(ws::Message::Protocol(ServerMessage::Next {
            id: operation_id,
            payload: response,
        }))
        .await;
}

/// Sends GraphQL errors to the client.
async fn send_graphql_errors(
    operation_id: OperationId,
    errors: NonEmpty<lang_graphql::http::GraphQLError>,
    connection: &ws::Connection,
) {
    connection
        .send(ws::Message::Protocol(ServerMessage::Error {
            id: operation_id,
            payload: errors,
        }))
        .await;
}

/// Sends a complete message after the query/mutation execution finishes.
async fn send_complete(operation_id: OperationId, connection: &ws::Connection) {
    connection
        .send(ws::Message::Protocol(ServerMessage::Complete {
            id: operation_id,
        }))
        .await;
}

/// A helper enum to represent a GraphQL response.
enum GraphQLResponse {
    Ok(lang_graphql::http::Response),
    Error(NonEmpty<lang_graphql::http::GraphQLError>),
}

impl GraphQLResponse {
    fn new(response: lang_graphql::http::Response) -> Self {
        // If any error exist
        if let Some(errors) = response.errors {
            // If some data present
            if let Some(data) = response.data {
                // It is a partial response
                Self::Ok(lang_graphql::http::Response::partial(
                    data,
                    errors.into(),
                    response.headers,
                ))
            } else {
                // If no data present, it is an error
                Self::Error(errors)
            }
        } else {
            // No errors, Ok response
            Self::Ok(response)
        }
    }
}

/// Sends a single result (query or mutation) along with a completion message.
/// If there are errors, they are sent before the complete message.
async fn send_single_result_operation_response(
    operation_id: OperationId,
    raw_request: &lang_graphql::http::RawRequest,
    session: Session,
    headers: http::HeaderMap,
    result: execute::ExecuteQueryResult,
    expose_internal_errors: ExposeInternalErrors,
    connection: &ws::Connection,
) {
    let graphql_response =
        graphql_frontend::GraphQLResponse::from_result(result, expose_internal_errors).inner();
    // Execute pre-response plugins
    run_pre_response_plugins(raw_request, session, headers, &graphql_response, connection);
    // Send the ok response and a complete message.
    match GraphQLResponse::new(graphql_response) {
        GraphQLResponse::Ok(response) => {
            send_graphql_ok(operation_id.clone(), response, connection).await;
            send_complete(operation_id, connection).await;
        }
        GraphQLResponse::Error(errors) => {
            // No need to send a complete message after sending errors.
            // Ref: https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#complete
            send_graphql_errors(operation_id, errors, connection).await;
        }
    }
}

/// Sends a subscription operation response.
async fn send_subscription_operation_response(
    response_hash: &mut ResponseHash,
    operation_id: OperationId,
    raw_request: &lang_graphql::http::RawRequest,
    session: &Session,
    headers: &http::HeaderMap,
    response: lang_graphql::http::Response,
    connection: &ws::Connection,
) -> bool {
    let mut stop_subscription = false;
    if !response_hash.matches(&response) {
        // Execute pre-response plugins before sending the response
        run_pre_response_plugins(
            raw_request,
            session.clone(),
            headers.clone(),
            &response,
            connection,
        );
        match GraphQLResponse::new(response) {
            // Send the ok response
            GraphQLResponse::Ok(ok_response) => {
                send_graphql_ok(operation_id, ok_response, connection).await;
            }
            GraphQLResponse::Error(errors) => {
                // Send the errors and stop subscription
                send_graphql_errors(operation_id, errors, connection).await;
                stop_subscription = true;
            }
        }
    }
    stop_subscription
}

#[derive(thiserror::Error, Debug)]
#[error("error in executing pre-response plugins, unable to encode response: {0}")]
struct ExecutePreResponsePluginsError(#[from] serde_json::Error);

impl tracing_util::TraceableError for ExecutePreResponsePluginsError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::User
    }
}

fn run_pre_response_plugins(
    raw_request: &lang_graphql::http::RawRequest,
    session: Session,
    headers: http::HeaderMap,
    response: &lang_graphql::http::Response,
    connection: &ws::Connection,
) {
    // Execute pre-response plugins only if there are any
    if let Some(pre_response_plugins) = NonEmpty::from_vec(
        connection
            .context
            .plugin_configs
            .pre_response_plugins
            .clone(),
    ) {
        let tracer = tracing_util::global_tracer();
        // Errors will be traced in the following span.
        let _result: Result<(), ExecutePreResponsePluginsError> = tracer.in_span(
            "run_pre_response_plugins",
            "Running pre-response plugins",
            tracing_util::SpanVisibility::User,
            || {
                let response_json = serde_json::to_value(response)?;
                tracing_util::get_active_span(|span| {
                    // Open new trace for executing pre-response plugin with linking current span
                    let plugin_execution_tracing_strategy =
                        pre_response_plugin::ExecutePluginsTracing::NewTraceWithLink {
                            span_context: span.span_context().clone(),
                        };
                    pre_response_plugin::execute_pre_response_plugins_in_task(
                        pre_response_plugins,
                        connection.context.http_context.client.clone(),
                        session,
                        raw_request.clone(),
                        response_json,
                        headers,
                        plugin_execution_tracing_strategy,
                    );
                });
                Ok(())
            },
        );
    }
}
