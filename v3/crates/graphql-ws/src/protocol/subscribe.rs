use axum::http;
use blake2::{Blake2b, Digest};
use execute::{self, plan, ExposeInternalErrors};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use nonempty::NonEmpty;
use std::sync::Arc;

use super::types::{ConnectionInitState, OperationId, ServerMessage};
use crate::poller;
use crate::websocket::types as ws;

/// Handles the subscription message from the client.
/// It either starts a new poller or sends a close message if the poller with given operation_id already exists.
pub async fn handle_subscribe(
    connection: ws::Connection,
    operation_id: OperationId,
    payload: lang_graphql::http::RawRequest,
) {
    match *connection.protocol_init_state.read().await {
        // If the connection is not initialized, send an unauthorized message.
        ConnectionInitState::NotInitialized => {
            connection.send(ws::Message::unauthorized()).await;
        }
        // If the connection is initialized, handle the subscription request.
        ConnectionInitState::Initialized {
            ref session,
            ref headers,
        } => {
            // If the poller for this operation already exists, send a close message.
            if connection.poller_exists(&operation_id).await {
                connection
                    .send(ws::Message::subscriber_already_exists(&operation_id))
                    .await;
            } else {
                // Start a new poller for the operation and insert it into the connection.
                let poller = start_poller(
                    operation_id.clone(),
                    connection.context.clone(),
                    session.clone(),
                    headers.clone(),
                    connection.clone(),
                    payload,
                );
                connection.insert_poller(operation_id, poller).await;
            }
        }
    }
}

/// Starts a new poller to handle GraphQL operations (queries, mutations, subscriptions).
fn start_poller(
    operation_id: OperationId,
    context: Arc<ws::Context>,
    session: Session,
    headers: http::HeaderMap,
    connection: ws::Connection,
    raw_request: lang_graphql::http::RawRequest,
) -> poller::Poller {
    poller::Poller::new(|| {
        Box::pin(async move {
            // Executes the GraphQL request and handles any errors.
            execute_request(
                operation_id.clone(),
                context.expose_internal_errors,
                session,
                headers,
                &connection,
                raw_request,
            )
            .await;
        })
    })
}

/// Executes the GraphQL request, handling queries, mutations, and subscriptions.
pub async fn execute_request(
    operation_id: OperationId,
    expose_internal_errors: ExposeInternalErrors,
    session: Session,
    headers: http::HeaderMap,
    connection: &ws::Connection,
    raw_request: lang_graphql::http::RawRequest,
) {
    // Execute the GraphQL request and handle any errors.
    match execute_request_internal(
        operation_id.clone(),
        session,
        headers,
        connection,
        raw_request,
    )
    .await
    {
        Ok(()) => {}
        Err(e) => {
            // If an error occurs, send an error message.
            let gql_error = e.to_graphql_error(expose_internal_errors);
            let message = ServerMessage::Error {
                id: operation_id,
                payload: NonEmpty::new(gql_error),
            };
            connection.send(ws::Message::Protocol(message)).await;
        }
    }
}

async fn execute_request_internal(
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
        graphql_frontend::normalize_request(schema, &session, query, raw_request)?;
    // Generate Intermediate Representation (IR) from the query.
    let ir = graphql_frontend::build_ir(schema, &session, &headers, &normalized_request)?;
    // Build a request plan based on the IR.
    let request_plan = graphql_frontend::build_request_plan(&ir)?;

    match request_plan {
        // Handle mutations.
        plan::RequestPlan::MutationPlan(mutation_plan) => {
            let execute_query_result =
                plan::execute_mutation_plan(http_context, mutation_plan, project_id).await;
            let response = graphql_frontend::GraphQLResponse::from_result(
                execute_query_result,
                expose_internal_errors,
            )
            .inner();
            send_single_result_operation(operation_id, response, connection).await;
        }
        // Handle queries.
        plan::RequestPlan::QueryPlan(query_plan) => {
            let execute_query_result =
                plan::execute_query_plan(http_context, query_plan, project_id).await;
            let response = graphql_frontend::GraphQLResponse::from_result(
                execute_query_result,
                expose_internal_errors,
            )
            .inner();
            send_single_result_operation(operation_id, response, connection).await;
        }
        // Handle subscriptions by starting a polling loop to repeatedly fetch data.
        plan::RequestPlan::SubscriptionPlan(alias, plan) => {
            match plan::resolve_ndc_subscription_execution(plan).await {
                Ok(ndc_subscription) => {
                    let query_request = ndc_subscription.query_request;
                    let data_connector = ndc_subscription.data_connector.clone();
                    let selection_set = ndc_subscription.selection_set.clone();
                    let process_response_as = ndc_subscription.process_response_as;
                    let is_nullable = process_response_as.is_nullable();
                    let polling_interval_duration =
                        tokio::time::Duration::from_millis(ndc_subscription.polling_interval_ms);

                    // Initialize a response hash to track changes in the response.
                    let mut response_hash = ResponseHash::new();

                    // A loop to periodically wait for the polling interval, then fetch data from NDC.
                    loop {
                        tokio::time::sleep(polling_interval_duration).await;
                        match execute::fetch_from_data_connector(
                            http_context,
                            &query_request,
                            &data_connector,
                            None,
                        )
                        .await
                        {
                            // Handle successful response and send the data.
                            Ok(response) => {
                                let response_rowsets = response.as_latest_rowsets();
                                let processed_response = execute::process_response(
                                    &selection_set,
                                    response_rowsets,
                                    &process_response_as,
                                );
                                let mut root_fields = IndexMap::new();
                                root_fields.insert(
                                    alias.clone(),
                                    plan::RootFieldResult::from_processed_response(
                                        is_nullable,
                                        processed_response,
                                    ),
                                );
                                let graphql_response = execute::ExecuteQueryResult { root_fields }
                                    .to_graphql_response(expose_internal_errors);

                                // If there are errors in the response, send them and stop polling.
                                match graphql_response.errors {
                                    Some(errors) => {
                                        send_graphql_errors(operation_id, errors, connection).await;
                                        break;
                                    }
                                    None => {
                                        send_graphql_ok(
                                            Some(&mut response_hash),
                                            operation_id.clone(),
                                            graphql_response,
                                            connection,
                                        )
                                        .await;
                                    }
                                }
                            }
                            // Handle errors by sending error messages and breaking the loop.
                            Err(e) => {
                                let graphql_error = execute::FieldError::from(e)
                                    .to_graphql_error(expose_internal_errors, None);
                                send_graphql_errors(
                                    operation_id,
                                    NonEmpty::new(graphql_error),
                                    connection,
                                )
                                .await;
                                break;
                            }
                        }
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
    response_hash: Option<&mut ResponseHash>,
    operation_id: OperationId,
    response: lang_graphql::http::Response,
    connection: &ws::Connection,
) {
    // Send response only when it is different from previous response
    if !response_hash.map_or(false, |hash| hash.matches(&response)) {
        connection
            .send(ws::Message::Protocol(ServerMessage::Next {
                id: operation_id,
                payload: response,
            }))
            .await;
    }
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

/// Sends a single result (query or mutation) along with a completion message.
/// If there are errors, they are sent before the complete message.
async fn send_single_result_operation(
    operation_id: OperationId,
    response: lang_graphql::http::Response,
    connection: &ws::Connection,
) {
    // If there is some data in the response, send the ok response and a complete message.
    if response.data.is_some() {
        send_graphql_ok(None, operation_id.clone(), response, connection).await;
        send_complete(operation_id, connection).await;
    } else if let Some(errors) = response.errors {
        // No need to send a complete message after sending errors.
        // Ref: https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#complete
        send_graphql_errors(operation_id, errors, connection).await;
    }
}
