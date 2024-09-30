pub mod tasks;
pub mod types;

use axum::{
    extract::ws,
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
};
use futures_util::StreamExt;

use crate::protocol;

static SEC_WEBSOCKET_PROTOCOL: &str = "Sec-WebSocket-Protocol";
static WEBSOCKET_CHANNEL_SIZE: usize = 50;

/// GraphQL WebSocket server implementation.
pub struct WebSocketServer {
    connections: types::Connections,
}

impl WebSocketServer {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            connections: types::Connections::new(), // Initialize an empty map of active connections
        }
    }

    /// Shuts down all active WebSocket connections.
    pub async fn shutdown(&self, reason: &'static str) {
        let mut map = self.connections.0.write().await;
        for (_, connection) in map.drain() {
            // Sending a close message through the channel will close the WebSocket connection
            connection
                .send(types::Message::force_reconnect(reason))
                .await;
        }
    }

    /// Fetch active websocket connections
    pub async fn fetch_active_connections(&self) -> Vec<types::ActiveConnection> {
        let map = self.connections.0.read().await;
        let mut active_connections = Vec::new();
        for connection in map.values() {
            active_connections.push(connection.to_active_connection().await);
        }
        active_connections
    }

    /// Handles the GraphQL WebSocket connection upgrade request.
    /// Validates the WebSocket protocol and upgrades the connection if valid.
    pub fn upgrade_and_handle_websocket(
        &self,
        ws_upgrade: ws::WebSocketUpgrade,
        headers: &HeaderMap,
        context: types::Context,
    ) -> Response {
        let tracer = tracing_util::global_tracer();
        tracer
            .in_span(
                "upgrade_and_handle_websocket",
                "Processing WebSocket upgrade request for GraphQL protoco",
                tracing_util::SpanVisibility::User,
                || {
                    let response = {
                        let protocol_header = headers
                            .get(SEC_WEBSOCKET_PROTOCOL)
                            .and_then(|val| val.to_str().ok());

                        // Create a websocket id
                        let websocket_id = types::WebSocketId::new();

                        // Set websocket id attribute
                        tracing_util::set_attribute_on_active_span(
                            tracing_util::AttributeVisibility::Default,
                            "websocket.id",
                            websocket_id.to_string(),
                        );

                        // Validate the WebSocket protocol
                        if let Some(protocol) = protocol_header {
                            if protocol == protocol::GRAPHQL_WS_PROTOCOL {
                                let connections = self.connections.clone();
                                // Upgrade the WebSocket connection and handle it
                                tracing_util::get_active_span(|span| {
                                    let span_context = span.span_context().clone();
                                    ws_upgrade
                                        .protocols(vec![protocol::GRAPHQL_WS_PROTOCOL])
                                        .on_upgrade(move |socket| {
                                            start_websocket_session(
                                                socket,
                                                websocket_id,
                                                context,
                                                connections,
                                                span_context,
                                            )
                                        })
                                })
                            } else {
                                // Return 400 Bad Request if the protocol doesn't match
                                let error_message =
                                    format!("Invalid WebSocket protocol: {protocol}");
                                (StatusCode::BAD_REQUEST, error_message).into_response()
                            }
                        } else {
                            // Return 400 Bad Request if the protocol header is missing
                            let error_message = format!("Missing {SEC_WEBSOCKET_PROTOCOL} header");
                            (StatusCode::BAD_REQUEST, error_message).into_response()
                        }
                    };
                    tracing_util::TraceableHttpResponse::new(response, "/graphql")
                },
            )
            .response
    }
}

/// Handles the WebSocket connection by splitting it into sender and receiver.
/// Manages incoming and outgoing messages and initializes a connection.
async fn start_websocket_session(
    socket: ws::WebSocket,
    websocket_id: types::WebSocketId,
    context: types::Context,
    connections: types::Connections,
    parent_span_context: tracing_util::SpanContext,
) {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async_with_link(
            "start_websocket_session",
            "Start a WebSocket Session",
            tracing_util::SpanVisibility::User,
            parent_span_context,
            || {
                // Set websocket id attribute
                tracing_util::set_attribute_on_active_span(
                    tracing_util::AttributeVisibility::Default,
                    "websocket.id",
                    websocket_id.to_string(),
                );
                Box::pin(async {
                    // Split the socket into a sender and receiver
                    let (websocket_sender, websocket_receiver) = socket.split();

                    // Create a channel for communicating with the WebSocket connection
                    let (channel_sender, channel_receiver) =
                        tokio::sync::mpsc::channel::<types::Message>(WEBSOCKET_CHANNEL_SIZE);

                    // Create a new WebSocket connection instance
                    let connection = connections
                        .new_connection(websocket_id, context, channel_sender)
                        .await;

                    let this_span_context =
                        tracing_util::get_active_span(|span| span.span_context().clone());

                    // Spawn a task to verify the graphql-ws connection_init state with a timeout
                    let init_checker_task = tokio::spawn(tasks::verify_connection_init(
                        connection.clone(),
                        protocol::CONNECTION_INIT_TIMEOUT,
                        this_span_context.clone(),
                    ));

                    // Spawn a task to handle outgoing messages
                    let outgoing_task = tokio::spawn(tasks::manage_outgoing_messages(
                        connection.clone(),
                        websocket_sender,
                        channel_receiver,
                        this_span_context.clone(),
                    ));

                    // Spawn a task to handle incoming messages
                    let incoming_task = tokio::spawn(tasks::process_incoming_message(
                        connection.clone(),
                        websocket_receiver,
                        this_span_context,
                    ));

                    // Handle the result of the initialization checker
                    match init_checker_task.await {
                        Ok(Ok(())) => {
                            // Connection initialized successfully within the specified time
                            let tasks = vec![incoming_task, outgoing_task];
                            // Both tasks are running concurrently. They are essential for the connection to work.
                            // If any of them completes, the other task should be aborted and the connection closed.
                            // Wait for any task to complete
                            let (_, _, remaining_tasks) =
                                futures_util::future::select_all(tasks).await;
                            // Abort remaining tasks after one completes
                            for task in remaining_tasks {
                                task.abort();
                            }
                        }
                        Ok(Err(tasks::ConnectionTimeOutError)) => {
                            // Connection not initialized within the specified time, send close message
                            connection.send(types::Message::conn_init_timeout()).await;
                            // Abort all tasks
                            incoming_task.abort();
                            outgoing_task.abort();
                        }
                        Err(_e) => {
                            // Handle internal server error
                            connection
                                .send(types::Message::internal_server_error())
                                .await;
                        }
                    };

                    // Remove the connection from the active connections map
                    connections.drop(&connection.id).await;

                    tracing_util::Successful::new(())
                })
            },
        )
        .await
        .into_inner();
}
