pub mod tasks;
pub mod types;

use axum::{
    extract::ws,
    http::{header::InvalidHeaderValue, HeaderMap, StatusCode},
    response::{IntoResponse, Response},
};
use futures_util::StreamExt;

use crate::protocol;

pub static SEC_WEBSOCKET_PROTOCOL: &str = "Sec-WebSocket-Protocol";
static SEC_WEBSOCKET_ID: &str = "Sec-WebSocket-Id";
static WEBSOCKET_CHANNEL_SIZE: usize = 50;

/// GraphQL WebSocket server implementation.
pub struct WebSocketServer {
    pub connections: types::Connections,
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
        let result = tracer.in_span(
            "upgrade_and_handle_websocket",
            "Processing WebSocket upgrade request for GraphQL protoco",
            tracing_util::SpanVisibility::User,
            || {
                // Create a websocket id
                let websocket_id = types::WebSocketId::new();

                // Set websocket id attribute
                tracing_util::set_attribute_on_active_span(
                    tracing_util::AttributeVisibility::Default,
                    "websocket.id",
                    websocket_id.to_string(),
                );

                // Put the websocket id in the baggage
                let trace_baggage = vec![tracing_util::KeyValue::new(
                    "websocket.id",
                    websocket_id.to_string(),
                )];
                tracing_util::run_with_baggage(trace_baggage, || {
                    let protocol = headers
                        .get(SEC_WEBSOCKET_PROTOCOL)
                        .and_then(|val| val.to_str().ok())
                        .ok_or(WebSocketError::MissingProtocolHeader)?;

                    let mut response = if protocol == protocol::GRAPHQL_WS_PROTOCOL {
                        let connections = self.connections.clone();
                        // Upgrade the WebSocket connection and handle it
                        let span_link = tracing_util::SpanLink::from_current_span();
                        // Clone the websocket_id to move it into the closure
                        let websocket_id = websocket_id.clone();
                        ws_upgrade
                            .protocols(vec![protocol::GRAPHQL_WS_PROTOCOL])
                            .on_upgrade(move |socket| {
                                start_websocket_session(
                                    socket,
                                    websocket_id,
                                    context,
                                    connections,
                                    span_link,
                                )
                            })
                    } else {
                        // Return error if the protocol doesn't match
                        return Err(WebSocketError::InvalidProtocol(protocol.to_string()));
                    };

                    // Set the WebSocket id response header
                    response
                        .headers_mut()
                        .insert(SEC_WEBSOCKET_ID, websocket_id.to_string().parse()?);
                    Ok(response)
                })
            },
        );

        result.unwrap_or_else(IntoResponse::into_response)
    }
}

/// Error types for WebSocket connections.
#[derive(Debug, thiserror::Error)]
pub enum WebSocketError {
    #[error("Missing {SEC_WEBSOCKET_PROTOCOL} header")]
    MissingProtocolHeader,
    #[error("Invalid WebSocket protocol: {0}")]
    InvalidProtocol(String),
    #[error("Unable to set {SEC_WEBSOCKET_ID} header value: {0}")]
    WebSocketIdInvalidHeaderValue(#[from] InvalidHeaderValue),
}

impl tracing_util::TraceableError for WebSocketError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            Self::MissingProtocolHeader | Self::InvalidProtocol(_) => {
                tracing_util::ErrorVisibility::User
            }
            Self::WebSocketIdInvalidHeaderValue(_) => tracing_util::ErrorVisibility::Internal,
        }
    }
}

impl IntoResponse for WebSocketError {
    fn into_response(self) -> Response {
        match self {
            Self::MissingProtocolHeader => (
                StatusCode::BAD_REQUEST,
                "Missing Sec-WebSocket-Protocol header",
            )
                .into_response(),
            Self::InvalidProtocol(protocol) => (
                StatusCode::BAD_REQUEST,
                format!("Invalid WebSocket protocol: {protocol}"),
            )
                .into_response(),
            Self::WebSocketIdInvalidHeaderValue(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
            }
        }
    }
}

/// Handles the WebSocket connection by splitting it into sender and receiver.
/// Manages incoming and outgoing messages and initializes a connection.
async fn start_websocket_session(
    socket: ws::WebSocket,
    websocket_id: types::WebSocketId,
    context: types::Context,
    connections: types::Connections,
    parent_span_link: tracing_util::SpanLink,
) {
    let tracer = tracing_util::global_tracer();
    tracer
        .new_trace_async_with_link(
            "start_websocket_session",
            "Start a WebSocket Session",
            tracing_util::SpanVisibility::User,
            parent_span_link,
            || {
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

                    let this_span_link = tracing_util::SpanLink::from_current_span();

                    // Spawn a task to verify the graphql-ws connection_init state with a timeout
                    let init_checker_task = tokio::spawn(tasks::verify_connection_init(
                        connection.clone(),
                        protocol::CONNECTION_INIT_TIMEOUT,
                        this_span_link.clone(),
                    ));

                    // Spawn a task to handle outgoing messages
                    let outgoing_task = tokio::spawn(tasks::manage_outgoing_messages(
                        connection.clone(),
                        websocket_sender,
                        channel_receiver,
                        this_span_link.clone(),
                    ));

                    // Spawn a task to handle incoming messages
                    let incoming_task = tokio::spawn(tasks::process_incoming_message(
                        connection.clone(),
                        websocket_receiver,
                        this_span_link,
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
                            // Abort incoming task
                            incoming_task.abort();
                            // A close message is handled by the outgoing task and it makes the task exit.
                            // So we need to wait for the task to complete
                            let _ = outgoing_task.await;
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
