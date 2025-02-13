pub mod tasks;
pub mod types;

use axum::{
    extract::ws,
    http::{
        header::{InvalidHeaderValue, ToStrError},
        HeaderMap, StatusCode,
    },
    response::{IntoResponse, Response},
};
use futures_util::StreamExt;

use crate::metrics::WebSocketMetrics;
use crate::protocol;

pub static SEC_WEBSOCKET_PROTOCOL: &str = "Sec-WebSocket-Protocol";
static SEC_WEBSOCKET_ID: &str = "Sec-WebSocket-Id";
static WEBSOCKET_CHANNEL_SIZE: usize = 50;

/// GraphQL WebSocket server implementation.
pub struct WebSocketServer<M> {
    pub connections: types::Connections<M>,
}

impl<M> WebSocketServer<M> {
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

    /// Handles the GraphQL WebSocket connection upgrade request.
    /// Validates the WebSocket protocol and upgrades the connection if valid.
    pub fn upgrade_and_handle_websocket(
        &self,
        client_address: std::net::SocketAddr,
        ws_upgrade: ws::WebSocketUpgrade,
        context: types::Context<M>,
    ) -> Response
    where
        M: WebSocketMetrics,
    {
        let tracer = tracing_util::global_tracer();
        let result: Result<_, WebSocketError> = tracer.in_span(
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
                    // Check if headers contain graphql-transport-ws protocol
                    check_protocol_in_headers(&context.handshake_headers)?;
                    let connections = self.connections.clone();
                    // Upgrade the WebSocket connection and handle it
                    let span_link = tracing_util::SpanLink::from_current_span();
                    // // Clone the websocket_id to move it into the closure
                    let websocket_id_cloned = websocket_id.clone();
                    let mut response = ws_upgrade
                        .protocols(vec![protocol::GRAPHQL_WS_PROTOCOL])
                        .on_upgrade(move |socket| {
                            start_websocket_session(
                                client_address,
                                socket,
                                websocket_id_cloned,
                                context,
                                connections,
                                span_link,
                            )
                        });
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
    /// Error when the Sec-WebSocket-Protocol header is missing
    #[error("Missing {SEC_WEBSOCKET_PROTOCOL} header")]
    MissingProtocolHeader,

    /// Error when the header value cannot be converted to a string
    #[error("{SEC_WEBSOCKET_PROTOCOL} header: {0}")]
    InvalidHeaderValue(#[from] ToStrError),

    /// Error when the GraphQL WebSocket protocol is not included
    #[error("Expecting {} protocol", protocol::GRAPHQL_WS_PROTOCOL)]
    ExpectingGraphqlWsProtocol,

    /// Error when setting the WebSocket ID header value fails in response
    #[error("Unable to set {SEC_WEBSOCKET_ID} header value: {0}")]
    WebSocketIdInvalidHeaderValue(#[from] InvalidHeaderValue),
}

impl tracing_util::TraceableError for WebSocketError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            Self::MissingProtocolHeader
            | Self::ExpectingGraphqlWsProtocol
            | Self::InvalidHeaderValue(_) => tracing_util::ErrorVisibility::User,
            Self::WebSocketIdInvalidHeaderValue(_) => tracing_util::ErrorVisibility::Internal,
        }
    }
}

impl IntoResponse for WebSocketError {
    fn into_response(self) -> Response {
        match self {
            Self::MissingProtocolHeader
            | Self::ExpectingGraphqlWsProtocol
            | Self::InvalidHeaderValue(_) => {
                (StatusCode::BAD_REQUEST, self.to_string()).into_response()
            }
            Self::WebSocketIdInvalidHeaderValue(_) => {
                (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
            }
        }
    }
}

/// Handles the WebSocket connection by splitting it into sender and receiver.
/// Manages incoming and outgoing messages and initializes a connection.
async fn start_websocket_session<M: WebSocketMetrics>(
    client_address: std::net::SocketAddr,
    socket: ws::WebSocket,
    websocket_id: types::WebSocketId,
    context: types::Context<M>,
    connections: types::Connections<M>,
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
                    let connection_expiry = context.connection_expiry.clone();
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

                    let expiry_task = match connection_expiry {
                        types::ConnectionExpiry::Never => None,
                        types::ConnectionExpiry::After(duration) => {
                            // Spawn a task to wait until the connection expires
                            // The task will send a close message to the client after the expiry duration.
                            // Sending a close message will make the outgoing task exit, thus closing the connection.
                            let connection = connection.clone();
                            Some(tokio::spawn(async move {
                                tasks::wait_until_expiry(connection, duration).await;
                            }))
                        }
                    };

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
                        client_address,
                        connection.clone(),
                        websocket_receiver,
                        this_span_link,
                    ));

                    // Spawn a task to send keep-alive messages at regular intervals
                    let keepalive_task = tokio::spawn(tasks::send_keepalive(connection.clone()));

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

                    // Abort the expiry task
                    if let Some(task) = expiry_task {
                        task.abort();
                    }
                    // Abort the keepalive task
                    keepalive_task.abort();
                    // Remove the connection from the active connections map
                    connections.drop(&connection.id).await;

                    tracing_util::Successful::new(())
                })
            },
        )
        .await
        .into_inner();
}

/// Validates that the required WebSocket protocol is present in the connection headers.
///
/// This function checks that:
/// 1. The Sec-WebSocket-Protocol header exists
/// 2. The header contains the GraphQL WebSocket ("graphql-transport-ws") protocol
pub(crate) fn check_protocol_in_headers(headers: &HeaderMap) -> Result<(), WebSocketError> {
    let protocol_header_values = headers.get_all(SEC_WEBSOCKET_PROTOCOL).iter();
    let mut provided_protocols = Vec::new();
    for protocol in protocol_header_values {
        let protocol_str = protocol.to_str()?;
        provided_protocols.extend_from_slice(&parse_comma_separated_header_values(protocol_str));
    }
    if provided_protocols.is_empty() {
        Err(WebSocketError::MissingProtocolHeader)?;
    } else if !provided_protocols.contains(&protocol::GRAPHQL_WS_PROTOCOL) {
        Err(WebSocketError::ExpectingGraphqlWsProtocol)?;
    }
    Ok(())
}

/// Parses a comma-separated header value into a vector of trimmed strings.
fn parse_comma_separated_header_values(header: &str) -> Vec<&str> {
    header
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::http::HeaderValue;

    #[test]
    fn test_check_protocol_missing_header() {
        let headers = HeaderMap::new();
        let result = check_protocol_in_headers(&headers);
        assert!(matches!(result, Err(WebSocketError::MissingProtocolHeader)));
    }

    #[test]
    fn test_check_protocol_wrong_protocol() {
        let mut headers = HeaderMap::new();
        headers.append(
            SEC_WEBSOCKET_PROTOCOL,
            HeaderValue::from_static("wrong-protocol"),
        );
        let result = check_protocol_in_headers(&headers);
        assert!(matches!(
            result,
            Err(WebSocketError::ExpectingGraphqlWsProtocol)
        ));
    }

    #[test]
    fn test_check_protocol_valid() {
        let mut headers = HeaderMap::new();
        headers.append(
            SEC_WEBSOCKET_PROTOCOL,
            HeaderValue::from_static(protocol::GRAPHQL_WS_PROTOCOL),
        );
        let result = check_protocol_in_headers(&headers);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_protocol_multiple_values() {
        let mut headers = HeaderMap::new();
        headers.append(
            SEC_WEBSOCKET_PROTOCOL,
            HeaderValue::from_static("other-protocol, graphql-transport-ws"),
        );
        let result = check_protocol_in_headers(&headers);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_protocol_multiple_headers() {
        let mut headers = HeaderMap::new();
        headers.append(
            SEC_WEBSOCKET_PROTOCOL,
            HeaderValue::from_static("other-protocol"),
        );
        headers.append(
            SEC_WEBSOCKET_PROTOCOL,
            HeaderValue::from_static(protocol::GRAPHQL_WS_PROTOCOL),
        );
        let result = check_protocol_in_headers(&headers);
        assert!(result.is_ok());
    }
}
