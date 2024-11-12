use axum::extract::ws;
use futures_util::{SinkExt, StreamExt};
use tokio::time::{timeout, Duration};

use super::types;
use crate::metrics::WebSocketMetrics;
use crate::protocol;

/// Enum to represent whether the loop should continue or break.
#[derive(PartialEq)]
enum BreakLoop {
    /// Indicates the loop should break.
    Break,
    /// Indicates the loop should continue.
    Dont,
}

/// Error type for connection timeouts during WebSocket initialization.
#[derive(thiserror::Error, Debug)]
#[error("Connection initialization timed out")]
pub(crate) struct ConnectionTimeOutError;

impl tracing_util::TraceableError for ConnectionTimeOutError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::User
    }
}

/// Checks if the graphql-ws protocol is initialized within the specified timeout duration.
/// Ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#connectioninit>
pub(crate) async fn verify_connection_init<M: Send>(
    connection: types::Connection<M>,
    timeout_duration: Duration,
    parent_span_link: tracing_util::SpanLink,
) -> Result<(), ConnectionTimeOutError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .new_trace_async_with_link(
            "websocket_verify_connection_init",
            "Verifying graphql-ws protocol connection_init within timeout",
            tracing_util::SpanVisibility::User,
            parent_span_link,
            || {
                Box::pin(async {
                    if !matches!(
                        timeout(timeout_duration, wait_for_initialization(connection)).await,
                        Ok(())
                    ) {
                        return Err(ConnectionTimeOutError);
                    }
                    Ok(())
                })
            },
        )
        .await
}

/// Waits for the WebSocket connection to reach the initialized state.
///
/// This function checks the state of the connection and only returns once initialization is complete.
async fn wait_for_initialization<M>(connection: types::Connection<M>) {
    loop {
        let state = connection.protocol_init_state.read().await;
        match *state {
            protocol::types::ConnectionInitState::Initialized { .. } => break,
            protocol::types::ConnectionInitState::NotInitialized => {
                // Release the lock before sleeping
                drop(state);
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }
}

/// Waits until the connection expires and sends a close message.
pub(crate) async fn wait_until_expiry<M>(
    connection: types::Connection<M>,
    expiry: std::time::Duration,
) {
    tokio::time::sleep(expiry).await;
    connection.send(types::Message::conn_expired()).await;
}

/// Handles incoming WebSocket messages from the client.
/// This task runs indefinitely until the connection is closed or an error occurs.
pub(crate) async fn process_incoming_message<M: WebSocketMetrics>(
    connection: types::Connection<M>,
    mut websocket_receiver: futures_util::stream::SplitStream<ws::WebSocket>,
    parent_span_link: tracing_util::SpanLink,
) {
    let tracer = tracing_util::global_tracer();
    while let Some(message) = websocket_receiver.next().await {
        let break_loop = tracer
            .new_trace_async_with_link(
                "websocket_process_incoming_message",
                "Processing incoming WebSocket message",
                tracing_util::SpanVisibility::User,
                parent_span_link.clone(),
                || {
                    Box::pin(async {
                        // Parse message
                        let break_loop = match parse_incoming_message(message) {
                            // Handle a close message from the client
                            Ok(ParsedClientMessage::Close) => {
                                // Add this event
                                tracing_util::add_event_on_active_span(
                                    "Received close message from client; closing connection"
                                        .to_string(),
                                );
                                BreakLoop::Break
                            }
                            // Handle other WebSocket messages
                            Ok(ParsedClientMessage::Protocol(client_message)) => {
                                protocol::handle_graphql_ws_message(
                                    connection.clone(),
                                    client_message,
                                )
                                .await;
                                BreakLoop::Dont
                            }
                            // Handle an error while receiving messages from the WebSocket
                            Err(ParseError::WebSocket(_err)) => {
                                tracing_util::add_event_on_active_span(
                                    "Unable to receive message from client; closing connection"
                                        .to_string(),
                                );
                                BreakLoop::Break
                            }
                            Err(ParseError::Json(err)) => {
                                // Send invalid message format error if deserialization fails
                                connection
                                    .send(types::Message::invalid_message_format(format!(
                                        "Invalid message format: {err}"
                                    )))
                                    .await;
                                tracing_util::add_event_on_active_span(
                                    "Invalid JSON message received; closing connection".to_string(),
                                );
                                BreakLoop::Break
                            }
                        };
                        tracing_util::Successful::new(break_loop)
                    })
                },
            )
            .await
            .into_inner();
        if break_loop == BreakLoop::Break {
            break;
        }
    }
}

enum ParsedClientMessage {
    Close,
    Protocol(protocol::types::ClientMessage),
}

#[derive(thiserror::Error, Debug)]
enum ParseError {
    #[error("Unable to fetch message from WebSocket: {0}")]
    WebSocket(#[from] axum::Error),
    #[error("Unable to parse WebSocket message: {0}")]
    Json(#[from] serde_json::Error),
}

impl tracing_util::TraceableError for ParseError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::User
    }
}

fn parse_incoming_message(
    message: Result<ws::Message, axum::Error>,
) -> Result<ParsedClientMessage, ParseError> {
    let tracer = tracing_util::global_tracer();
    tracer.in_span(
        "parse_incoming_message",
        "Parse WebSocket message frame",
        tracing_util::SpanVisibility::User,
        || {
            let message = message?;
            match message {
                ws::Message::Close(_) => Ok(ParsedClientMessage::Close),
                message => {
                    let client_message = serde_json::from_slice::<protocol::types::ClientMessage>(
                        &message.into_data(),
                    )?;
                    Ok(ParsedClientMessage::Protocol(client_message))
                }
            }
        },
    )
}

#[derive(thiserror::Error, Debug)]
enum ManageOutgoingMessageError {
    #[error("Unable to serialize message into JSON: {0}")]
    Json(#[from] serde_json::Error),
    #[error("Unable to send message to WebSocket: {0}")]
    WebSocket(#[from] axum::Error),
}

impl tracing_util::TraceableError for ManageOutgoingMessageError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::Internal
    }
}

/// Manages outgoing WebSocket messages.
/// Sends messages from the connection's channel to the WebSocket client.
pub(crate) async fn manage_outgoing_messages<M: WebSocketMetrics + Sync>(
    connection: types::Connection<M>,
    mut websocket_sender: futures_util::stream::SplitSink<ws::WebSocket, ws::Message>,
    mut channel_receiver: tokio::sync::mpsc::Receiver<types::Message>,
    parent_span_link: tracing_util::SpanLink,
) {
    while let Some(message) = channel_receiver.recv().await {
        let tracer = tracing_util::global_tracer();
        let result: Result<BreakLoop, ManageOutgoingMessageError> = tracer
            .new_trace_async_with_link(
                "websocket_manage_outgoing_message",
                "Manage outgoing WebSocket message",
                tracing_util::SpanVisibility::User,
                parent_span_link.clone(),
                || {
                    Box::pin(async {
                        match message {
                            // Handle raw WebSocket messages
                            types::Message::Raw(msg) => {
                                let is_close = matches!(msg, ws::Message::Close(_));
                                websocket_sender.send(msg).await?;
                                if is_close {
                                    Ok(BreakLoop::Break)
                                } else {
                                    Ok(BreakLoop::Dont)
                                }
                            }
                            // Handle protocol messages by serializing them into JSON
                            types::Message::Protocol(msg) => {
                                match serde_json::to_string(&msg) {
                                    Ok(json_text) => {
                                        websocket_sender.send(ws::Message::Text(json_text)).await?;
                                    }
                                    Err(err) => {
                                        // Send internal server error message if serialization fails
                                        websocket_sender
                                            .send(types::internal_server_message_ws())
                                            .await?;
                                        // Return error
                                        Err(err)?;
                                    }
                                };

                                // Stop the poller if the operation is complete or an error occurred
                                if let Some(operation_id) = msg.is_complete_or_error() {
                                    connection.stop_poller(operation_id).await;
                                }
                                Ok(BreakLoop::Dont)
                            }
                        }
                    })
                },
            )
            .await;

        match result {
            Ok(BreakLoop::Dont) => {}
            Ok(BreakLoop::Break) | Err(_) => {
                break;
            }
        }
    }
}

/// Sends keepalive messages to the client at regular intervals.
pub(crate) async fn send_keepalive<M>(connection: types::Connection<M>) {
    loop {
        tokio::time::sleep(protocol::KEEPALIVE_INTERVAL).await;
        connection.send(types::Message::keep_alive()).await;
    }
}
