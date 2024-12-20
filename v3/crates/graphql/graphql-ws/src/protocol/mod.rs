pub mod init;
pub mod subscribe;
pub mod types;

use crate::metrics::WebSocketMetrics;
use crate::websocket::types as ws;
use types::{ClientMessage, ServerMessage};

/// Protocol name for the GraphQL over WebSocket.
/// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#communication>
pub static GRAPHQL_WS_PROTOCOL: &str = "graphql-transport-ws";

/// Timeout for the connection initialization process.
pub static CONNECTION_INIT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(3);

/// Interval for sending keep-alive messages to the client.
pub static KEEPALIVE_INTERVAL: std::time::Duration = std::time::Duration::from_secs(5);

/// Handles incoming client messages and dispatches them to appropriate handlers.
pub async fn handle_graphql_ws_message<M: WebSocketMetrics>(
    client_address: std::net::SocketAddr,
    connection: ws::Connection<M>,
    message: ClientMessage,
) {
    let tracer = tracing_util::global_tracer();
    let message_type = message.message_type();
    tracer
        .in_span_async(
            "handle_graphql_ws_message",
            format!("Handle graphql_ws protocol message: {message_type}"),
            tracing_util::SpanVisibility::User,
            || {
                Box::pin(async {
                    match message {
                        // Handle the ConnectionInit message and initialize the connection
                        ClientMessage::ConnectionInit { payload } => {
                            init::handle_connection_init(connection, payload).await;
                        }
                        // Handle the Subscribe message and start the subscription process
                        ClientMessage::Subscribe {
                            id: operation_id,
                            payload,
                        } => {
                            subscribe::handle_subscribe(
                                client_address,
                                connection,
                                operation_id,
                                payload,
                            )
                            .await;
                        }
                        // Handle the Complete message and stop the corresponding poller
                        ClientMessage::Complete { id: operation_id } => {
                            connection.stop_poller(&operation_id).await;
                        }
                        // Respond to a Ping message by sending a Pong
                        ClientMessage::Ping => {
                            connection
                                .send(ws::Message::Protocol(Box::new(ServerMessage::Pong)))
                                .await;
                        }
                        // Ignore the Pong message as no action is needed
                        ClientMessage::Pong => {
                            // Do nothing
                        }
                    }
                    tracing_util::Successful::new(())
                })
            },
        )
        .await
        .into_inner();
}
