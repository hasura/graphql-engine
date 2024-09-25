pub mod init;
pub mod subscribe;
pub mod types;

use types::{ClientMessage, ServerMessage};

use crate::websocket::types as ws;

/// Protocol name for the GraphQL over WebSocket.
/// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#communication>
pub static GRAPHQL_WS_PROTOCOL: &str = "graphql-transport-ws";

/// Timeout for the connection initialization process.
pub static CONNECTION_INIT_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(10);

/// Handles incoming client messages and dispatches them to appropriate handlers.
pub async fn handle_client_message(connection: ws::Connection, message: ClientMessage) {
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
            subscribe::handle_subscribe(connection, operation_id, payload).await;
        }
        // Handle the Complete message and stop the corresponding poller
        ClientMessage::Complete { id: operation_id } => {
            connection.stop_poller(&operation_id).await;
        }
        // Respond to a Ping message by sending a Pong
        ClientMessage::Ping => {
            connection
                .send(ws::Message::Protocol(ServerMessage::Pong))
                .await;
        }
        // Ignore the Pong message as no action is needed
        ClientMessage::Pong => {
            // Do nothing
        }
    }
}
