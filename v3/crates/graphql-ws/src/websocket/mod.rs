pub mod types;

use axum::{
    extract::ws,
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
};
use futures_util::{SinkExt, StreamExt};
use tokio::time::{timeout, Duration};

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

    /// Get active websocket connections
    pub async fn get_active_connections(&self) -> Vec<types::ActiveConnection> {
        let map = self.connections.0.read().await;
        let mut active_connections = Vec::new();
        for connection in map.values() {
            active_connections.push(connection.to_active_connection().await);
        }
        active_connections
    }

    /// Handles the GraphQL WebSocket connection upgrade request.
    /// Validates the WebSocket protocol and upgrades the connection if valid.
    pub fn handle_graphql_websocket(
        &self,
        ws_upgrade: ws::WebSocketUpgrade,
        headers: &HeaderMap,
        context: types::Context,
    ) -> Response {
        let protocol_header = headers
            .get(SEC_WEBSOCKET_PROTOCOL)
            .and_then(|val| val.to_str().ok());

        // Validate the WebSocket protocol
        if let Some(protocol) = protocol_header {
            if protocol != protocol::GRAPHQL_WS_PROTOCOL {
                // Return 400 Bad Request if the protocol doesn't match
                let error_message = format!("Invalid WebSocket protocol: {protocol}");
                return (StatusCode::BAD_REQUEST, error_message).into_response();
            }
        } else {
            // Return 400 Bad Request if the protocol header is missing
            let error_message = format!("Missing {SEC_WEBSOCKET_PROTOCOL} header");
            return (StatusCode::BAD_REQUEST, error_message).into_response();
        }

        let connections = self.connections.clone();

        // Upgrade the WebSocket connection and handle it
        ws_upgrade
            .protocols(vec![protocol::GRAPHQL_WS_PROTOCOL])
            .on_upgrade(move |socket| handle_websocket(socket, context, connections))
    }
}

/// Handles the WebSocket connection by splitting it into sender and receiver.
/// Manages incoming and outgoing messages and initializes a connection.
async fn handle_websocket(
    socket: ws::WebSocket,
    context: types::Context,
    connections: types::Connections,
) {
    // Split the socket into a sender and receiver
    let (websocket_sender, websocket_receiver) = socket.split();

    // Create a channel for communicating with the WebSocket connection
    let (channel_sender, channel_receiver) =
        tokio::sync::mpsc::channel::<types::Message>(WEBSOCKET_CHANNEL_SIZE);

    // Create a new WebSocket connection instance
    let connection = connections.new_connection(context, channel_sender).await;

    // Spawn a task to check the initialization state with a timeout
    let init_checker_task = tokio::spawn(check_init_state(
        connection.clone(),
        protocol::CONNECTION_INIT_TIMEOUT,
    ));

    // Spawn a task to handle outgoing messages
    let outgoing_task = tokio::spawn(handle_outgoing(
        connection.clone(),
        websocket_sender,
        channel_receiver,
    ));

    // Spawn a task to handle incoming messages
    let incoming_task = tokio::spawn(handle_incoming(connection.clone(), websocket_receiver));

    // Handle the result of the initialization checker
    match init_checker_task.await {
        Ok(true) => {
            // Connection initialized successfully within the specified time
            let tasks = vec![incoming_task, outgoing_task];
            // Both tasks are running concurrently. They are essential for the connection to work.
            // If any of them completes, the other task should be aborted and the connection closed.
            // Wait for any task to complete
            let (_, _, remaining_tasks) = futures_util::future::select_all(tasks).await;
            // Abort remaining tasks after one completes
            for task in remaining_tasks {
                task.abort();
            }
        }
        Ok(false) => {
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
}

/// Handles outgoing WebSocket messages.
/// Sends messages from the connection's channel to the WebSocket client.
async fn handle_outgoing(
    connection: types::Connection,
    mut websocket_sender: futures_util::stream::SplitSink<ws::WebSocket, ws::Message>,
    mut channel_receiver: tokio::sync::mpsc::Receiver<types::Message>,
) {
    while let Some(message) = channel_receiver.recv().await {
        match message {
            // Handle raw WebSocket messages
            types::Message::Raw(msg) => {
                let is_close = matches!(msg, ws::Message::Close(_));
                if websocket_sender.send(msg).await.is_err() {
                    break;
                }
                if is_close {
                    break;
                }
            }
            // Handle protocol messages by serializing them into JSON
            types::Message::Protocol(msg) => {
                match serde_json::to_string(&msg) {
                    Ok(bytes) => {
                        if websocket_sender
                            .send(ws::Message::Text(bytes))
                            .await
                            .is_err()
                        {
                            break;
                        }
                        // Stop the poller if the operation is complete or an error occurred
                        if let Some(operation_id) = msg.is_complete_or_error() {
                            connection.stop_poller(operation_id).await;
                        }
                    }
                    Err(_err) => {
                        // Send internal server error message if serialization fails
                        if websocket_sender
                            .send(types::internal_server_message_ws())
                            .await
                            .is_err()
                        {
                            break;
                        }
                        break;
                    }
                };
            }
        }
    }
}

/// Handles incoming WebSocket messages from the client.
/// This task runs indefinitely until the connection is closed or an error occurs.
async fn handle_incoming(
    connection: types::Connection,
    mut websocket_receiver: futures_util::stream::SplitStream<ws::WebSocket>,
) {
    while let Some(result) = websocket_receiver.next().await {
        match result {
            // Handle a close message from the client
            Ok(ws::Message::Close(_)) => {
                // Close the connection by exiting the loop
                break;
            }
            // Handle other WebSocket messages
            Ok(message) => {
                match serde_json::from_slice::<protocol::types::ClientMessage>(&message.into_data())
                {
                    Ok(client_message) => {
                        protocol::handle_client_message(connection.clone(), client_message).await;
                    }
                    Err(err) => {
                        // Send invalid message format error if deserialization fails
                        connection
                            .send(types::Message::invalid_message_format(format!(
                                "Invalid message format: {err}"
                            )))
                            .await;
                        break;
                    }
                }
            }
            // Handle an error while receiving messages from the WebSocket
            Err(_err) => {
                break;
            }
        }
    }
}

/// Checks if the WebSocket connection is initialized within the specified timeout duration.
/// Returns `true` if the connection is initialized on time, otherwise returns `false`.
async fn check_init_state(connection: types::Connection, timeout_duration: Duration) -> bool {
    matches!(
        timeout(timeout_duration, wait_for_initialization(connection)).await,
        Ok(())
    )
}

/// Waits for the WebSocket connection to reach the initialized state.
/// Continuously checks the connection state and breaks when initialization completes.
async fn wait_for_initialization(connection: types::Connection) {
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
