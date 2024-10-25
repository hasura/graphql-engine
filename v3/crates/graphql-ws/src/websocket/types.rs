use axum::extract::ws;
use execute::{ExposeInternalErrors, HttpContext, ProjectId};
use hasura_authn::AuthConfig;
use metadata_resolve::LifecyclePluginConfigs;
use serde::Serialize;
use smol_str::SmolStr;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{mpsc::Sender, RwLock};

use crate::poller;
use crate::protocol::types as protocol;

/// Context required to handle a WebSocket connection
#[derive(Clone)] // Cheap to clone as heavy fields are wrapped in `Arc`
pub struct Context {
    pub http_context: HttpContext,
    pub expose_internal_errors: ExposeInternalErrors,
    pub project_id: Option<ProjectId>,
    pub schema: Arc<lang_graphql::schema::Schema<graphql_schema::GDS>>,
    pub auth_config: Arc<AuthConfig>,
    pub plugin_configs: Arc<LifecyclePluginConfigs>,
}

/// Represents a WebSocket connection ID.
#[derive(Clone, Serialize, PartialEq, Eq, Hash, derive_more::Display)]
pub struct WebSocketId(SmolStr);

impl WebSocketId {
    /// Creates a new WebSocket connection ID.
    pub fn new() -> Self {
        Self(SmolStr::new(uuid::Uuid::new_v4().to_string()))
    }
}

impl Default for WebSocketId {
    fn default() -> Self {
        Self::new()
    }
}

/// A mutable and free clone-able collection of WebSocket connections.
#[derive(Clone)]
pub struct Connections(pub Arc<RwLock<HashMap<WebSocketId, Connection>>>);

impl Connections {
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(HashMap::new())))
    }

    pub(crate) async fn new_connection(
        &self,
        id: WebSocketId,
        context: Context,
        channel: Sender<Message>,
    ) -> Connection {
        let new_connection = Connection::new(id, context, channel);
        let mut map = self.0.write().await;
        map.insert(new_connection.id.clone(), new_connection.clone());
        new_connection
    }

    pub(crate) async fn drop(&self, id: &WebSocketId) {
        let mut map = self.0.write().await;
        if let Some(connection) = map.remove(id) {
            // Clean up pollers when the connection ends
            for (_, poller) in connection.pollers.write().await.drain() {
                poller.stop();
            }
        }
    }
}

impl Default for Connections {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents an internal WebSocket connection.
/// Designed for efficient cloning, as all contained fields are inexpensive to clone.
#[derive(Clone)]
pub struct Connection {
    // Unique WebSocket connection ID
    pub id: WebSocketId,
    // Manages the WebSocket protocol state
    pub protocol_init_state: Arc<RwLock<protocol::ConnectionInitState>>,
    // Shared connection context
    pub context: Context,
    // Channel for sending messages over the WebSocket
    pub send_channel: Sender<Message>,
    // Active pollers associated with operations. A web socket connection can have multiple active subscriptions.
    pub pollers: Arc<RwLock<HashMap<protocol::OperationId, poller::Poller>>>,
}

/// A representation of an active WebSocket connection.
/// This can be used for monitoring purposes.
#[derive(Serialize)]
pub struct ActiveConnection {
    /// The connection ID.
    pub connection_id: WebSocketId,
    /// The project ID associated with the connection.
    pub project_id: Option<ProjectId>,
    /// A list of active subscriptions associated with the connection.
    pub active_subscriptions: Vec<protocol::OperationId>,
}

impl Connection {
    /// Creates a new WebSocket connection with the given context and message sender channel.
    /// To actually create a WebSocket connection, use the `Connections::new_connection` method.
    pub fn new(id: WebSocketId, context: Context, channel: Sender<Message>) -> Self {
        Self {
            id,
            protocol_init_state: Arc::new(RwLock::new(
                protocol::ConnectionInitState::NotInitialized,
            )), // Initial protocol state
            context,                                        // Shared connection context
            send_channel: channel, // Channel for sending messages over the WebSocket
            pollers: Arc::new(RwLock::new(HashMap::new())), // A map of active pollers
        }
    }

    pub(crate) async fn to_active_connection(&self) -> ActiveConnection {
        let pollers = self.pollers.read().await;
        ActiveConnection {
            connection_id: self.id.clone(),
            project_id: self.context.project_id.clone(),
            active_subscriptions: pollers.keys().cloned().collect(),
        }
    }

    /// Sends a message over the WebSocket.
    /// If the receiver has dropped the channel, the message will not be sent.
    pub async fn send(&self, message: Message) {
        // TODO: Handle the error case when the receiver channel is dropped
        let _ = self.send_channel.send(message).await;
    }

    /// Stops the poller associated with the given operation ID, if it exists.
    /// Exposed to outside of crate for test purposes
    pub(crate) async fn stop_poller(&self, key: &protocol::OperationId) {
        let mut map = self.pollers.write().await;
        if let Some(poller) = map.remove(key) {
            poller.stop(); // Stop the poller before removing it
        }
    }

    /// Checks if a poller exists for the given operation ID.
    pub(crate) async fn poller_exists(&self, key: &protocol::OperationId) -> bool {
        let map = self.pollers.read().await;
        map.contains_key(key)
    }

    /// Inserts a new poller associated with the given operation ID.
    pub(crate) async fn insert_poller(&self, key: protocol::OperationId, poller: poller::Poller) {
        let mut map = self.pollers.write().await;
        map.insert(key, poller);
    }
}

/// Represents a message that can be sent over a WebSocket connection.
pub enum Message {
    /// Represents a raw WebSocket message.
    Raw(ws::Message),
    /// Represents a message using the protocol server format.
    Protocol(Box<protocol::ServerMessage>),
}

impl Message {
    /// Creates a close message with the given code and reason.
    fn close_message(code: u16, reason: impl Into<Cow<'static, str>>) -> Message {
        Message::Raw(close_ws_message(code, reason))
    }

    /// Returns a message indicating a forbidden request (4403).
    pub fn forbidden() -> Self {
        Self::close_message(4403, "Forbidden")
    }

    /// Returns a message indicating connection is not authorized through `connection_init`
    pub fn unauthorized() -> Self {
        Self::close_message(4401, "Unauthorized")
    }

    /// Returns a message indicating too many initialization requests (4429).
    pub fn too_many_init_requests() -> Self {
        Self::close_message(4429, "Too many initialization requests")
    }

    /// Returns a message indicating that a subscriber already exists for the given operation ID (4409).
    pub fn subscriber_already_exists(operation_id: &protocol::OperationId) -> Self {
        let message = format!("Subscriber for {} already exists", operation_id.0);
        Self::close_message(4409, message)
    }

    /// Returns a message indicating that the connection initialization timed out (4408).
    pub fn conn_init_timeout() -> Self {
        Self::close_message(4408, "Connection initialization timeout")
    }

    /// Returns a message indicating an invalid message format (4400).
    pub fn invalid_message_format(message: String) -> Self {
        Self::close_message(4400, message)
    }

    /// Returns a message indicating an internal server error (1011).
    pub fn internal_server_error() -> Self {
        Self::Raw(internal_server_message_ws())
    }

    /// Force re-connect with a message
    pub fn force_reconnect(message: &'static str) -> Self {
        Self::close_message(1012, message)
    }
}

/// Creates a close WebSocket message with the specified code and reason.
fn close_ws_message(code: u16, reason: impl Into<Cow<'static, str>>) -> ws::Message {
    ws::Message::Close(Some(ws::CloseFrame {
        code,
        reason: reason.into(),
    }))
}

/// Returns a WebSocket message indicating an internal server error (1011).
pub fn internal_server_message_ws() -> ws::Message {
    close_ws_message(1011, "Internal Server Error")
}
