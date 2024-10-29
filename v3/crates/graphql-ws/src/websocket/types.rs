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

use crate::metrics::WebSocketMetrics;
use crate::poller;
use crate::protocol::types as protocol;

/// Context required to handle a WebSocket connection
#[derive(Clone)] // Cheap to clone as heavy fields are wrapped in `Arc`
pub struct Context<M> {
    pub connection_expiry: ConnectionExpiry,
    pub http_context: HttpContext,
    pub expose_internal_errors: ExposeInternalErrors,
    pub project_id: Option<ProjectId>,
    pub schema: Arc<lang_graphql::schema::Schema<graphql_schema::GDS>>,
    pub auth_config: Arc<AuthConfig>,
    pub plugin_configs: Arc<LifecyclePluginConfigs>,
    pub metrics: M,
}

/// Represents a WebSocket connection ID.
#[derive(Clone, Serialize, PartialEq, Eq, Hash, derive_more::Display)]
pub struct WebSocketId(SmolStr);

impl WebSocketId {
    /// Creates a new WebSocket connection ID.
    pub fn new() -> Self {
        Self(SmolStr::new(uuid::Uuid::new_v4().to_string()))
    }

    /// Returns the WebSocket connection ID as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Default for WebSocketId {
    fn default() -> Self {
        Self::new()
    }
}

/// Configures the expiry for a WebSocket connection
#[derive(Clone)]
pub enum ConnectionExpiry {
    Never,
    After(std::time::Duration),
}

/// A mutable and free clone-able collection of WebSocket connections.
#[derive(Clone)]
pub struct Connections<M>(pub Arc<RwLock<HashMap<WebSocketId, Connection<M>>>>);

impl<M> Connections<M> {
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(HashMap::new())))
    }

    pub(crate) async fn new_connection(
        &self,
        id: WebSocketId,
        context: Context<M>,
        channel: Sender<Message>,
    ) -> Connection<M>
    where
        M: WebSocketMetrics,
    {
        // Record this new connection in metrics
        context.metrics.record_connection_init();
        let new_connection = Connection::new(id, context, channel);
        let mut map = self.0.write().await;
        map.insert(new_connection.id.clone(), new_connection.clone());
        new_connection
    }

    pub(crate) async fn drop(&self, id: &WebSocketId)
    where
        M: WebSocketMetrics,
    {
        if let Some(connection) = self.remove_connection(id).await {
            // Record the connection drop in metrics
            connection.context.metrics.record_connection_drop();
            // Clean up pollers when the connection ends
            for operation_id in connection.pollers.read().await.keys() {
                connection.stop_poller(operation_id).await;
            }
        }
    }

    async fn remove_connection(&self, id: &WebSocketId) -> Option<Connection<M>> {
        let mut map = self.0.write().await;
        map.remove(id)
    }
}

impl<M> Default for Connections<M> {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents an internal WebSocket connection.
/// Designed for efficient cloning, as all contained fields are inexpensive to clone.
#[derive(Clone)]
pub struct Connection<M> {
    // Unique WebSocket connection ID
    pub id: WebSocketId,
    // Manages the WebSocket protocol state
    pub protocol_init_state: Arc<RwLock<protocol::ConnectionInitState>>,
    // Shared connection context
    pub context: Context<M>,
    // Channel for sending messages over the WebSocket
    pub send_channel: Sender<Message>,
    // Active pollers associated with operations. A web socket connection can have multiple active subscriptions.
    pub pollers: Arc<RwLock<HashMap<protocol::OperationId, poller::Poller>>>,
}

impl<M> Connection<M> {
    /// Creates a new WebSocket connection with the given context and message sender channel.
    /// To actually create a WebSocket connection, use the `Connections::new_connection` method.
    pub fn new(id: WebSocketId, context: Context<M>, channel: Sender<Message>) -> Self {
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

    /// Sends a message over the WebSocket.
    /// If the receiver has dropped the channel, the message will not be sent.
    pub async fn send(&self, message: Message) {
        // TODO: Handle the error case when the receiver channel is dropped
        let _ = self.send_channel.send(message).await;
    }

    /// Stops the poller associated with the given operation ID, if it exists.
    /// Exposed to outside of crate for test purposes
    pub(crate) async fn stop_poller(&self, key: &protocol::OperationId)
    where
        M: WebSocketMetrics,
    {
        if let Some(poller) = self.remove_poller(key).await {
            // Record the poller drop in the metrics.
            self.context.metrics.record_poller_stop(&self.id);
            poller.stop(); // Stop the poller before dropping it
        }
    }

    /// Internal method to remove a poller associated with the given operation ID.
    async fn remove_poller(&self, key: &protocol::OperationId) -> Option<poller::Poller> {
        let mut map = self.pollers.write().await;
        map.remove(key)
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

    /// Connection expired
    pub fn conn_expired() -> Self {
        // THe 1013 code is used to indicate "Try Again Later".
        // The session is expired and the client should try to reconnect.
        Self::close_message(1013, "WebSocket session expired")
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
