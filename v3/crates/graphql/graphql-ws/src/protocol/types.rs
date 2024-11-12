use axum::http;
use hasura_authn_core as hasura_auth;
use nonempty::NonEmpty;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A unique identifier for a GraphQL operation.
/// Sent by the client with the `subscribe` message and used by the server to identify the operation.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub struct OperationId(pub String);

/// Messages that the client can send to the server
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ClientMessage {
    /// The client sends this message to the server to initiate the connection.
    /// First message sent by the client over an established WebSocket.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#connectioninit>
    #[serde(rename = "connection_init")]
    ConnectionInit { payload: Option<InitPayload> },

    /// The client sends this message to the server to execute a GraphQL operation.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#subscribe>
    #[serde(rename = "subscribe")]
    Subscribe {
        id: OperationId,
        payload: lang_graphql::http::RawRequest,
    },

    /// The client sends this message to the server to stop a running GraphQL operation.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#complete>
    #[serde(rename = "complete")]
    Complete { id: OperationId },

    /// The client sends this message to the server to detect failed connections.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#ping>
    #[serde(rename = "ping")]
    Ping,

    /// A response to a `ping` message.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#pong>
    #[serde(rename = "pong")]
    Pong,
}

impl ClientMessage {
    /// Returns message type
    pub fn message_type(&self) -> &'static str {
        match self {
            Self::ConnectionInit { .. } => "connection_init",
            Self::Subscribe { .. } => "subscribe",
            Self::Complete { .. } => "complete",
            Self::Ping => "ping",
            Self::Pong => "pong",
        }
    }
}

/// The payload of the `connection_init` message.
#[derive(Debug, Serialize, Deserialize)]
pub struct InitPayload {
    /// The headers to be sent for initiating connection.
    #[serde(default)]
    pub headers: HashMap<String, String>,
}

/// Messages that the server can send to the client
#[derive(Serialize)]
#[serde(tag = "type")]
pub enum ServerMessage {
    /// The server sends this message to the client to acknowledge the connection.
    /// Response to the `connection_init` message.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#connectionack>
    #[serde(rename = "connection_ack")]
    ConnectionAck,

    /// Server sends the result of a requested operation.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#next>
    #[serde(rename = "next")]
    Next {
        id: OperationId,
        payload: lang_graphql::http::Response,
    },

    /// Server sends errors resulting from a requested operation.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#error>
    #[serde(rename = "error")]
    Error {
        id: OperationId,
        payload: NonEmpty<lang_graphql::http::GraphQLError>,
    },

    /// Server sends this message to the client to indicate that the operation has been completed.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#complete>
    #[serde(rename = "complete")]
    Complete { id: OperationId },

    /// Server sends this message to the client to avoid connection idle.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#ping>
    #[serde(rename = "ping")]
    Ping { payload: Option<serde_json::Value> },

    /// Server sends a response to a `ping` message.
    /// ref: <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#pong>
    #[serde(rename = "pong")]
    Pong,
}

impl ServerMessage {
    /// Returns the operation id if the message is a `Complete` or `Error` message.
    pub fn is_complete_or_error(&self) -> Option<&OperationId> {
        match self {
            Self::Error { id, .. } | Self::Complete { id } => Some(id),
            _ => None,
        }
    }
}

/// The state of the 'graphql-transport-ws' protocol initialization.
/// Updated when the server receives the 'connection_init' message.
#[derive(Debug)]
pub enum ConnectionInitState {
    /// The connection has not been initialized. Default state.
    NotInitialized,
    /// The connection has been initialized.
    /// Contains the session and headers to be used for subsequent operations.
    Initialized {
        session: hasura_auth::Session,
        headers: http::HeaderMap,
    },
}
