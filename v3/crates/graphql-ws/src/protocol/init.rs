use axum::http;
use execute::HttpContext;
use hasura_authn::{authenticate, AuthConfig, AuthError};
use hasura_authn_core::{authorize_identity, Session, SessionError};
use std::collections::HashMap;

use super::types::{ConnectionInitState, InitPayload, ServerMessage};
use crate::websocket::types as ws;

/// Handles the connection initialization message from the client.
/// This function authenticates, authorizes, and initializes the WebSocket connection.
pub async fn handle_connection_init(connection: ws::Connection, payload: Option<InitPayload>) {
    let context = &connection.context;
    let mut state = connection.protocol_init_state.write().await;

    match *state {
        // If the connection is not yet initialized, proceed with initialization
        ConnectionInitState::NotInitialized => {
            match initialize(payload, &context.http_context, &context.auth_config).await {
                Ok((session, headers)) => {
                    // Update state to Initialized and send a connection acknowledgment
                    *state = ConnectionInitState::Initialized { session, headers };
                    connection
                        .send(ws::Message::Protocol(ServerMessage::ConnectionAck))
                        .await;
                }
                Err(_e) => {
                    // Initialization failed, send a forbidden message
                    // TODO: Handle error here
                    connection.send(ws::Message::forbidden()).await;
                }
            }
        }
        // If already initialized, send an error for too many initialization requests
        ConnectionInitState::Initialized { .. } => {
            connection.send(ws::Message::too_many_init_requests()).await;
        }
    }
}

/// Performs the initialization process by validating the payload, authenticating, and authorizing.
/// It returns a session and the headers if the initialization is successful.
async fn initialize(
    payload: Option<InitPayload>,
    http_context: &HttpContext,
    auth_config: &AuthConfig,
) -> Result<(Session, http::HeaderMap), ConnectionInitError> {
    // Parse the headers from the payload
    let headers = match payload {
        Some(payload) => parse_headers(payload.headers)?,
        None => http::HeaderMap::new(),
    };
    // Authenticate the client based on headers and context
    let identity = authenticate(&headers, &http_context.client, auth_config).await?;
    // Authorize the authenticated identity
    let session = authorize_identity(&identity, &headers)?;
    Ok((session, headers))
}

/// Error types that may occur during connection initialization.
#[derive(Debug, thiserror::Error)]
pub enum ConnectionInitError {
    #[error("Invalid header name: {0}")]
    InvalidHeaderName(#[from] http::header::InvalidHeaderName),
    #[error("Invalid header value: {0}")]
    InvalidHeaderValue(#[from] http::header::InvalidHeaderValue),
    #[error("AuthError: {0}")]
    Authn(#[from] AuthError),
    #[error("SessionError: {0}")]
    Session(#[from] SessionError),
}

/// Parses headers from a given map of strings into an `http::HeaderMap`.
/// Returns a parsed header map or an error if the headers are invalid.
fn parse_headers(map: HashMap<String, String>) -> Result<http::HeaderMap, ConnectionInitError> {
    let mut headers = http::HeaderMap::new();
    for (key, value) in map {
        let header_name = http::HeaderName::from_bytes(key.as_bytes())?;
        let header_value = http::HeaderValue::from_str(&value)?;
        headers.insert(header_name, header_value);
    }
    Ok(headers)
}
