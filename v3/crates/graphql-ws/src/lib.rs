pub(crate) mod metrics;
pub(crate) mod poller;
pub(crate) mod protocol;
pub(crate) mod websocket;

pub use metrics::{NoOpWebSocketMetrics, WebSocketMetrics};
pub use protocol::types::OperationId;
pub use websocket::{
    types::{ConnectionExpiry, Context, WebSocketId},
    WebSocketServer,
};

// For tests
pub use protocol::{
    subscribe::{execute_query_internal, send_request_error},
    types::ServerMessage,
    GRAPHQL_WS_PROTOCOL,
};
pub use websocket::{
    types::{Connection, Connections, Message},
    SEC_WEBSOCKET_PROTOCOL,
};
