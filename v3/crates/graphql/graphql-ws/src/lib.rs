pub(crate) mod metrics;
pub(crate) mod poller;
pub(crate) mod protocol;
pub(crate) mod websocket;

pub use metrics::{NoOpWebSocketMetrics, WebSocketMetrics};
pub use protocol::types::OperationId;
pub use websocket::{
    WebSocketServer,
    types::{ConnectionExpiry, Context, WebSocketId},
};

// For tests
pub use protocol::{
    GRAPHQL_WS_PROTOCOL,
    subscribe::{execute_query_internal, send_request_error},
    types::ServerMessage,
};
pub use websocket::{
    SEC_WEBSOCKET_PROTOCOL,
    types::{Connection, Connections, Message},
};
