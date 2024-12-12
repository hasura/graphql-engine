use crate::WebSocketId;

/// Trait defining metrics tracking behavior for WebSocket connections.
pub trait WebSocketMetrics: Clone + Send + Sync + 'static {
    /// Records the initialization of a WebSocket connection.
    fn record_connection_init(&self);

    /// Records the termination of a WebSocket connection.
    fn record_connection_drop(&self);

    /// Records the start of a poller associated with a specific WebSocket.
    fn record_poller_start(&self, websocket_id: &WebSocketId);

    /// Records the stop of a poller associated with a specific WebSocket.
    fn record_poller_stop(&self, websocket_id: &WebSocketId);
}

/// A no-operation implementation of `WebSocketMetrics`
#[derive(Clone)]
pub struct NoOpWebSocketMetrics;

// Does nothing for all metric recording methods.
impl WebSocketMetrics for NoOpWebSocketMetrics {
    fn record_connection_init(&self) {}
    fn record_connection_drop(&self) {}
    fn record_poller_start(&self, _websocket_id: &WebSocketId) {}
    fn record_poller_stop(&self, _websocket_id: &WebSocketId) {}
}
