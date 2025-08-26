use std::net::SocketAddr;
use tracing::{info, Level};

#[tokio::main]
async fn main() {
    // Init tracing with sensible defaults
    tracing_subscriber::fmt().with_max_level(Level::INFO).init();

    let app = pre_ndc_response_plugin_example::router();

    // Bind address can be overridden via PORT env var
    let port = std::env::var("PORT")
        .ok()
        .and_then(|p| p.parse::<u16>().ok())
        .unwrap_or(5002);
    let addr = SocketAddr::from(([0, 0, 0, 0], port));

    info!(%addr, "Starting pre-ndc-response sample plugin on");

    axum::serve(
        tokio::net::TcpListener::bind(addr).await.expect("bind"),
        app,
    )
    .await
    .expect("server error");
}
