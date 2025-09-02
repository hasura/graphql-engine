use std::net::SocketAddr;
use tracing::{info, Level};

#[tokio::main]
async fn main() {
    // Init tracing with sensible defaults
    tracing_subscriber::fmt().with_max_level(Level::INFO).init();

    let app = pre_parse_plugin_example::router();

    // Bind address can be overridden via PORT env var (common in cloud runtimes)
    let port = std::env::var("PORT")
        .ok()
        .and_then(|p| p.parse::<u16>().ok())
        .unwrap_or(5003);
    let addr = SocketAddr::from(([0, 0, 0, 0], port));

    info!(%addr, "Starting pre-parse sample plugin on");

    axum::serve(
        tokio::net::TcpListener::bind(addr).await.expect("bind"),
        app,
    )
    .await
    .expect("server error");
}
