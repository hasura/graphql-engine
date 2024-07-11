use std::collections::HashMap;
use std::env;
use std::net;

use axum::{http::header::HeaderMap, response::Json, routing::post, Router};
use serde_json::Value;
use tower_http::trace::TraceLayer;
use tracing::debug;

const DEFAULT_PORT: u16 = 3050;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    if let Ok(otlp_endpoint) = env::var("OTLP_ENDPOINT") {
        tracing_util::initialize_tracing(Some(&otlp_endpoint), env!("CARGO_PKG_NAME"), None)?;
    }

    let app = Router::new()
        .route("/validate-request", post(validate_request))
        .layer(axum::middleware::from_fn(
            graphql_request_tracing_middleware,
        ))
        .layer(TraceLayer::new_for_http());

    let host = net::IpAddr::V4(net::Ipv4Addr::UNSPECIFIED);
    let port = env::var("PORT")
        .map(|str| str.parse())
        .unwrap_or(Ok(DEFAULT_PORT))?;
    let address = (host, port).into();

    let server = axum::Server::bind(&address).serve(app.into_make_service());
    println!(
        "Dev webhook authentication listening at {}",
        server.local_addr()
    );
    server
        .with_graceful_shutdown(async {
            // wait for a SIGINT, i.e. a Ctrl+C from the keyboard
            let sigint = async {
                tokio::signal::ctrl_c()
                    .await
                    .expect("failed to install signal handler")
            };
            // wait for a SIGTERM, i.e. a normal `kill` command
            #[cfg(unix)]
            let sigterm = async {
                tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
                    .expect("failed to install signal handler")
                    .recv()
                    .await
            };
            // block until either of the above happens
            #[cfg(unix)]
            tokio::select! {
                _ = sigint => (),
                _ = sigterm => (),
            }
            #[cfg(windows)]
            tokio::select! {
                _ = sigint => (),
            }
        })
        .await?;
    Ok(())
}

async fn validate_request(
    headers: HeaderMap,
    Json(payload): Json<HashMap<String, HashMap<String, String>>>,
) -> Json<Value> {
    debug!(
        headers = format!("{:?}", headers),
        body = format!("{:?}", payload),
        "receiving request"
    );

    Json(serde_json::to_value(payload.get("headers").unwrap()).unwrap())
}

async fn graphql_request_tracing_middleware<B: Send>(
    request: http::Request<B>,
    next: axum::middleware::Next<B>,
) -> axum::response::Result<axum::response::Response> {
    use tracing_util::*;
    let traceable = global_tracer()
        .in_span_async_with_parent_context(
            "request",
            "request",
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                Box::pin(async move {
                    let mut response = next.run(request).await;
                    TraceContextResponsePropagator::new()
                        .inject(&mut HeaderInjector(response.headers_mut()));
                    TraceableHttpResponse::new(response, "")
                })
            },
        )
        .await;
    Ok(traceable.response)
}
