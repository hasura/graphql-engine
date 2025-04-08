use std::collections::HashMap;
use std::env;
use std::net;

use axum::{Router, http::header::HeaderMap, response::Json, routing::post};
use axum_core::body::Body;
use clap::Parser;
use serde::Serialize;
use serde_json::Value;
use tower_http::trace::TraceLayer;
use tracing::debug;

const DEFAULT_PORT: u16 = 3050;

#[derive(Parser, Serialize)]
struct ServerOptions {
    /// The OpenTelemetry collector endpoint.
    #[arg(long, value_name = "URL", env = "OTLP_ENDPOINT")]
    otlp_endpoint: Option<String>,

    /// Log traces to stdout.
    #[arg(long, env = "EXPORT_TRACES_STDOUT")]
    export_traces_stdout: bool,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let server_options = ServerOptions::parse();
    if let Some(otlp_endpoint) = server_options.otlp_endpoint {
        let export_traces_stdout = if server_options.export_traces_stdout {
            tracing_util::ExportTracesStdout::Enable
        } else {
            tracing_util::ExportTracesStdout::Disable
        };

        tracing_util::initialize_tracing(
            Some(&otlp_endpoint),
            env!("CARGO_PKG_NAME").to_string(),
            None,
            tracing_util::PropagateBaggage::Enable,
            export_traces_stdout,
        )?;
    }

    let app = Router::new()
        .route("/validate-request", post(validate_request))
        .layer(axum::middleware::from_fn(
            graphql_request_tracing_middleware,
        ))
        .layer(TraceLayer::new_for_http());

    let host = net::IpAddr::V6(net::Ipv6Addr::UNSPECIFIED);
    let port = env::var("PORT")
        .map(|str| str.parse())
        .unwrap_or(Ok(DEFAULT_PORT))?;
    let address = (host, port);
    let listener = tokio::net::TcpListener::bind(address).await.unwrap();

    let server = axum::serve(
        listener,
        app.into_make_service_with_connect_info::<net::SocketAddr>(),
    );
    server
        .with_graceful_shutdown(axum_ext::shutdown_signal())
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

async fn graphql_request_tracing_middleware(
    request: http::Request<Body>,
    next: axum::middleware::Next,
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

                    get_text_map_propagator(|propagator| {
                        propagator.inject(&mut HeaderInjector(response.headers_mut()))
                    });
                    TraceableHttpResponse::new(response, "")
                })
            },
        )
        .await;
    Ok(traceable.response)
}
