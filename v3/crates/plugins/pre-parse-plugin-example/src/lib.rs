use axum::{
    body::Body,
    extract::Request,
    middleware,
    routing::{get, post},
    Json, Router,
};
use pre_parse_plugin::execute::PreParsePluginRequestBody;
use tracing::info;

// Pass through handler: returns 204 No Content to let engine continue unchanged
pub async fn pass_through(_: Json<PreParsePluginRequestBody>) -> axum::http::StatusCode {
    axum::http::StatusCode::NO_CONTENT
}

pub fn router() -> Router {
    Router::new()
        .route("/health", get(|| async { "OK" }))
        // default endpoint: pass-through
        .route("/", post(pass_through))
        .layer(middleware::from_fn(log_request))
}

// This function logs incoming requests
async fn log_request(req: Request<Body>, next: axum::middleware::Next) -> axum::response::Response {
    info!(method = %req.method(), uri = %req.uri(), "Incoming pre-parse-plugin request");
    next.run(req).await
}
