use axum::{http::Request, middleware::Next};
use axum_core::body::Body;
use tracing_util::{SpanVisibility, TraceableHttpResponse};

/// Middleware to start tracing of the `/v1/rest` request. This middleware
/// must be active for the entire duration of the request i.e. this middleware
/// should be the entry point and the exit point of the JSON:API request.
pub async fn rest_request_tracing_middleware(
    request: Request<Body>,
    next: Next,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = "/v1/rest";
    tracer
        .in_span_async_with_parent_context(
            path,
            path,
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                Box::pin(async move {
                    let response = next.run(request).await;
                    TraceableHttpResponse::new(response, path)
                })
            },
        )
        .await
        .response
}
