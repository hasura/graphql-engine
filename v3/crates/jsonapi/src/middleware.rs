use axum::{extract::State, http::Request, middleware::Next, response::IntoResponse};
use axum_core::body::Body;
use tracing_util::{SpanVisibility, TraceableHttpResponse};

use crate::endpoint::EndPoint;
use crate::types::JsonApiHttpError;

/// Middleware to start tracing of the JSON:API request. This middleware
/// must be active for the entire duration of the request i.e. this middleware
/// should be the entry point and the exit point of the JSON:API request.
pub async fn jsonapi_request_tracing_middleware(
    State(endpoint): State<EndPoint>,
    request: Request<Body>,
    next: Next,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = endpoint.as_str();
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

/// Utility to build any server state with middleware error converter for JSON:API
pub fn build_state_with_middleware_error_converter<S>(
    state: S,
) -> engine_types::WithMiddlewareErrorConverter<S> {
    engine_types::WithMiddlewareErrorConverter::new(state, |error| {
        JsonApiHttpError::from_middleware_error(error).into_response()
    })
}
