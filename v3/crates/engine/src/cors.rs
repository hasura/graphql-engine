use axum::http::header::HeaderName;
use reqwest::Method;
use std::time::Duration;
use tower_http::cors;

// Allow tracing headers to be retrievable on responses over CORS.
//
// Clippy has a false-positive with HeadeName and interior mutability
// (https://github.com/rust-lang/rust-clippy/issues/9776). This will get fixed
// when we upgrade Clippy.
#[allow(clippy::declare_interior_mutable_const)]
const TRACE_RESPONSE_HEADER_NAMES: [HeaderName; 7] = [
    // W3C headers
    HeaderName::from_static("traceresponse"),
    HeaderName::from_static("traceparent"),
    HeaderName::from_static("tracestate"),
    // Zipkin/B3 headers
    HeaderName::from_static("x-b3-traceid"),
    HeaderName::from_static("x-b3-spanid"),
    HeaderName::from_static("x-b3-parentspanid"),
    HeaderName::from_static("x-b3-sampled"),
];

/// Add CORS layer to the app.
pub fn build_cors_layer(cors_allow_origin: &[String]) -> cors::CorsLayer {
    let cors_allow_origin = if cors_allow_origin.is_empty() {
        // Allow all origins and mirror the request origin in 'Access-Control-Allow-Origin'
        cors::AllowOrigin::mirror_request()
    } else {
        let allowed_origins = cors_allow_origin.to_owned();
        cors::AllowOrigin::predicate(move |origin_header_value, _req| {
            allowed_origins.iter().any(|allowed_origin| {
                // The allowed origins can include leading whitespace characters when
                // provided as a comma-space-separated list.
                // Example: --cors-allow-origin = 'val1, val2, val3'
                origin_header_value == allowed_origin.trim()
            })
        })
    };
    cors::CorsLayer::new()
        .max_age(Duration::from_secs(24 * 60 * 60)) // 24 hours
        .allow_headers(cors::AllowHeaders::mirror_request())
        .allow_origin(cors_allow_origin)
        .allow_credentials(true)
        .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS])
        .expose_headers(TRACE_RESPONSE_HEADER_NAMES)
}

#[cfg(test)]
mod test {
    use axum::{
        Router,
        body::Body,
        http::{HeaderValue, Request},
    };
    use pretty_assertions::assert_eq;
    use reqwest::StatusCode;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_no_cors() {
        let app = Router::new();
        // Preflight CORS request
        let response = app
            .oneshot(
                Request::builder()
                    .uri("/")
                    .method("OPTIONS")
                    .header("Origin", "http://example.com")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn test_cors_allow_all_origins() {
        let app = Router::new().layer(super::build_cors_layer(&[]));
        // Preflight CORS request
        let response = app
            .oneshot(
                Request::builder()
                    .uri("/")
                    .method("OPTIONS")
                    .header("Origin", "http://example.com")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response.headers().get("access-control-allow-origin"),
            Some(&HeaderValue::from_static("http://example.com"))
        );
    }

    #[tokio::test]
    async fn test_cors_restrict_origin() {
        let app = Router::new().layer(super::build_cors_layer(&["http://example.com".to_string()]));
        // Preflight CORS request
        let response = app
            .oneshot(
                Request::builder()
                    .uri("/")
                    .method("OPTIONS")
                    .header("Origin", "http://localhost:8080") // Only http://example.com is allowed origin
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response.headers().get("access-control-allow-origin"), // Response shouldn't contain the allow origin header
            None
        );
    }

    #[tokio::test]
    async fn test_cors_allow_origin() {
        let app = Router::new().layer(super::build_cors_layer(&[
            "http://localhost:8080".to_string(),
            "http://example.com".to_string(),
        ]));
        // Preflight CORS request
        let response = app
            .oneshot(
                Request::builder()
                    .uri("/")
                    .method("OPTIONS")
                    .header("Origin", "http://example.com")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response.headers().get("access-control-allow-origin"),
            Some(&HeaderValue::from_static("http://example.com"))
        );
    }
}
