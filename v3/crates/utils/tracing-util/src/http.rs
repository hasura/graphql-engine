//! Traceable HTTP response
//!
//! This module contains a wrapper around [`http::Response<T>`] that implements [`Traceable`].
//!
//! # Example:
//! ```
//! use tracing_util::{SpanVisibility, TraceableHttpResponse};
//! use axum::{body::Body, http::Request, middleware::Next};
//!
//! async fn graphql_request_tracing_middleware(
//!     request: Request<Body>,
//!     next: Next,
//! ) -> axum::response::Result<axum::response::Response> {
//!     let tracer = tracing_util::global_tracer();
//!     let path = "/graphql";
//!
//!     Ok(tracer
//!        .in_span_async(path, path.to_string(), SpanVisibility::User, || {
//!            Box::pin(async move {
//!                let response = next.run(request).await;
//!                TraceableHttpResponse::new(response, path)
//!            })
//!        })
//!        .await
//!        .response)
//! }
//! ```

use crate::traceable::{ErrorVisibility, Traceable, TraceableError};
use std::borrow::Cow;

/// Wrapper around `http::Response<T>` that is traceable in spans.
///
/// # Example:
/// ```
/// use axum::response::{Response, IntoResponse};
/// let response: Response = "hello world!".into_response();
/// tracing_util::TraceableHttpResponse::new(response, "/create_user");
/// ```
pub struct TraceableHttpResponse<T, N: Into<Cow<'static, str>>> {
    /// The HTTP response.
    pub response: http::Response<T>,
    /// Path of the request that generated this response.
    pub path: N,
}

impl<T, N: Into<Cow<'static, str>>> TraceableHttpResponse<T, N> {
    /// Creates a new `TraceableHttpResponse`.
    pub fn new(response: http::Response<T>, path: N) -> Self {
        Self { response, path }
    }
}

/// Error type for `TraceableHttpResponse`.
/// Only used as an associated type when implementing [`Traceable`] trait for [`TraceableHttpResponse`].
#[derive(Debug, derive_more::Display)]
#[display("{error}")]
pub struct ResponseError {
    error: String,
}

impl TraceableError for ResponseError {
    fn visibility(&self) -> ErrorVisibility {
        // Errors in HTTP responses are always visible to the user.
        ErrorVisibility::User
    }
}

/// Implement `Traceable` for `TraceableHttpResponse` so that it can be used in spans.
impl<T, N: Into<Cow<'static, str>> + derive_more::Display> Traceable
    for TraceableHttpResponse<T, N>
{
    type ErrorType<'a>
        = ResponseError
    where
        T: 'a,
        N: 'a;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        // If the response status is either client or server error, return an error.
        let response_status = self.response.status();
        if response_status.is_client_error() || response_status.is_server_error() {
            Some(ResponseError {
                error: format!(
                    "HTTP request to {} failed with status {}",
                    self.path, response_status,
                ),
            })
        } else {
            None
        }
    }
}
