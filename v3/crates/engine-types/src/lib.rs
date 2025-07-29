use serde::Serialize;

/// Context for making HTTP requests
#[derive(Debug, Clone)]
pub struct HttpContext {
    /// The HTTP client to use for making requests
    pub client: reqwest::Client,
    /// Response size limit for NDC requests
    pub ndc_response_size_limit: Option<usize>,
}

#[derive(Clone, serde::Serialize, Debug)]
pub struct ProjectId(pub String);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ExposeInternalErrors {
    Expose,
    Censor,
}

/// Errors that can be raised by middleware
#[derive(Debug)]
pub struct MiddlewareError {
    /// The HTTP status code to return
    pub status: axum::http::StatusCode,
    /// The error message
    pub message: String,
    /// Is the error an internal error?
    pub is_internal: bool,
}

impl MiddlewareError {
    pub fn to_error_type(&self) -> ErrorType {
        if self.is_internal {
            ErrorType::Internal
        } else {
            ErrorType::User
        }
    }
}

/// A wrapper around a middleware state that also contains an error converter
#[derive(Clone)]
pub struct WithMiddlewareErrorConverter<S> {
    /// The state
    pub state: S,
    /// Function pointer to convert a middleware error to a HTTP response
    /// Clone is cheap because a function pointer is a fixed size single point address.
    middleware_error_converter: fn(MiddlewareError) -> axum::response::Response,
}

impl<S> WithMiddlewareErrorConverter<S> {
    pub fn new(
        state: S,
        middleware_error_converter: fn(MiddlewareError) -> axum::response::Response,
    ) -> Self {
        Self {
            state,
            middleware_error_converter,
        }
    }

    pub fn handle_error(&self, error: MiddlewareError) -> axum::response::Response {
        (self.middleware_error_converter)(error)
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum ErrorType {
    // error caused by user actions, such as syntax error or expired JWT token
    User,
    // internal errors that we count in our alerting. this should not include 5xx errors from downstream services
    Internal,
    // errors caused by a user not having permissions to do something
    Permission,
    // some unsupported feature
    Unsupported,
    // error parsing SQL
    Parse,
    // connector error
    Connector,
}
