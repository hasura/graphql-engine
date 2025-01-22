use std::str::FromStr;

use axum::{
    http::{HeaderMap, HeaderName, StatusCode},
    response::IntoResponse,
};
use http_body_util::BodyExt;
use regex::Regex;
use serde::Serialize;

use open_dds::plugins::{
    LifecyclePreRoutePluginHook, LifecyclePreRoutePluginHookConfigRequestMethod,
    LifecyclePreRoutePluginHookIncomingHTTPMethod,
};
use serde_json::json;
use tracing_util::{
    set_attribute_on_active_span, ErrorVisibility, SpanVisibility, Traceable, TraceableError,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error while making the HTTP request to the pre-parse plugin {0} - {1}")]
    ErrorWhileMakingHTTPRequestToTheHook(String, reqwest::Error),
    #[error("Error while building the request for the pre-parse plugin {0} - {1}")]
    BuildRequestError(String, String),
    #[error("Reqwest error: {0}")]
    ReqwestError(reqwest::Error),
    #[error("Unexpected status code: {0}")]
    UnexpectedStatusCode(u16),
    #[error("Error parsing the request: {0}")]
    PluginRequestParseError(serde_json::error::Error),
    #[error("HTTP method {0} not supported")]
    UnsupportedHTTPMethod(String),
    #[error("Invalid header name {0}")]
    InvalidHeaderName(String),
    #[error("Invalid header value {0}")]
    InvalidHeaderValue(String),
    #[error("Not found")]
    NotFound,
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        let mut response_builder = axum::response::Response::builder();
        let status = match self {
            Error::UnexpectedStatusCode(_) => StatusCode::INTERNAL_SERVER_ERROR,
            Error::NotFound => StatusCode::NOT_FOUND,
            _ => StatusCode::BAD_REQUEST,
        };
        response_builder = response_builder.status(status);
        let body = match self {
            Error::NotFound => axum::body::Body::empty(),
            _ => axum::body::Body::from(json!({"error": self.to_string()}).to_string()),
        };
        response_builder.body(body).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum ErrorResponse {
    UserError(serde_json::Value),
    InternalError(Option<serde_json::Value>),
}

impl IntoResponse for ErrorResponse {
    fn into_response(self) -> axum::response::Response {
        // User errors are 400 (BAD_REQUEST) and internal errors are 500 (INTERNAL_SERVER_ERROR)
        let status = match self {
            ErrorResponse::UserError(_) => StatusCode::BAD_REQUEST,
            ErrorResponse::InternalError(_) => StatusCode::INTERNAL_SERVER_ERROR,
        };
        // Get the error body, we won't include the error body for internal errors.
        let error_body = match self {
            ErrorResponse::UserError(error) | ErrorResponse::InternalError(Some(error)) => {
                json!({"error": error})
            }
            ErrorResponse::InternalError(None) => json!({"error": "internal error"}),
        };
        // Create the response body having the error body as the response body.
        let body = match serde_json::to_vec(&error_body) {
            Ok(body) => axum::body::Body::from(body),
            // This should never happen, but we'll handle it just in case by returning the error as the response body.
            Err(_err) => axum::body::Body::from(
                json!({"error": "there was an error serializing the error"}).to_string(),
            ),
        };
        axum::response::Response::builder()
            .status(status)
            .header("content-type", "application/json")
            .body(body)
            .unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum PreRoutePluginResponse {
    Return(Vec<u8>, HeaderMap),
    ReturnError {
        plugin_name: String,
        error: ErrorResponse,
    },
}

impl std::fmt::Display for ErrorResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            ErrorResponse::UserError(error) | ErrorResponse::InternalError(Some(error)) => {
                error.to_string()
            }
            ErrorResponse::InternalError(None) => String::new(),
        };
        write!(f, "{message}")
    }
}

impl TraceableError for ErrorResponse {
    fn visibility(&self) -> ErrorVisibility {
        match &self {
            ErrorResponse::UserError(_) => ErrorVisibility::User,
            ErrorResponse::InternalError(_) => ErrorVisibility::Internal,
        }
    }
}

impl Traceable for PreRoutePluginResponse {
    type ErrorType<'a> = ErrorResponse;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        match self {
            PreRoutePluginResponse::ReturnError {
                plugin_name: _,
                error,
            } => Some(error.clone()),
            PreRoutePluginResponse::Return(..) => None,
        }
    }
}

impl IntoResponse for PreRoutePluginResponse {
    fn into_response(self) -> axum::response::Response {
        match self {
            PreRoutePluginResponse::Return(body, headers) => {
                let mut response_builder = axum::response::Response::builder();
                for (key, value) in headers {
                    if let Some(key) = key {
                        response_builder = response_builder.header(key, value);
                    }
                }
                response_builder
                    .status(StatusCode::OK)
                    .body(axum::body::Body::from(body))
                    .unwrap()
            }
            PreRoutePluginResponse::ReturnError {
                plugin_name: _,
                error,
            } => error.into_response(),
        }
    }
}

/// String type alias for the HTTP method. We could have used `reqwest::Method` directly, but it doesn't implement
/// `Serialize` traits, which is required for serializing the request body.
pub type HTTPMethod = String;

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PreRouteRequestBody {
    pub path: Option<String>,
    pub method: Option<HTTPMethod>,
    pub query: Option<String>,
    pub body: Option<serde_json::Value>,
}

impl PreRouteRequestBody {
    pub fn empty() -> Self {
        Self {
            path: None,
            method: None,
            query: None,
            body: None,
        }
    }
}

async fn build_request(
    http_client: &reqwest::Client,
    config: &LifecyclePreRoutePluginHook,
    client_headers: &HeaderMap,
    incoming_request_path: &str,
    incoming_request_method: &reqwest::Method,
    incoming_request_query: Option<String>,
    request: axum::http::Request<axum::body::Body>,
) -> Result<reqwest::RequestBuilder, String> {
    let mut request_builder = match config.config.request.method {
        LifecyclePreRoutePluginHookConfigRequestMethod::GET => http_client.get(&config.url.value),
        LifecyclePreRoutePluginHookConfigRequestMethod::POST => http_client.post(&config.url.value),
    };
    let mut pre_plugin_headers = tracing_util::get_trace_headers();
    if let Some(header_config) = config.config.request.headers.as_ref() {
        let mut headers = HeaderMap::new();
        if let Some(additional_headers) = &header_config.additional {
            for (key, value) in &additional_headers.0 {
                let header_name =
                    HeaderName::from_str(key).map_err(|_| format!("Invalid header name {key}"))?;
                let header_value = value
                    .value
                    .parse()
                    .map_err(|_| format!("Invalid value for the header {key}"))?;
                headers.insert(header_name, header_value);
            }
        }
        for header in &header_config.forward {
            if let Some(header_value) = client_headers.get(header) {
                let header_name = HeaderName::from_str(header)
                    .map_err(|_| format!("Invalid header name {header}"))?;
                headers.insert(header_name, header_value.clone());
            }
        }
        pre_plugin_headers.extend(headers);
    }
    request_builder = request_builder.headers(pre_plugin_headers);

    let mut request_body = PreRouteRequestBody::empty();
    if config.config.request.raw_request.method.is_some() {
        request_body.method = Some(incoming_request_method.as_str().to_string());
    }
    if config.config.request.raw_request.path.is_some() {
        request_body.path = Some(incoming_request_path.to_string());
    }
    if config.config.request.raw_request.query.is_some() {
        request_body.query = incoming_request_query;
    }
    if config.config.request.raw_request.body.is_some() {
        let body = request.into_body();
        let bytes = body
            .collect()
            .await
            .map_err(|err| err.to_string())?
            .to_bytes();
        request_body.body = match serde_json::from_slice::<serde_json::Value>(&bytes) {
            Ok(body) => Ok(Some(body)),
            Err(err) => match err.classify() {
                // If the request body is empty, we don't set the body in the request body.
                serde_json::error::Category::Eof => Ok(None),
                _ => Err(err.to_string()),
            },
        }?;
    }
    request_builder = request_builder.json(&request_body);
    Ok(request_builder)
}

pub async fn execute_plugin(
    http_client: &reqwest::Client,
    plugin: &LifecyclePreRoutePluginHook,
    headers_map: &HeaderMap,
    incoming_request_path: &str,
    incoming_request_method: &reqwest::Method,
    incoming_request_query: Option<String>,
    request: axum::http::Request<axum::body::Body>,
) -> Result<PreRoutePluginResponse, Error> {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let http_request_builder = build_request(
                        http_client,
                        plugin,
                        headers_map,
                        incoming_request_path,
                        incoming_request_method,
                        incoming_request_query,
                        request,
                    )
                    .await
                    .map_err(|e| Error::BuildRequestError(plugin.name.clone(), e))?;
                    let req = http_request_builder.build().map_err(Error::ReqwestError)?;
                    http_client.execute(req).await.map_err(|e| {
                        Error::ErrorWhileMakingHTTPRequestToTheHook(plugin.name.clone(), e)
                    })
                })
            },
        )
        .await?;
    match response.status() {
        reqwest::StatusCode::OK => {
            let response_headers = response.headers().clone();
            let body = response.bytes().await.map_err(Error::ReqwestError)?;
            let mut headers = HeaderMap::new();
            if let Some(response_config) = &plugin.config.response {
                if let Some(header_config) = &response_config.headers {
                    if let Some(additional_headers) = &header_config.additional {
                        for (key, value) in &additional_headers.0 {
                            let header_name = HeaderName::from_str(key)
                                .map_err(|_| Error::InvalidHeaderName(key.clone()))?;
                            let header_value = value
                                .value
                                .parse()
                                .map_err(|_| Error::InvalidHeaderValue(value.value.clone()))?;
                            headers.insert(header_name, header_value);
                        }
                    }
                    for header in &header_config.forward {
                        if let Some(header_value) = response_headers.get(header) {
                            let header_name = HeaderName::from_str(header)
                                .map_err(|_| Error::InvalidHeaderName(header.to_string()))?;
                            headers.insert(header_name, header_value.clone());
                        }
                    }
                }
            }
            Ok(PreRoutePluginResponse::Return(body.to_vec(), headers))
        }
        reqwest::StatusCode::INTERNAL_SERVER_ERROR => {
            let body = response.json().await.map_err(Error::ReqwestError)?;
            Ok(PreRoutePluginResponse::ReturnError {
                plugin_name: plugin.name.clone(),
                error: ErrorResponse::InternalError(Some(body)),
            })
        }
        reqwest::StatusCode::BAD_REQUEST => {
            let body = response.json().await.map_err(Error::ReqwestError)?;
            Ok(PreRoutePluginResponse::ReturnError {
                plugin_name: plugin.name.clone(),
                error: ErrorResponse::UserError(body),
            })
        }
        _ => Err(Error::UnexpectedStatusCode(response.status().as_u16())),
    }
}

/// Execute the pre-route plugins.
/// This function iterates over the pre-route plugins and executes the first plugin that matches the incoming request
/// path.
/// If a plugin is found, it is executed and the response is returned. If no plugin is found, a `NotFound` error is
/// returned.
/// If a plugin is found but there is an error while executing it, the error is returned.
pub async fn pre_route_handler(
    method: reqwest::Method,
    uri: axum::http::Uri,
    headers_map: axum::http::header::HeaderMap,
    pre_route_plugins: &Vec<LifecyclePreRoutePluginHook>,
    http_client: &reqwest::Client,
    raw_query: Option<String>,
    request: axum::http::Request<axum::body::Body>,
) -> Result<PreRoutePluginResponse, Error> {
    let tracer = tracing_util::global_tracer();
    for plugin_config in pre_route_plugins {
        let re = Regex::new(&plugin_config.config.match_path).unwrap();
        // Check if the request path matches the plugin's regex and if the request method is in the plugin's list of methods.
        let request_method = match method {
            reqwest::Method::GET => Ok(LifecyclePreRoutePluginHookIncomingHTTPMethod::GET),
            reqwest::Method::POST => Ok(LifecyclePreRoutePluginHookIncomingHTTPMethod::POST),
            reqwest::Method::PUT => Ok(LifecyclePreRoutePluginHookIncomingHTTPMethod::PUT),
            reqwest::Method::DELETE => Ok(LifecyclePreRoutePluginHookIncomingHTTPMethod::DELETE),
            reqwest::Method::PATCH => Ok(LifecyclePreRoutePluginHookIncomingHTTPMethod::PATCH),
            _ => Err(Error::UnsupportedHTTPMethod(method.to_string())),
        }?;
        if re.is_match(uri.path()) && plugin_config.config.match_methods.contains(&request_method) {
            let plugin_response = tracer
                .in_span_async(
                    "execute_pre_route_plugin",
                    "Execute a Pre-route Plugin",
                    SpanVisibility::User,
                    || {
                        Box::pin(async {
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.name",
                                plugin_config.name.clone(),
                            );

                            execute_plugin(
                                http_client,
                                plugin_config,
                                &headers_map,
                                uri.path(),
                                &method,
                                raw_query.clone(),
                                request,
                            )
                            .await
                        })
                    },
                )
                .await?; // Short Circuit; stop executing remaining plugins if current one errors.
            return Ok(plugin_response);
        }
    }
    Err(Error::NotFound)
}
