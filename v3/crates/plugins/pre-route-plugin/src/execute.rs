use std::str::FromStr;

use axum::{
    http::{HeaderMap, HeaderName, StatusCode},
    response::IntoResponse,
};
use regex::Regex;
use serde::Serialize;

use open_dds::plugins::{LifecyclePreRoutePluginHook, RequestMethod};
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
        let status = match self {
            ErrorResponse::UserError(_) => StatusCode::BAD_REQUEST,
            ErrorResponse::InternalError(_) => StatusCode::INTERNAL_SERVER_ERROR,
        };
        axum::response::Response::builder()
            .status(status)
            .body(axum::body::Body::from(
                json!({"error": self.to_string()}).to_string(),
            ))
            .unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum PreRoutePluginResponse {
    Return(Vec<u8>),
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
            PreRoutePluginResponse::Return(_) => None,
        }
    }
}

impl IntoResponse for PreRoutePluginResponse {
    fn into_response(self) -> axum::response::Response {
        match self {
            PreRoutePluginResponse::Return(body) => axum::response::Response::builder()
                .status(StatusCode::OK)
                .body(axum::body::Body::from(body))
                .unwrap(),
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
}

impl PreRouteRequestBody {
    pub fn empty() -> Self {
        Self {
            path: None,
            method: None,
            query: None,
        }
    }
}

fn build_request(
    http_client: &reqwest::Client,
    config: &LifecyclePreRoutePluginHook,
    client_headers: &HeaderMap,
    incoming_request_path: &str,
    incoming_request_method: &reqwest::Method,
    incoming_request_query: Option<String>,
) -> Result<reqwest::RequestBuilder, String> {
    let mut request_builder = match config.config.request.method {
        RequestMethod::GET => http_client.get(&config.url.value),
        RequestMethod::POST => http_client.post(&config.url.value),
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
                    )
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
            let body = response.bytes().await.map_err(Error::ReqwestError)?;
            Ok(PreRoutePluginResponse::Return(body.to_vec()))
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
) -> Result<PreRoutePluginResponse, Error> {
    let tracer = tracing_util::global_tracer();
    for plugin_config in pre_route_plugins {
        let re = Regex::new(&plugin_config.config.match_path).unwrap();
        if re.is_match(uri.path()) {
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
