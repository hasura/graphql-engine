use std::{collections::BTreeMap, str::FromStr};

use axum::{
    http::{HeaderMap, HeaderName, StatusCode},
    response::IntoResponse,
};
use reqwest::header::HeaderValue;
use serde::Serialize;

use hasura_authn_core::Session;
use lang_graphql::{ast::common as ast, http::RawRequest};
use open_dds::plugins::LifecyclePreParsePluginHook;
use tracing_util::{
    ErrorVisibility, SpanVisibility, Traceable, TraceableError, set_attribute_on_active_span,
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
}

impl Error {
    pub fn is_internal(&self) -> bool {
        match self {
            Error::ErrorWhileMakingHTTPRequestToTheHook(_, _) | Error::UnexpectedStatusCode(_) => {
                false
            }
            Error::BuildRequestError(_, _)
            | Error::ReqwestError(_)
            | Error::PluginRequestParseError(_) => true,
        }
    }

    pub fn into_graphql_error(self) -> lang_graphql::http::GraphQLError {
        lang_graphql::http::GraphQLError {
            message: self.to_string(),
            path: None,
            extensions: None,
            is_internal: self.is_internal(),
        }
    }

    pub fn into_middleware_error(self) -> engine_types::MiddlewareError {
        engine_types::MiddlewareError {
            status: StatusCode::INTERNAL_SERVER_ERROR,
            message: self.to_string(),
            is_internal: self.is_internal(),
        }
    }
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

#[derive(Debug, Clone)]
pub enum ErrorResponse {
    UserError(serde_json::Value),
    InternalError(Option<serde_json::Value>),
}

impl ErrorResponse {
    pub fn into_graphql_error(self, plugin_name: &str) -> lang_graphql::http::GraphQLError {
        match self {
            Self::UserError(error) => lang_graphql::http::GraphQLError {
                message: format!("User error in pre-parse plugin: {plugin_name}"),
                path: None,
                extensions: Some(lang_graphql::http::Extensions { details: error }),
                is_internal: false,
            },
            Self::InternalError(_error) => lang_graphql::http::GraphQLError {
                message: format!("Internal error in pre-parse plugin: {plugin_name}"),
                path: None,
                extensions: None,
                is_internal: false,
            },
        }
    }
    pub fn to_status_code(&self) -> StatusCode {
        match self {
            Self::UserError(_) => StatusCode::BAD_REQUEST,
            Self::InternalError(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
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

impl Traceable for PreExecutePluginResponse {
    type ErrorType<'a> = ErrorResponse;

    fn get_error(&self) -> Option<ErrorResponse> {
        match self {
            PreExecutePluginResponse::Continue | PreExecutePluginResponse::Return(_) => None,
            PreExecutePluginResponse::ReturnError {
                plugin_name: _,
                error,
            } => Some(error.clone()),
        }
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

#[derive(Debug, Clone)]
pub enum PreExecutePluginResponse {
    Return(Vec<u8>),
    Continue,
    ReturnError {
        plugin_name: String,
        error: ErrorResponse,
    },
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct RawRequestBody {
    pub query: Option<String>,
    pub variables: Option<BTreeMap<ast::Name, serde_json::Value>>,
    pub operation_name: Option<ast::Name>,
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PreExecutePluginRequestBody {
    pub session: Option<Session>,
    pub raw_request: RawRequestBody,
}

fn build_request(
    client_address: std::net::SocketAddr,
    http_client: &reqwest::Client,
    config: &LifecyclePreParsePluginHook,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
) -> Result<reqwest::RequestBuilder, String> {
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

    if let Ok(header_value) = HeaderValue::from_str(&client_address.ip().to_string()) {
        pre_plugin_headers.insert("X-Forwarded-For", header_value);
    }

    let mut request_builder = http_client
        .post(config.url.value.clone())
        .headers(pre_plugin_headers);
    let mut request_body = PreExecutePluginRequestBody {
        session: None,
        raw_request: RawRequestBody {
            query: None,
            variables: None,
            operation_name: raw_request.operation_name.clone(),
        },
    };
    if config.config.request.session.is_some() {
        request_body.session = Some(session.clone());
    }
    if config.config.request.raw_request.query.is_some() {
        request_body.raw_request.query = Some(raw_request.query.clone());
    }
    if config.config.request.raw_request.variables.is_some() {
        request_body
            .raw_request
            .variables
            .clone_from(&raw_request.variables);
    }
    request_builder = request_builder.json(&request_body);
    Ok(request_builder)
}

pub async fn execute_plugin(
    client_address: std::net::SocketAddr,
    http_client: &reqwest::Client,
    config: &LifecyclePreParsePluginHook,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
) -> Result<PreExecutePluginResponse, Error> {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let http_request_builder = build_request(
                        client_address,
                        http_client,
                        config,
                        client_headers,
                        session,
                        raw_request,
                    )
                    .map_err(|err| Error::BuildRequestError(config.name.clone(), err))?;
                    let req = http_request_builder.build().map_err(Error::ReqwestError)?;
                    http_client.execute(req).await.map_err(|e| {
                        Error::ErrorWhileMakingHTTPRequestToTheHook(config.name.clone(), e)
                    })
                })
            },
        )
        .await?;
    match response.status() {
        StatusCode::NO_CONTENT => Ok(PreExecutePluginResponse::Continue),
        StatusCode::OK => {
            let body = response.bytes().await.map_err(Error::ReqwestError)?;
            Ok(PreExecutePluginResponse::Return(body.to_vec()))
        }
        StatusCode::INTERNAL_SERVER_ERROR => {
            let body = response.json().await.map_err(Error::ReqwestError)?;

            Ok(PreExecutePluginResponse::ReturnError {
                plugin_name: config.name.clone(),
                error: ErrorResponse::InternalError(Some(body)),
            })
        }
        StatusCode::BAD_REQUEST => {
            let response_json: serde_json::Value =
                response.json().await.map_err(Error::ReqwestError)?;
            Ok(PreExecutePluginResponse::ReturnError {
                plugin_name: config.name.clone(),
                error: ErrorResponse::UserError(response_json),
            })
        }
        _ => Err(Error::UnexpectedStatusCode(response.status().as_u16())),
    }
}

pub async fn pre_parse_plugins_handler(
    client_address: std::net::SocketAddr,
    pre_parse_plugins_config: &nonempty::NonEmpty<LifecyclePreParsePluginHook>,
    http_client: &reqwest::Client,
    session: Session,
    raw_request_bytes: &axum::body::Bytes,
    headers_map: HeaderMap,
) -> Result<Option<axum::response::Response>, Error> {
    let tracer = tracing_util::global_tracer();
    let mut response = None;
    let raw_request = serde_json::from_slice::<RawRequest>(raw_request_bytes)
        .map_err(Error::PluginRequestParseError)?;
    let result = tracer
        .in_span_async(
            "pre_parse_plugin_middleware",
            "Pre-execution Plugin middleware",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    execute_pre_parse_plugins(
                        client_address,
                        pre_parse_plugins_config,
                        http_client,
                        &session,
                        &headers_map,
                        &raw_request,
                    )
                    .await
                })
            },
        )
        .await?;

    match result {
        PreExecutePluginResponse::Return(value) => {
            let plugin_response = axum::response::Response::builder()
                .status(StatusCode::OK)
                .header("Content-Type", "application/json")
                .body(axum::body::Body::from(value))
                .unwrap();

            response = Some(plugin_response);
        }
        PreExecutePluginResponse::Continue => {}
        PreExecutePluginResponse::ReturnError { plugin_name, error } => {
            let status_code = error.to_status_code();
            let graphql_error = error.into_graphql_error(&plugin_name);
            let error_response =
                lang_graphql::http::Response::error_with_status(status_code, graphql_error)
                    .into_response();
            response = Some(error_response);
        }
    }
    Ok(response)
}

/// Execute all the pre-parse plugins in sequence.
/// If any plugin returns an error, the execution stops and the error is returned.
pub async fn execute_pre_parse_plugins(
    client_address: std::net::SocketAddr,
    pre_parse_plugins_config: &nonempty::NonEmpty<LifecyclePreParsePluginHook>,
    http_client: &reqwest::Client,
    session: &Session,
    headers_map: &HeaderMap,
    raw_request: &RawRequest,
) -> Result<PreExecutePluginResponse, Error> {
    let tracer = tracing_util::global_tracer();
    for plugin_config in pre_parse_plugins_config {
        let plugin_response = tracer
            .in_span_async(
                "execute_pre_parse_plugin",
                "Execute a Pre-parse Plugin",
                SpanVisibility::User,
                || {
                    Box::pin(async {
                        set_attribute_on_active_span(
                            tracing_util::AttributeVisibility::Default,
                            "plugin.name",
                            plugin_config.name.clone(),
                        );
                        let plugin_response = execute_plugin(
                            client_address,
                            http_client,
                            plugin_config,
                            headers_map,
                            session,
                            raw_request,
                        )
                        .await;
                        if let Ok(PreExecutePluginResponse::ReturnError {
                            plugin_name: _,
                            error: ErrorResponse::InternalError(error_value),
                        }) = &plugin_response
                        {
                            let error_value =
                                error_value.as_ref().unwrap_or(&serde_json::Value::Null);
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.internal_error",
                                error_value.to_string(),
                            );
                        }
                        if let Ok(PreExecutePluginResponse::ReturnError {
                            plugin_name: _,
                            error: ErrorResponse::UserError(error_value),
                        }) = &plugin_response
                        {
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.user_error",
                                error_value.to_string(),
                            );
                        }
                        plugin_response
                    })
                },
            )
            .await?; // Short Circuit; stop executing remaining plugins if current one errors.
        match plugin_response {
            PreExecutePluginResponse::Continue => (),
            // Stop executing the next plugin if current plugin returns an error or a response
            response @ (PreExecutePluginResponse::Return(_)
            | PreExecutePluginResponse::ReturnError {
                plugin_name: _,
                error: _,
            }) => return Ok(response),
        }
    }
    Ok(PreExecutePluginResponse::Continue)
}
