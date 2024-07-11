use std::{collections::HashMap, fmt::Display};

use axum::{
    body::HttpBody,
    http::{HeaderMap, Request, StatusCode},
    response::IntoResponse,
};
use serde::Serialize;

use crate::configuration::PrePluginConfig;
use hasura_authn_core::Session;
use lang_graphql::{ast::common as ast, http::RawRequest};
use tracing_util::{
    set_attribute_on_active_span, ErrorVisibility, SpanVisibility, Traceable, TraceableError,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error while making the HTTP request to the pre-execution plugin {0} - {1}")]
    ErrorWhileMakingHTTPRequestToTheHook(String, reqwest::Error),
    #[error("Reqwest error: {0}")]
    ReqwestError(reqwest::Error),
    #[error("Unexpected status code: {0}")]
    UnexpectedStatusCode(u16),
    #[error("plugin response parse error: {0}")]
    PluginResponseParseError(serde_json::error::Error),
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        lang_graphql::http::Response::error_message_with_status(
            StatusCode::INTERNAL_SERVER_ERROR,
            self.to_string(),
        )
        .into_response()
    }
}

#[derive(Debug, Clone)]
pub enum ErrorResponse {
    UserError(Vec<u8>),
    InternalError(Option<Vec<u8>>),
}

impl std::fmt::Display for ErrorResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            ErrorResponse::UserError(error) | ErrorResponse::InternalError(Some(error)) => {
                let error = serde_json::from_slice::<serde_json::Value>(error)
                    .map_err(|_| std::fmt::Error)?;
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
            PreExecutePluginResponse::ReturnError(err) => Some(err.clone()),
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
    ReturnError(ErrorResponse),
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct RawRequestBody {
    pub query: Option<String>,
    pub variables: Option<HashMap<ast::Name, serde_json::Value>>,
    pub operation_name: Option<ast::Name>,
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PreExecutePluginRequestBody {
    pub session: Option<Session>,
    pub raw_request: RawRequestBody,
}

fn build_request(
    http_client: &reqwest::Client,
    config: &PrePluginConfig,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
) -> reqwest::RequestBuilder {
    let mut pre_plugin_headers = tracing_util::get_trace_headers();
    if config.request.headers {
        pre_plugin_headers.extend(client_headers.clone());
    }
    let mut request_builder = http_client
        .post(config.url.clone())
        .headers(pre_plugin_headers);
    let mut request_body = PreExecutePluginRequestBody {
        session: None,
        raw_request: RawRequestBody {
            query: None,
            variables: None,
            operation_name: raw_request.operation_name.clone(),
        },
    };
    if config.request.session {
        request_body.session = Some(session.clone());
    };
    if config.request.raw_request.query {
        request_body.raw_request.query = Some(raw_request.query.clone());
    };
    if config.request.raw_request.variables {
        request_body
            .raw_request
            .variables
            .clone_from(&raw_request.variables);
    };
    request_builder = request_builder.json(&request_body);
    request_builder
}

pub async fn execute_plugin(
    http_client: &reqwest::Client,
    config: &PrePluginConfig,
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
                    let http_request_builder =
                        build_request(http_client, config, client_headers, session, raw_request);
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
            let body = response.bytes().await.map_err(Error::ReqwestError)?;

            Ok(PreExecutePluginResponse::ReturnError(
                ErrorResponse::InternalError(Some(body.to_vec())),
            ))
        }
        StatusCode::BAD_REQUEST => {
            let body = response.bytes().await.map_err(Error::ReqwestError)?;
            Ok(PreExecutePluginResponse::ReturnError(
                ErrorResponse::UserError(body.to_vec()),
            ))
        }
        _ => Err(Error::UnexpectedStatusCode(response.status().as_u16())),
    }
}

pub async fn pre_execution_plugins_handler<'a, B>(
    pre_execution_plugins_config: &Vec<PrePluginConfig>,
    http_client: &reqwest::Client,
    session: Session,
    request: Request<B>,
    headers_map: HeaderMap,
) -> axum::response::Result<(Request<axum::body::Body>, Option<axum::response::Response>)>
where
    B: HttpBody,
    B::Error: Display,
{
    let (parts, body) = request.into_parts();
    let bytes = body
        .collect()
        .await
        .map_err(|err| {
            (reqwest::StatusCode::INTERNAL_SERVER_ERROR, err.to_string()).into_response()
        })?
        .to_bytes();
    let tracer = tracing_util::global_tracer();
    let mut response = None;
    let raw_request =
        serde_json::from_slice::<RawRequest>(&bytes).map_err(Error::PluginResponseParseError)?;
    for pre_plugin_config in pre_execution_plugins_config {
        let plugin_response = tracer
            .in_span_async(
                "pre_execution_plugin_middleware",
                "Pre-execution Plugin middleware",
                SpanVisibility::Internal,
                || {
                    Box::pin(async {
                        set_attribute_on_active_span(
                            tracing_util::AttributeVisibility::Default,
                            "plugin.name",
                            pre_plugin_config.name.clone(),
                        );
                        let plugin_response = execute_plugin(
                            http_client,
                            pre_plugin_config,
                            &headers_map,
                            &session,
                            &raw_request,
                        )
                        .await;
                        if let Ok(PreExecutePluginResponse::ReturnError(
                            ErrorResponse::InternalError(error_value),
                        )) = &plugin_response
                        {
                            let error_value = serde_json::from_slice::<serde_json::Value>(
                                error_value.as_ref().unwrap_or(&vec![]),
                            )
                            .map_err(Error::PluginResponseParseError)?;
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.internal_error",
                                error_value.to_string(),
                            );
                        };
                        if let Ok(PreExecutePluginResponse::ReturnError(
                            ErrorResponse::UserError(error_value),
                        )) = &plugin_response
                        {
                            let error_value =
                                serde_json::from_slice::<serde_json::Value>(error_value)
                                    .map_err(Error::PluginResponseParseError)?;
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
            .await?;
        match plugin_response {
            PreExecutePluginResponse::Return(value) => {
                response = Some(value.into_response());
                break;
            }
            PreExecutePluginResponse::Continue => (),
            PreExecutePluginResponse::ReturnError(ErrorResponse::UserError(error_value)) => {
                let error_value = serde_json::from_slice::<serde_json::Value>(&error_value)
                    .map_err(Error::PluginResponseParseError)?;
                let user_error_response =
                    lang_graphql::http::Response::error_message_with_status_and_details(
                        reqwest::StatusCode::BAD_REQUEST,
                        format!(
                            "User error in pre-execution plugin {0}",
                            pre_plugin_config.name
                        ),
                        error_value,
                    )
                    .into_response();
                response = Some(user_error_response);
                break;
            }
            PreExecutePluginResponse::ReturnError(ErrorResponse::InternalError(_error_value)) => {
                let internal_error_response =
                    lang_graphql::http::Response::error_message_with_status(
                        reqwest::StatusCode::INTERNAL_SERVER_ERROR,
                        format!(
                            "Internal error in pre-execution plugin {0}",
                            pre_plugin_config.name
                        ),
                    )
                    .into_response();
                response = Some(internal_error_response);
                break;
            }
        };
    }
    Ok((
        Request::from_parts(parts, axum::body::Body::from(bytes)),
        response,
    ))
}
