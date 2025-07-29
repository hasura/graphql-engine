use std::borrow::Cow;

use axum::http::HeaderMap;
use hasura_authn_core::Session;
use lang_graphql::http::RawRequest;
use metadata_resolve::ResolvedLifecyclePreResponseSyncPluginHook;
use open_dds::plugins::OnPluginFailure;
use reqwest::StatusCode;
use tracing_util::{SpanVisibility, set_attribute_on_active_span};

use crate::execute::common::{Error, build_request};

pub enum ProcessedPreResponsePluginResponse {
    Continue,
    Response(axum::response::Response),
}

pub enum ErrorResponse {
    UserError(serde_json::Value),
    InternalError(Option<serde_json::Value>),
}

impl ErrorResponse {
    pub fn into_graphql_error(self, plugin_name: &str) -> lang_graphql::http::GraphQLError {
        match self {
            Self::UserError(error) => lang_graphql::http::GraphQLError {
                message: format!("User error in pre-response plugin: {plugin_name}"),
                path: None,
                extensions: Some(lang_graphql::http::Extensions { details: error }),
                is_internal: false,
            },
            Self::InternalError(_error) => lang_graphql::http::GraphQLError {
                message: format!("Internal error in pre-response plugin: {plugin_name}"),
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

pub enum RawPreResponsePluginResponse {
    Continue,
    ContinueWithResponse(serde_json::Value),
    ReturnError {
        plugin_name: String,
        error: ErrorResponse,
    },
}

pub enum PreResponsePluginResponse {
    Continue,
    ReturnResponse(Vec<u8>),
    ReturnError {
        plugin_name: String,
        error: ErrorResponse,
    },
}

pub async fn execute_sync_pre_response_plugins(
    client_address: std::net::SocketAddr,
    pre_response_plugins_config: &nonempty::NonEmpty<ResolvedLifecyclePreResponseSyncPluginHook>,
    http_client: &reqwest::Client,
    session: &Session,
    raw_request: RawRequest,
    raw_response: serde_json::Value,
    headers_map: &HeaderMap,
) -> Result<PreResponsePluginResponse, Error> {
    let tracer = tracing_util::global_tracer();
    let mut response = Cow::Borrowed(&raw_response);
    for plugin_config in pre_response_plugins_config {
        let plugin_response = tracer
            .in_span_async(
                "execute_sync_pre_response_plugin",
                "Execute a Synchronous Pre-response Plugin",
                SpanVisibility::User,
                || {
                    Box::pin(async {
                        set_attribute_on_active_span(
                            tracing_util::AttributeVisibility::Default,
                            "plugin.name",
                            plugin_config.name.clone(),
                        );
                        execute_plugin(
                            client_address,
                            http_client,
                            plugin_config,
                            headers_map,
                            session,
                            &raw_request,
                            &response,
                        )
                        .await
                    })
                },
            )
            .await?;
        match plugin_response {
            RawPreResponsePluginResponse::Continue => {}
            RawPreResponsePluginResponse::ContinueWithResponse(new_response) => {
                response = Cow::Owned(new_response);
            }
            RawPreResponsePluginResponse::ReturnError { plugin_name, error } => {
                if plugin_config.config.on_plugin_failure == OnPluginFailure::Continue {
                    return Ok(PreResponsePluginResponse::ReturnError { plugin_name, error });
                }
            }
        }
    }
    match response {
        Cow::Borrowed(_) => Ok(PreResponsePluginResponse::Continue),
        Cow::Owned(response) => {
            let response =
                serde_json::to_vec(&response).map_err(Error::ResponseSerializationError)?;
            Ok(PreResponsePluginResponse::ReturnResponse(response))
        }
    }
}

pub async fn execute_plugin(
    client_address: std::net::SocketAddr,
    http_client: &reqwest::Client,
    config: &ResolvedLifecyclePreResponseSyncPluginHook,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
    response: &serde_json::Value,
) -> Result<RawPreResponsePluginResponse, Error> {
    let http_request_builder = build_request(
        client_address,
        http_client,
        &config.config.request,
        &config.url,
        client_headers,
        session,
        raw_request,
        response,
    )
    .map_err(|err| Error::BuildRequestError(config.name.clone(), err))?;
    let req = http_request_builder.build().map_err(Error::ReqwestError)?;
    let response = http_client
        .execute(req)
        .await
        .map_err(|e| Error::ErrorWhileMakingHTTPRequestToTheHook(config.name.clone(), e))?;
    match response.status() {
        StatusCode::NO_CONTENT => Ok(RawPreResponsePluginResponse::Continue),
        StatusCode::OK => {
            let body = response.json().await.map_err(Error::ReqwestError)?;
            Ok(RawPreResponsePluginResponse::ContinueWithResponse(body))
        }
        StatusCode::BAD_REQUEST => {
            let body = response.json().await.map_err(Error::ReqwestError)?;
            Ok(RawPreResponsePluginResponse::ReturnError {
                plugin_name: config.name.clone(),
                error: ErrorResponse::UserError(body),
            })
        }
        StatusCode::INTERNAL_SERVER_ERROR => {
            let body = response.json().await.map_err(Error::ReqwestError)?;
            Ok(RawPreResponsePluginResponse::ReturnError {
                plugin_name: config.name.clone(),
                error: ErrorResponse::InternalError(Some(body)),
            })
        }
        _ => Err(Error::UnexpectedStatusCode(response.status().as_u16())),
    }
}
