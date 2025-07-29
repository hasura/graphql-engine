use std::{collections::BTreeMap, str::FromStr};

use axum::{
    http::{HeaderMap, HeaderName, StatusCode},
    response::IntoResponse,
};

use hasura_authn_core::Session;
use lang_graphql::{ast::common as ast, http::RawRequest};
use open_dds::plugins::{LifecyclePluginUrl, LifecyclePreResponsePluginHookConfigRequest};
use reqwest::header::HeaderValue;
use serde::Serialize;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error while making the HTTP request to the pre-parse plugin {0} - {1}")]
    ErrorWhileMakingHTTPRequestToTheHook(String, reqwest::Error),
    #[error("Error while building the request for the pre-parse plugin {0} - {1}")]
    BuildRequestError(String, String),
    #[error("Reqwest error: {0}")]
    ReqwestError(reqwest::Error),
    #[error("Error parsing the request: {0}")]
    PluginRequestParseError(serde_json::Error),
    #[error("Error parsing the engine response: {0}")]
    EngineResponseParseError(serde_json::Error),
    #[error("Unexpected status code: {0}")]
    UnexpectedStatusCode(u16),
    #[error("Error serializing the modified response: {0}")]
    ResponseSerializationError(serde_json::Error),
    #[error("Error while preparing the response: {0}")]
    ResponsePreparationError(axum::http::Error),
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

impl Error {
    pub fn to_graphql_response(self) -> lang_graphql::http::Response {
        let is_internal = match &self {
            Error::ErrorWhileMakingHTTPRequestToTheHook(_, _)
            | Error::UnexpectedStatusCode(_)
            | Error::ResponseSerializationError(_)
            | Error::ResponsePreparationError(_) => false,
            Error::BuildRequestError(_, _)
            | Error::ReqwestError(_)
            | Error::PluginRequestParseError(_)
            | Error::EngineResponseParseError(_) => true,
        };
        lang_graphql::http::Response::error_message_with_status(
            StatusCode::INTERNAL_SERVER_ERROR,
            self.to_string(),
            is_internal,
        )
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        self.to_graphql_response().into_response()
    }
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
pub struct PreResponsePluginRequestBody {
    pub session: Option<Session>,
    pub raw_request: RawRequestBody,
    pub response: serde_json::Value,
}

/// Tracing strategy for executing the plugins, accommodating different GraphQL transport methods
pub enum ExecutePluginsTracing {
    /// Execute plugins in a span with parent context derived from client headers
    ///
    /// Used for GraphQL over HTTP, where plugin spans are stitched to the HTTP trace
    /// using a parent context derived from the client headers. This allows for
    /// a continuous trace from the client request through the plugin execution.
    ParentContext,

    /// Execute plugins in a new trace with a link to the parent span
    ///
    /// Used for GraphQL over websockets, where plugin spans are wrapped in new traces
    /// with a link to the parent span. This strategy helps maintain trace continuity
    /// in long-lived websocket connections while allowing for independent tracing
    /// of individual operations.
    NewTraceWithLink {
        /// The parent span to which this new trace will be linked
        span_link: tracing_util::SpanLink,
    },
}

pub(crate) fn build_request(
    client_address: std::net::SocketAddr,
    http_client: &reqwest::Client,
    request_config: &LifecyclePreResponsePluginHookConfigRequest,
    plugin_url: &LifecyclePluginUrl,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
    response: &serde_json::Value,
) -> Result<reqwest::RequestBuilder, String> {
    let mut pre_plugin_headers = tracing_util::get_trace_headers();

    if let Some(header_config) = request_config.headers.as_ref() {
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
        .post(plugin_url.value.clone())
        .headers(pre_plugin_headers);
    let mut request_body = PreResponsePluginRequestBody {
        session: None,
        raw_request: RawRequestBody {
            query: None,
            variables: None,
            operation_name: raw_request.operation_name.clone(),
        },
        response: response.clone(),
    };
    if request_config.session.is_some() {
        request_body.session = Some(session.clone());
    }
    if request_config.raw_request.query.is_some() {
        request_body.raw_request.query = Some(raw_request.query.clone());
    }
    if request_config.raw_request.variables.is_some() {
        request_body
            .raw_request
            .variables
            .clone_from(&raw_request.variables);
    }
    request_builder = request_builder.json(&request_body);
    Ok(request_builder)
}
