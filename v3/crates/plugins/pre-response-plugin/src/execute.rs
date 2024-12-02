use std::{collections::HashMap, str::FromStr};

use axum::{
    http::{HeaderMap, HeaderName, StatusCode},
    response::IntoResponse,
};

use hasura_authn_core::Session;
use lang_graphql::{ast::common as ast, http::RawRequest};
use open_dds::plugins::LifecyclePreResponsePluginHook;
use serde::Serialize;
use tracing_util::{set_attribute_on_active_span, ErrorVisibility, SpanVisibility, TraceableError};

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

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct RawRequestBody {
    pub query: Option<String>,
    pub variables: Option<HashMap<ast::Name, serde_json::Value>>,
    pub operation_name: Option<ast::Name>,
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PreResponsePluginRequestBody {
    pub session: Option<Session>,
    pub raw_request: RawRequestBody,
    pub response: serde_json::Value,
}

fn build_request(
    http_client: &reqwest::Client,
    config: &LifecyclePreResponsePluginHook,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
    response: &serde_json::Value,
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
    let mut request_builder = http_client
        .post(config.url.value.clone())
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
    if config.config.request.session.is_some() {
        request_body.session = Some(session.clone());
    };
    if config.config.request.raw_request.query.is_some() {
        request_body.raw_request.query = Some(raw_request.query.clone());
    };
    if config.config.request.raw_request.variables.is_some() {
        request_body
            .raw_request
            .variables
            .clone_from(&raw_request.variables);
    };
    request_builder = request_builder.json(&request_body);
    Ok(request_builder)
}

pub async fn execute_plugin(
    http_client: &reqwest::Client,
    config: &LifecyclePreResponsePluginHook,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
    response: &serde_json::Value,
) -> Result<String, Error> {
    let tracer = tracing_util::global_tracer();
    let _response = tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    let http_request_builder = build_request(
                        http_client,
                        config,
                        client_headers,
                        session,
                        raw_request,
                        response,
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
    Ok("Done".to_string())
}

pub fn pre_response_plugins_handler(
    pre_response_plugins_config: &nonempty::NonEmpty<LifecyclePreResponsePluginHook>,
    http_client: &reqwest::Client,
    session: Session,
    raw_request_bytes: &axum::body::Bytes,
    raw_response_bytes: &axum::body::Bytes,
    headers_map: HeaderMap,
) -> Result<(), Error> {
    let raw_request = serde_json::from_slice::<RawRequest>(raw_request_bytes)
        .map_err(Error::PluginRequestParseError)?;
    let raw_response = serde_json::from_slice::<serde_json::Value>(raw_response_bytes)
        .map_err(Error::EngineResponseParseError)?;
    // Execute the pre-response plugins in a separate task
    execute_pre_response_plugins_in_task(
        pre_response_plugins_config.clone(),
        http_client.clone(),
        session,
        raw_request,
        raw_response,
        headers_map,
        ExecutePluginsTracing::ParentContext, // Use parent context for pre-response plugin tracing
    );
    Ok(())
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

/// Execute the pre-response plugins in a separate task
pub fn execute_pre_response_plugins_in_task(
    pre_response_plugins_config: nonempty::NonEmpty<LifecyclePreResponsePluginHook>,
    http_client: reqwest::Client,
    session: Session,
    raw_request: RawRequest,
    raw_response: serde_json::Value,
    headers_map: HeaderMap,
    tracing_strategy: ExecutePluginsTracing,
) {
    let tracer = tracing_util::global_tracer();
    // Spawn a new task to execute the pre-response plugins
    tokio::spawn(async move {
        // Execute all pre-response plugins
        let span_name = "execute_all_pre_response_plugins";
        let span_display_name = "Execute all Pre-response Plugins".to_string();
        let span_visibility = SpanVisibility::User;
        // See comments on `ExecutePluginsTracing` for more details on the tracing strategy.
        match tracing_strategy {
            ExecutePluginsTracing::ParentContext => {
                tracer
                    .in_span_async_with_parent_context(
                        span_name,
                        span_display_name,
                        span_visibility,
                        &headers_map,
                        || {
                            Box::pin(async {
                                execute_all_plugins(
                                    &pre_response_plugins_config,
                                    &http_client,
                                    &session,
                                    &raw_request,
                                    &raw_response,
                                    &headers_map,
                                )
                                .await;
                                tracing_util::Successful::new(())
                            })
                        },
                    )
                    .await;
            }
            ExecutePluginsTracing::NewTraceWithLink { span_link } => {
                tracer
                    .new_trace_async_with_link(
                        span_name,
                        span_display_name,
                        span_visibility,
                        span_link,
                        || {
                            Box::pin(async {
                                execute_all_plugins(
                                    &pre_response_plugins_config,
                                    &http_client,
                                    &session,
                                    &raw_request,
                                    &raw_response,
                                    &headers_map,
                                )
                                .await;
                                tracing_util::Successful::new(())
                            })
                        },
                    )
                    .await;
            }
        }
    });
}

/// Execute all pre-response plugins
async fn execute_all_plugins(
    pre_response_plugins_config: &nonempty::NonEmpty<LifecyclePreResponsePluginHook>,
    http_client: &reqwest::Client,
    session: &Session,
    raw_request: &RawRequest,
    raw_response: &serde_json::Value,
    headers_map: &HeaderMap,
) {
    let tracer = tracing_util::global_tracer();
    let mut async_executions = Vec::with_capacity(pre_response_plugins_config.capacity());
    // Execute each pre-response plugin asynchronously without await.
    for pre_plugin_config in pre_response_plugins_config {
        let async_execution = async {
            (tracer
                .in_span_async(
                    "pre_response_plugin_execute",
                    "Pre-response Plugin execute",
                    SpanVisibility::User,
                    || {
                        Box::pin(async {
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.name",
                                pre_plugin_config.name.clone(),
                            );
                            execute_plugin(
                                http_client,
                                pre_plugin_config,
                                headers_map,
                                session,
                                raw_request,
                                raw_response,
                            )
                            .await
                        })
                    },
                )
                .await,)
        };
        // Collect the async execution future
        async_executions.push(async_execution);
    }
    // Wait for all the async executions to complete
    let _plugin_responses = futures_util::future::join_all(async_executions).await;
}
