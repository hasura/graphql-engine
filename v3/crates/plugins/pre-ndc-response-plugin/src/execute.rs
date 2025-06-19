use axum::http::{HeaderMap, HeaderName};
use engine_types::HttpContext;
use hasura_authn_core::Session;
use metadata_resolve::{DataConnectorLink, Qualified, ResolvedLifecyclePreNdcResponsePluginHook};
use open_dds::data_connector::DataConnectorName;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use tracing_util::{ErrorVisibility, SpanVisibility, TraceableError, set_attribute_on_active_span};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error while making the HTTP request to the pre-response plugin {0} - {1}")]
    ErrorWhileMakingHTTPRequestToTheHook(String, reqwest::Error),
    #[error("Error while building the request for the pre-response plugin {0} - {1}")]
    BuildRequestError(String, String),
    #[error("Reqwest error: {0}")]
    ReqwestError(reqwest::Error),
    #[error("Unexpected status code: {0}")]
    UnexpectedStatusCode(u16),
    #[error("Error parsing the request: {0}")]
    PluginRequestParseError(serde_json::error::Error),
    #[error("Internal error from plugin {plugin_name}")]
    PluginInternalError {
        plugin_name: String,
        error: serde_json::Value,
    },
    #[error("User error from plugin {plugin_name}")]
    PluginUserError {
        plugin_name: String,
        error: serde_json::Value,
    },
}

impl Error {
    pub fn into_graphql_error(self) -> lang_graphql::http::GraphQLError {
        let is_internal = match self.visibility() {
            ErrorVisibility::Internal => true,
            ErrorVisibility::User => false,
        };
        lang_graphql::http::GraphQLError {
            message: self.to_string(),
            path: None,
            extensions: None,
            is_internal,
        }
    }

    pub fn get_details(&self) -> Option<serde_json::Value> {
        match self {
            Error::PluginUserError { error, .. } => Some(error.clone()),
            _ => None,
        }
    }
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Error::PluginUserError { .. } => ErrorVisibility::User,
            Error::BuildRequestError(_, _)
            | Error::ReqwestError(_)
            | Error::PluginRequestParseError(_)
            | Error::ErrorWhileMakingHTTPRequestToTheHook(_, _)
            | Error::UnexpectedStatusCode(_)
            | Error::PluginInternalError { .. } => ErrorVisibility::Internal,
        }
    }
}

/// Operation type determines the request and response types that are expected in the payload and optionally the response
#[derive(Serialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
pub enum OperationType {
    Query,
    QueryExplain,
    Mutation,
    MutationExplain,
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PreNdcResponsePluginRequestBody<Req, Res> {
    pub session: Option<Session>,
    pub ndc_request: Option<Req>,
    pub ndc_response: Option<Res>,
    pub data_connector_name: Qualified<DataConnectorName>,
    pub operation_type: OperationType,
    pub ndc_version: String,
}

/// Execute pre ndc response plugins, if any
/// Returns either None (use original response) or Some(new_response).
/// If there are no plugins, returns None.
/// May instead return an error from any plugins
pub async fn execute_pre_ndc_response_plugins<Req, Res>(
    pre_ndc_response_plugins: &[ResolvedLifecyclePreNdcResponsePluginHook],
    data_connector: &DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    ndc_request: &Req,
    ndc_response: &Res,
    operation_type: OperationType,
    ndc_version: &str,
) -> Result<Option<Res>, Error>
where
    Req: Serialize + Send + Sync,
    Res: Serialize + for<'de> Deserialize<'de> + Clone + Send + Sync,
{
    match pre_ndc_response_plugins.iter().find(|plugin| {
        plugin
            .connectors
            .iter()
            .any(|connector_name| connector_name == &data_connector.name)
    }) {
        None => Ok(None),
        Some(plugin) => {
            handle_pre_ndc_response_plugin(
                plugin,
                data_connector,
                http_context,
                session,
                ndc_request,
                ndc_response,
                operation_type,
                ndc_version,
            )
            .await
        }
    }
}

/// execute a pre ndc response plugin
async fn handle_pre_ndc_response_plugin<Req, Res>(
    pre_ndc_response_plugin: &ResolvedLifecyclePreNdcResponsePluginHook,
    data_connector: &DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    ndc_request: &Req,
    ndc_response: &Res,
    operation_type: OperationType,
    ndc_version: &str,
) -> Result<Option<Res>, Error>
where
    Req: Serialize + Send + Sync,
    Res: Serialize + for<'de> Deserialize<'de> + Clone + Send + Sync,
{
    let tracer = tracing_util::global_tracer();

    tracer
        .in_span_async(
            "handle_pre_ndc_response_plugin",
            "Handle pre ndc response plugin",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    set_attribute_on_active_span(
                        tracing_util::AttributeVisibility::Default,
                        "plugin.name",
                        pre_ndc_response_plugin.name.to_string(),
                    );

                    let http_client = http_context.client.clone();
                    // construct payload based on configuration
                    let http_request_builder = build_request(
                        pre_ndc_response_plugin,
                        data_connector,
                        &http_client,
                        session,
                        ndc_request,
                        ndc_response,
                        operation_type,
                        ndc_version,
                    )
                    .map_err(|err| {
                        Error::BuildRequestError(pre_ndc_response_plugin.name.to_string(), err)
                    })?;

                    let req = http_request_builder.build().map_err(Error::ReqwestError)?;

                    let response =
                        execute_pre_ndc_response_plugin(pre_ndc_response_plugin, &http_client, req)
                            .await?;

                    match response.status() {
                        reqwest::StatusCode::NO_CONTENT => Ok(None),
                        reqwest::StatusCode::OK => {
                            Ok(response.json().await.map_err(Error::ReqwestError)?)
                        }
                        reqwest::StatusCode::BAD_REQUEST => {
                            let body: serde_json::Value =
                                response.json().await.map_err(Error::ReqwestError)?;

                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.user_error",
                                body.to_string(),
                            );

                            Err(Error::PluginUserError {
                                plugin_name: pre_ndc_response_plugin.name.to_string(),
                                error: body,
                            })
                        }
                        reqwest::StatusCode::INTERNAL_SERVER_ERROR => {
                            let body: serde_json::Value =
                                response.json().await.map_err(Error::ReqwestError)?;

                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.internal_error",
                                body.to_string(),
                            );

                            Err(Error::PluginInternalError {
                                plugin_name: pre_ndc_response_plugin.name.to_string(),
                                error: body,
                            })
                        }
                        _ => Err(Error::UnexpectedStatusCode(response.status().as_u16())),
                    }
                })
            },
        )
        .await
}

/// execute a pre ndc response plugin
async fn execute_pre_ndc_response_plugin(
    pre_ndc_response_plugin: &ResolvedLifecyclePreNdcResponsePluginHook,
    http_client: &Client,
    http_request: reqwest::Request,
) -> Result<reqwest::Response, Error> {
    let tracer = tracing_util::global_tracer();

    tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    http_client.execute(http_request).await.map_err(|e| {
                        Error::ErrorWhileMakingHTTPRequestToTheHook(
                            pre_ndc_response_plugin.name.to_string(),
                            e,
                        )
                    })
                })
            },
        )
        .await
}

fn build_request<Req, Res>(
    pre_ndc_response_plugin: &ResolvedLifecyclePreNdcResponsePluginHook,
    data_connector: &DataConnectorLink,
    http_client: &Client,
    session: &Session,
    ndc_request: &Req,
    ndc_response: &Res,
    operation_type: OperationType,
    ndc_version: &str,
) -> Result<reqwest::RequestBuilder, String>
where
    Req: Serialize,
    Res: Serialize,
{
    let mut http_headers = HeaderMap::new();

    if let Some(headers) = &pre_ndc_response_plugin.config.request.headers {
        for (key, value) in &headers.0 {
            let header_name =
                HeaderName::from_str(key).map_err(|_| format!("Invalid header name {key}"))?;
            let header_value = value
                .value
                .parse()
                .map_err(|_| format!("Invalid value for the header {key}"))?;
            http_headers.insert(header_name, header_value);
        }
    }

    let mut request_builder = http_client
        .post(pre_ndc_response_plugin.url.value.clone())
        .headers(http_headers);

    let mut request_body = PreNdcResponsePluginRequestBody {
        session: None,
        ndc_request: None,
        ndc_response: None,
        data_connector_name: data_connector.name.clone(),
        operation_type,
        ndc_version: ndc_version.to_string(),
    };
    if pre_ndc_response_plugin.config.request.session.is_some() {
        request_body.session = Some(session.clone());
    }
    if pre_ndc_response_plugin.config.request.ndc_request.is_some() {
        request_body.ndc_request = Some(ndc_request);
    }
    if pre_ndc_response_plugin
        .config
        .request
        .ndc_response
        .is_some()
    {
        request_body.ndc_response = Some(ndc_response);
    }
    request_builder = request_builder.json(&request_body);

    Ok(request_builder)
}
