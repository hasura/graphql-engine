use axum::http::{HeaderMap, HeaderName};
use engine_types::HttpContext;
use hasura_authn_core::{Role, Session, SessionVariableName};
use metadata_resolve::{DataConnectorLink, Qualified, ResolvedLifecyclePreNdcRequestPluginHook};
use open_dds::data_connector::DataConnectorName;
use reqwest::{
    Client,
    header::{InvalidHeaderName, InvalidHeaderValue},
};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, str::FromStr, sync::Arc};
use tracing_util::{ErrorVisibility, SpanVisibility, TraceableError, set_attribute_on_active_span};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error while making the HTTP request to the pre-parse plugin {0} - {1}")]
    ErrorWhileMakingHTTPRequestToTheHook(String, reqwest::Error),
    #[error("Error while building the request for the pre-parse plugin {0} - {1}")]
    BuildRequestError(String, #[source] BuildRequestError),
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

#[derive(Debug, thiserror::Error)]
pub enum BuildRequestError {
    #[error("Invalid header name {header_name}: {error}")]
    InvalidHeaderName {
        header_name: String,
        #[source]
        error: InvalidHeaderName,
    },
    #[error("Invalid header value for header {header_name}: {error}")]
    InvalidHeaderValue {
        header_name: HeaderName,
        #[source]
        error: InvalidHeaderValue,
    },
    #[error("Failed to convert session: {0}")]
    SessionConversionError(String),
    #[error("Failed to convert session variable '{variable_name}': {error}")]
    SessionVariableConversionError {
        variable_name: SessionVariableName,
        error: serde_json::Error,
    },
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Error::BuildRequestError(_, _) => ErrorVisibility::Internal,
            Error::PluginUserError { .. }
            | Error::ReqwestError(_)
            | Error::PluginRequestParseError(_)
            | Error::ErrorWhileMakingHTTPRequestToTheHook(_, _)
            | Error::UnexpectedStatusCode(_)
            | Error::PluginInternalError { .. } => ErrorVisibility::User,
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub enum PreNdcRequestPluginResponse<Req, Res> {
    NdcRequest(Req),
    NdcResponse(Res),
}

/// Operation type determines the request and response types that are expected in the payload and optionally the response
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
pub enum OperationType {
    Query,
    QueryExplain,
    Mutation,
    MutationExplain,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PreNdcRequestSession {
    pub role: Role,
    pub variables: BTreeMap<SessionVariableName, serde_json::Value>,
}

impl TryFrom<Session> for PreNdcRequestSession {
    type Error = BuildRequestError;

    fn try_from(session: Session) -> Result<Self, Self::Error> {
        let variables = session
            .variables
            .into_iter()
            .map(|(k, v)| Ok((k, v.as_value()?)))
            .collect::<Result<_, Self::Error>>()?;
        Ok(Self {
            role: session.role,
            variables,
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PreNdcRequestPluginRequestBody<Req> {
    pub session: Option<PreNdcRequestSession>,
    pub ndc_request: Option<Req>,
    pub data_connector_name: Qualified<DataConnectorName>,
    pub operation_type: OperationType,
    pub ndc_version: String,
}

/// Execute pre ndc request plugins, if any
/// Returns either a possibly modified ndc request, or a response.
/// If there are no plugins, returns the ndc request as-is.
/// May instead return an error from any plugins
pub async fn execute_pre_ndc_request_plugins<Req, Res>(
    pre_ndc_request_plugins: &BTreeMap<
        Qualified<DataConnectorName>,
        Arc<ResolvedLifecyclePreNdcRequestPluginHook>,
    >,
    data_connector: &DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    ndc_request: &Req,
    operation_type: OperationType,
    ndc_version: &str,
) -> Result<Option<PreNdcRequestPluginResponse<Req, Res>>, Error>
where
    Req: Serialize + for<'de> Deserialize<'de> + Clone + Send + Sync + std::fmt::Debug,
    Res: for<'de> Deserialize<'de>,
{
    match pre_ndc_request_plugins.get(&data_connector.name) {
        None => Ok(None),
        Some(plugin) => {
            handle_pre_ndc_request_plugin(
                plugin,
                data_connector,
                http_context,
                session,
                ndc_request,
                operation_type,
                ndc_version,
            )
            .await
        }
    }
}

/// execute a pre ndc request plugin
async fn handle_pre_ndc_request_plugin<Req, Res>(
    pre_ndc_request_plugin: &ResolvedLifecyclePreNdcRequestPluginHook,
    data_connector: &DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    ndc_request: &Req,
    operation_type: OperationType,
    ndc_version: &str,
) -> Result<Option<PreNdcRequestPluginResponse<Req, Res>>, Error>
where
    Req: Serialize + for<'de> Deserialize<'de> + Clone + Send + Sync + std::fmt::Debug,
    Res: for<'de> Deserialize<'de>,
{
    let tracer = tracing_util::global_tracer();

    tracer
        .in_span_async(
            "handle_pre_ndc_request_plugin",
            "Handle pre ndc request plugin",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    set_attribute_on_active_span(
                        tracing_util::AttributeVisibility::Default,
                        "plugin.name",
                        pre_ndc_request_plugin.name.to_string(),
                    );

                    let http_client = http_context.client.clone();
                    // construct payload based on configuration
                    let http_request_builder = build_request(
                        pre_ndc_request_plugin,
                        data_connector,
                        &http_client,
                        session,
                        ndc_request,
                        operation_type,
                        ndc_version,
                    )
                    .map_err(|err| {
                        Error::BuildRequestError(pre_ndc_request_plugin.name.to_string(), err)
                    })?;

                    let req = http_request_builder.build().map_err(Error::ReqwestError)?;

                    let response =
                        execute_pre_ndc_request_plugin(pre_ndc_request_plugin, &http_client, req)
                            .await?;

                    match response.status() {
                        reqwest::StatusCode::NO_CONTENT => Ok(None),
                        reqwest::StatusCode::OK => {
                            let body: PreNdcRequestPluginResponse<Req, Res> =
                                response.json().await.map_err(Error::ReqwestError)?;
                            Ok(Some(body))
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
                                plugin_name: pre_ndc_request_plugin.name.to_string(),
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
                                plugin_name: pre_ndc_request_plugin.name.to_string(),
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
/// execute a pre ndc request plugin
async fn execute_pre_ndc_request_plugin(
    pre_ndc_request_plugin: &ResolvedLifecyclePreNdcRequestPluginHook,
    http_client: &Client,
    http_request: reqwest::Request,
) -> Result<reqwest::Response, Error> {
    let tracer = tracing_util::global_tracer();

    tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    http_client.execute(http_request).await.map_err(|e| {
                        Error::ErrorWhileMakingHTTPRequestToTheHook(
                            pre_ndc_request_plugin.name.to_string(),
                            e,
                        )
                    })
                })
            },
        )
        .await
}

fn build_request<Req>(
    pre_ndc_request_plugin: &ResolvedLifecyclePreNdcRequestPluginHook,
    data_connector: &DataConnectorLink,
    http_client: &Client,
    session: &Session,
    ndc_request: Req,
    operation_type: OperationType,
    ndc_version: &str,
) -> Result<reqwest::RequestBuilder, BuildRequestError>
where
    Req: Serialize + std::fmt::Debug,
{
    let mut http_headers = HeaderMap::new();

    if let Some(headers) = &pre_ndc_request_plugin.config.request.headers {
        for (key, value) in &headers.0 {
            let header_name =
                HeaderName::from_str(key).map_err(|err| BuildRequestError::InvalidHeaderName {
                    header_name: key.clone(),
                    error: err,
                })?;
            let header_value =
                value
                    .value
                    .parse()
                    .map_err(|error| BuildRequestError::InvalidHeaderValue {
                        header_name: header_name.clone(),
                        error,
                    })?;
            http_headers.insert(header_name, header_value);
        }
    }

    let mut request_builder = http_client
        .post(pre_ndc_request_plugin.url.value.clone())
        .headers(http_headers);

    let mut request_body = PreNdcRequestPluginRequestBody {
        session: None,
        ndc_request: None,
        data_connector_name: data_connector.name.clone(),
        operation_type,
        ndc_version: ndc_version.to_string(),
    };
    if pre_ndc_request_plugin.config.request.session.is_some() {
        request_body.session = Some(session.clone().try_into()?);
    }
    if pre_ndc_request_plugin.config.request.ndc_request.is_some() {
        request_body.ndc_request = Some(ndc_request);
    }

    request_builder = request_builder.json(&request_body);

    Ok(request_builder)
}
