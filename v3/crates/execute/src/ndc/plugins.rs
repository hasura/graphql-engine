use engine_types::HttpContext;
use hasura_authn_core::Session;
use metadata_resolve::{
    Qualified, ResolvedLifecyclePreNdcRequestPluginHook, ResolvedLifecyclePreNdcResponsePluginHook,
};
use open_dds::data_connector::DataConnectorName;
use pre_ndc_request_plugin::execute::{
    PreNdcRequestPluginResponse, execute_pre_ndc_request_plugins,
};
use pre_ndc_response_plugin::execute::execute_pre_ndc_response_plugins;
use std::{collections::BTreeMap, sync::Arc};

use crate::ndc::NdcExplainResponse;

use super::{NdcMutationRequest, NdcMutationResponse, NdcQueryRequest, NdcQueryResponse, client};

/// Wrapper for execute_pre_ndc_request_plugins that is specific to queries, and aware of the NdcQueryRequestEnum and variants.
/// It handles converting the NdcQueryRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_query_request_plugins(
    plugins: &BTreeMap<Qualified<DataConnectorName>, Arc<ResolvedLifecyclePreNdcRequestPluginHook>>,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    request_headers: &axum::http::HeaderMap,
    query_request: &NdcQueryRequest,
) -> Result<Option<PreNdcRequestPluginResponse<NdcQueryRequest, NdcQueryResponse>>, client::Error> {
    Ok(match query_request {
        NdcQueryRequest::V01(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::Query,
                "v0.1.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcQueryRequest::V01(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => Some(
                    PreNdcRequestPluginResponse::NdcResponse(NdcQueryResponse::V01(ndc_response)),
                ),
            }
        }
        NdcQueryRequest::V02(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::Query,
                "v0.2.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcQueryRequest::V02(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => Some(
                    PreNdcRequestPluginResponse::NdcResponse(NdcQueryResponse::V02(ndc_response)),
                ),
            }
        }
    })
}

/// Wrapper for execute_pre_ndc_response_plugins that is specific to queries, and aware of the NdcQueryRequestEnum and variants.
/// It handles converting the NdcQueryRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_query_response_plugins(
    plugins: &BTreeMap<
        Qualified<DataConnectorName>,
        Arc<ResolvedLifecyclePreNdcResponsePluginHook>,
    >,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    query_request: &NdcQueryRequest,
    response: &NdcQueryResponse,
) -> Result<Option<NdcQueryResponse>, client::Error> {
    match (query_request, response) {
        (NdcQueryRequest::V01(request), NdcQueryResponse::V01(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::Query,
                "v0.1.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcQueryResponse::V01(response))),
                None => Ok(None),
            }
        }
        (NdcQueryRequest::V02(request), NdcQueryResponse::V02(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::Query,
                "v0.2.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcQueryResponse::V02(response))),
                None => Ok(None),
            }
        }
        _ => Ok(None), // Version mismatch, return original response
    }
}

/// Wrapper for execute_pre_ndc_request_plugins that is specific to mutations, and aware of the NdcMutationRequestEnum and variants.
/// It handles converting the NdcMutationRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_mutation_request_plugins(
    plugins: &BTreeMap<Qualified<DataConnectorName>, Arc<ResolvedLifecyclePreNdcRequestPluginHook>>,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    request_headers: &axum::http::HeaderMap,
    mutation_request: &NdcMutationRequest,
) -> Result<
    Option<PreNdcRequestPluginResponse<NdcMutationRequest, NdcMutationResponse>>,
    client::Error,
> {
    Ok(match mutation_request {
        NdcMutationRequest::V01(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::Mutation,
                "v0.1.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcMutationRequest::V01(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => {
                    Some(PreNdcRequestPluginResponse::NdcResponse(
                        NdcMutationResponse::V01(ndc_response),
                    ))
                }
            }
        }
        NdcMutationRequest::V02(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::Mutation,
                "v0.2.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcMutationRequest::V02(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => {
                    Some(PreNdcRequestPluginResponse::NdcResponse(
                        NdcMutationResponse::V02(ndc_response),
                    ))
                }
            }
        }
    })
}

/// Wrapper for execute_pre_ndc_response_plugins that is specific to mutations, and aware of the NdcMutationRequestEnum and variants.
/// It handles converting the NdcMutationRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_mutation_response_plugins(
    plugins: &BTreeMap<
        Qualified<DataConnectorName>,
        Arc<ResolvedLifecyclePreNdcResponsePluginHook>,
    >,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    mutation_request: &NdcMutationRequest,
    response: &NdcMutationResponse,
) -> Result<Option<NdcMutationResponse>, client::Error> {
    match (mutation_request, response) {
        (NdcMutationRequest::V01(request), NdcMutationResponse::V01(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::Mutation,
                "v0.1.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcMutationResponse::V01(response))),
                None => Ok(None),
            }
        }
        (NdcMutationRequest::V02(request), NdcMutationResponse::V02(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::Mutation,
                "v0.2.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcMutationResponse::V02(response))),
                None => Ok(None),
            }
        }
        _ => Ok(None), // Version mismatch, return original response
    }
}

/// Wrapper for execute_pre_ndc_request_plugins that is specific to query explain requests, and aware of the NdcQueryRequestEnum and variants.
/// It handles converting the NdcQueryRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_query_explain_request_plugins(
    plugins: &BTreeMap<Qualified<DataConnectorName>, Arc<ResolvedLifecyclePreNdcRequestPluginHook>>,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    request_headers: &axum::http::HeaderMap,
    query_request: &NdcQueryRequest,
) -> Result<Option<PreNdcRequestPluginResponse<NdcQueryRequest, NdcExplainResponse>>, client::Error>
{
    Ok(match query_request {
        NdcQueryRequest::V01(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::QueryExplain,
                "v0.1.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcQueryRequest::V01(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => Some(
                    PreNdcRequestPluginResponse::NdcResponse(NdcExplainResponse::V01(ndc_response)),
                ),
            }
        }
        NdcQueryRequest::V02(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::QueryExplain,
                "v0.2.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcQueryRequest::V02(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => Some(
                    PreNdcRequestPluginResponse::NdcResponse(NdcExplainResponse::V02(ndc_response)),
                ),
            }
        }
    })
}

/// Wrapper for execute_pre_ndc_response_plugins that is specific to query explain requests, and aware of the NdcQueryRequestEnum and variants.
/// It handles converting the NdcQueryRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_query_explain_response_plugins(
    plugins: &BTreeMap<
        Qualified<DataConnectorName>,
        Arc<ResolvedLifecyclePreNdcResponsePluginHook>,
    >,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    query_request: &NdcQueryRequest,
    response: &NdcExplainResponse,
) -> Result<Option<NdcExplainResponse>, client::Error> {
    match (query_request, response) {
        (NdcQueryRequest::V01(request), NdcExplainResponse::V01(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::QueryExplain,
                "v0.1.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcExplainResponse::V01(response))),
                None => Ok(None),
            }
        }
        (NdcQueryRequest::V02(request), NdcExplainResponse::V02(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::QueryExplain,
                "v0.2.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcExplainResponse::V02(response))),
                None => Ok(None),
            }
        }
        _ => Ok(None), // Version mismatch, return original response
    }
}

/// Wrapper for execute_pre_ndc_request_plugins that is specific to mutation explain requests, and aware of the NdcMutationRequestEnum and variants.
/// It handles converting the NdcMutationRequestEnum to the specific request type, and back.
pub async fn execute_pre_ndc_mutation_explain_request_plugins(
    plugins: &BTreeMap<Qualified<DataConnectorName>, Arc<ResolvedLifecyclePreNdcRequestPluginHook>>,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    request_headers: &axum::http::HeaderMap,
    mutation_request: &NdcMutationRequest,
) -> Result<
    Option<PreNdcRequestPluginResponse<NdcMutationRequest, NdcExplainResponse>>,
    client::Error,
> {
    Ok(match mutation_request {
        NdcMutationRequest::V01(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::MutationExplain,
                "v0.1.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcMutationRequest::V01(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => Some(
                    PreNdcRequestPluginResponse::NdcResponse(NdcExplainResponse::V01(ndc_response)),
                ),
            }
        }
        NdcMutationRequest::V02(request) => {
            match execute_pre_ndc_request_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request_headers,
                request,
                pre_ndc_request_plugin::execute::OperationType::MutationExplain,
                "v0.2.x",
            )
            .await?
            {
                None => None,
                Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => Some(
                    PreNdcRequestPluginResponse::NdcRequest(NdcMutationRequest::V02(ndc_request)),
                ),
                Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => Some(
                    PreNdcRequestPluginResponse::NdcResponse(NdcExplainResponse::V02(ndc_response)),
                ),
            }
        }
    })
}

pub async fn execute_pre_ndc_mutation_explain_response_plugins(
    plugins: &BTreeMap<
        Qualified<DataConnectorName>,
        Arc<ResolvedLifecyclePreNdcResponsePluginHook>,
    >,
    data_connector: &metadata_resolve::DataConnectorLink,
    http_context: &HttpContext,
    session: &Session,
    mutation_request: &NdcMutationRequest,
    response: &NdcExplainResponse,
) -> Result<Option<NdcExplainResponse>, client::Error> {
    match (mutation_request, response) {
        (NdcMutationRequest::V01(request), NdcExplainResponse::V01(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::MutationExplain,
                "v0.1.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcExplainResponse::V01(response))),
                None => Ok(None),
            }
        }
        (NdcMutationRequest::V02(request), NdcExplainResponse::V02(response)) => {
            match execute_pre_ndc_response_plugins(
                plugins,
                data_connector,
                http_context,
                session,
                request,
                response,
                pre_ndc_response_plugin::execute::OperationType::MutationExplain,
                "v0.2.x",
            )
            .await?
            {
                Some(response) => Ok(Some(NdcExplainResponse::V02(response))),
                None => Ok(None),
            }
        }
        _ => Ok(None), // Version mismatch, return original response
    }
}
