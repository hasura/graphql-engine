pub mod client;
pub mod migration;
mod plugins;
pub mod types;
use hasura_authn_core::Session;
use metadata_resolve::LifecyclePluginConfigs;
use plugins::{
    execute_pre_ndc_mutation_explain_request_plugins,
    execute_pre_ndc_mutation_explain_response_plugins, execute_pre_ndc_mutation_request_plugins,
    execute_pre_ndc_mutation_response_plugins, execute_pre_ndc_query_explain_request_plugins,
    execute_pre_ndc_query_explain_response_plugins, execute_pre_ndc_query_request_plugins,
    execute_pre_ndc_query_response_plugins,
};
use pre_ndc_request_plugin::execute::PreNdcRequestPluginResponse;
pub use types::*;

use std::borrow::Cow;
use std::sync::Arc;

use http::HeaderMap;

use lang_graphql::ast::common as ast;
use tracing_util::{AttributeVisibility, SpanVisibility, set_attribute_on_active_span};

use crate::error;
use engine_types::{HttpContext, ProjectId};

/// Executes a NDC operation
pub async fn execute_ndc_query(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query: &NdcQueryRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    execution_span_attribute: &'static str,
    field_span_attribute: String,
    project_id: Option<&ProjectId>,
) -> Result<NdcQueryResponse, error::FieldError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "execute_ndc_query",
            format!(
                "Execute {} query using data connector {}",
                field_span_attribute, data_connector.name
            ),
            SpanVisibility::User,
            || {
                Box::pin(async {
                    set_attribute_on_active_span(
                        AttributeVisibility::Default,
                        "operation",
                        execution_span_attribute,
                    );
                    set_attribute_on_active_span(
                        AttributeVisibility::Default,
                        "field",
                        field_span_attribute,
                    );
                    let connector_response = fetch_from_data_connector(
                        http_context,
                        plugins,
                        session,
                        request_headers,
                        query,
                        data_connector,
                        project_id,
                    )
                    .await?;
                    Ok(connector_response)
                })
            },
        )
        .await
}

pub async fn fetch_from_data_connector(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query_request: &NdcQueryRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<NdcQueryResponse, client::Error> {
    let tracer = tracing_util::global_tracer();

    let plugin_execution_result = execute_pre_ndc_query_request_plugins(
        &plugins.pre_ndc_request_plugins,
        data_connector,
        http_context,
        session,
        request_headers,
        query_request,
    )
    .await?;

    // plugin can return nothing, a new request to be used, or a response to be returned immediately
    let owned_request;
    let query_request = match plugin_execution_result {
        None => query_request,
        Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => {
            owned_request = ndc_request;
            &owned_request
        }
        Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => return Ok(ndc_response),
    };

    let response = tracer
        .in_span_async(
            "fetch_from_data_connector",
            "Fetch from data connector",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Query),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::query_post(ndc_config, query_request).await
                    // .map_err(error::RequestError::from) // error::Error -> InternalError -> Error
                })
            },
        )
        .await?;

    let response = execute_pre_ndc_query_response_plugins(
        &plugins.pre_ndc_response_plugins,
        data_connector,
        http_context,
        session,
        query_request,
        &response,
    )
    .await?
    .unwrap_or(response);

    Ok(response)
}

// This function appends project-id (if present) to the HeaderMap defined by the data_connector object
pub fn append_project_id_to_headers<'a>(
    headers: &'a HeaderMap,
    project_id: Option<&ProjectId>,
) -> Result<Cow<'a, HeaderMap>, client::Error> {
    match project_id {
        None => Ok(Cow::Borrowed(headers)),
        Some(project_id) => {
            let mut modified_headers = headers.clone();
            modified_headers.append(
                "project-id",
                reqwest::header::HeaderValue::from_str(&project_id.0)?,
            );
            Ok(Cow::Owned(modified_headers))
        }
    }
}

/// Executes a NDC mutation
pub(crate) async fn execute_ndc_mutation(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query: &NdcMutationRequest,
    data_connector: &Arc<metadata_resolve::DataConnectorLink>,
    execution_span_attribute: &'static str,
    field_span_attribute: String,
    project_id: Option<&ProjectId>,
) -> Result<NdcMutationResponse, error::FieldError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "execute_ndc_mutation",
            format!(
                "Execute {} mutation using data connector {}",
                field_span_attribute, data_connector.name
            ),
            SpanVisibility::User,
            || {
                Box::pin(async {
                    set_attribute_on_active_span(
                        AttributeVisibility::Default,
                        "operation",
                        execution_span_attribute,
                    );
                    set_attribute_on_active_span(
                        AttributeVisibility::Default,
                        "field",
                        field_span_attribute,
                    );
                    let connector_response = fetch_from_data_connector_mutation(
                        http_context,
                        plugins,
                        session,
                        request_headers,
                        query,
                        data_connector,
                        project_id,
                    )
                    .await?;
                    Ok(connector_response)
                })
            },
        )
        .await
}

pub async fn fetch_from_data_connector_mutation(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query_request: &NdcMutationRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<NdcMutationResponse, client::Error> {
    let tracer = tracing_util::global_tracer();

    let plugin_execution_result = execute_pre_ndc_mutation_request_plugins(
        &plugins.pre_ndc_request_plugins,
        data_connector,
        http_context,
        session,
        request_headers,
        query_request,
    )
    .await?;

    // plugin can return nothing, a new request to be used, or a response to be returned immediately
    let owned_request;
    let query_request = match plugin_execution_result {
        None => query_request,
        Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => {
            owned_request = ndc_request;
            &owned_request
        }
        Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => return Ok(ndc_response),
    };

    let response = tracer
        .in_span_async(
            "fetch_from_data_connector_mutation",
            format!("Execute mutation on data connector {}", data_connector.name),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Mutation),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::mutation_post(ndc_config, query_request).await
                })
            },
        )
        .await?;

    let response = execute_pre_ndc_mutation_response_plugins(
        &plugins.pre_ndc_response_plugins,
        data_connector,
        http_context,
        session,
        query_request,
        &response,
    )
    .await?
    .unwrap_or(response);

    Ok(response)
}

pub async fn fetch_from_data_connector_explain(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query_request: &NdcQueryRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<NdcExplainResponse, client::Error> {
    let tracer = tracing_util::global_tracer();

    let plugin_execution_result = execute_pre_ndc_query_explain_request_plugins(
        &plugins.pre_ndc_request_plugins,
        data_connector,
        http_context,
        session,
        request_headers,
        query_request,
    )
    .await?;

    // plugin can return nothing, a new request to be used, or a response to be returned immediately
    let owned_request;
    let query_request = match plugin_execution_result {
        None => query_request,
        Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => {
            owned_request = ndc_request;
            &owned_request
        }
        Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => return Ok(ndc_response),
    };

    let response = tracer
        .in_span_async(
            "fetch_from_data_connector_explain",
            format!("Execute explain on data connector {}", data_connector.name),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Query),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::explain_query_post(ndc_config, query_request).await
                })
            },
        )
        .await?;

    let response = execute_pre_ndc_query_explain_response_plugins(
        &plugins.pre_ndc_response_plugins,
        data_connector,
        http_context,
        session,
        query_request,
        &response,
    )
    .await?
    .unwrap_or(response);

    Ok(response)
}

pub async fn fetch_from_data_connector_mutation_explain(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query_request: &NdcMutationRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<NdcExplainResponse, client::Error> {
    let tracer = tracing_util::global_tracer();

    let plugin_execution_result = execute_pre_ndc_mutation_explain_request_plugins(
        &plugins.pre_ndc_request_plugins,
        data_connector,
        http_context,
        session,
        request_headers,
        query_request,
    )
    .await?;

    // plugin can return nothing, a new request to be used, or a response to be returned immediately
    let owned_request;
    let query_request = match plugin_execution_result {
        None => query_request,
        Some(PreNdcRequestPluginResponse::NdcRequest(ndc_request)) => {
            owned_request = ndc_request;
            &owned_request
        }
        Some(PreNdcRequestPluginResponse::NdcResponse(ndc_response)) => return Ok(ndc_response),
    };

    let response = tracer
        .in_span_async(
            "fetch_from_data_connector_mutation_explain",
            format!(
                "Execute mutation explain on data connector {}",
                data_connector.name
            ),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Mutation),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::explain_mutation_post(ndc_config, query_request).await
                })
            },
        )
        .await?;

    let response = execute_pre_ndc_mutation_explain_response_plugins(
        &plugins.pre_ndc_response_plugins,
        data_connector,
        http_context,
        session,
        query_request,
        &response,
    )
    .await?
    .unwrap_or(response);

    Ok(response)
}

pub async fn fetch_from_data_connector_insert_rel(
    http_context: &HttpContext,
    insert_request: &ndc_models::RelationalInsertRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<ndc_models::RelationalInsertResponse, client::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "fetch_from_data_connector_insert_rel",
            format!(
                "Execute relational insert on data connector {}",
                data_connector.name
            ),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Mutation),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::mutation_relational_insert_post(ndc_config, insert_request).await
                })
            },
        )
        .await
}

pub async fn fetch_from_data_connector_update_rel(
    http_context: &HttpContext,
    update_request: &ndc_models::RelationalUpdateRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<ndc_models::RelationalUpdateResponse, client::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "fetch_from_data_connector_update_rel",
            format!(
                "Execute relational update on data connector {}",
                data_connector.name
            ),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Mutation),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::mutation_relational_update_post(ndc_config, update_request).await
                })
            },
        )
        .await
}

pub async fn fetch_from_data_connector_delete_rel(
    http_context: &HttpContext,
    delete_request: &ndc_models::RelationalDeleteRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<ndc_models::RelationalDeleteResponse, client::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "fetch_from_data_connector_delete_rel",
            format!(
                "Execute relational delete on data connector {}",
                data_connector.name
            ),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(&data_connector.headers.0, project_id)?;
                    let ndc_config = client::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Mutation),
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_context.client.clone(),
                        headers,
                        response_size_limit: http_context.ndc_response_size_limit,
                    };
                    client::mutation_relational_delete_post(ndc_config, delete_request).await
                })
            },
        )
        .await
}
