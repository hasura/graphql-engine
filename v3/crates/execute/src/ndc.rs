pub mod client;
pub mod migration;
pub mod types;
pub use types::*;

use std::borrow::Cow;
use std::sync::Arc;

use axum::http::HeaderMap;

use lang_graphql::ast::common as ast;
use tracing_util::{AttributeVisibility, SpanVisibility, set_attribute_on_active_span};

use crate::error;
use engine_types::{HttpContext, ProjectId};

/// Executes a NDC operation
pub async fn execute_ndc_query(
    http_context: &HttpContext,
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
                    let connector_response =
                        fetch_from_data_connector(http_context, query, data_connector, project_id)
                            .await?;
                    Ok(connector_response)
                })
            },
        )
        .await
}

pub async fn fetch_from_data_connector(
    http_context: &HttpContext,
    query_request: &NdcQueryRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<NdcQueryResponse, client::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
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
        .await
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
    query_request: &NdcMutationRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
    project_id: Option<&ProjectId>,
) -> Result<NdcMutationResponse, client::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
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
        .await
}
