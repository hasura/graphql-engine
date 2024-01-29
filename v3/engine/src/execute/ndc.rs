use axum::http::HeaderMap;
use serde_json as json;

use gql::normalized_ast;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use ndc_client as ndc;
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, SpanVisibility};

use super::process_response::process_command_rows;
use super::query_plan::ProcessResponseAs;
use super::{error, ProjectId};
use crate::metadata::resolved;
use crate::schema::GDS;

/// Executes a NDC operation
pub async fn execute_ndc_query<'n, 's>(
    http_client: &reqwest::Client,
    query: ndc::models::QueryRequest,
    data_connector: &resolved::data_connector::DataConnector,
    execution_span_attribute: String,
    field_span_attribute: String,
    project_id: Option<ProjectId>,
) -> Result<Vec<ndc::models::RowSet>, error::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async("execute_ndc_query", SpanVisibility::User, || {
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
                    fetch_from_data_connector(http_client, query, data_connector, project_id)
                        .await?;
                Ok(connector_response.0)
            })
        })
        .await
}

pub(crate) async fn fetch_from_data_connector<'s>(
    http_client: &reqwest::Client,
    query_request: ndc::models::QueryRequest,
    data_connector: &resolved::data_connector::DataConnector,
    project_id: Option<ProjectId>,
) -> Result<ndc::models::QueryResponse, error::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "fetch_from_data_connector",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(data_connector.headers.0.clone(), project_id)?;
                    let ndc_config = ndc::apis::configuration::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Query),
                        user_agent: None,
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_client.clone(),
                        headers,
                    };
                    ndc::apis::default_api::query_post(&ndc_config, query_request)
                        .await
                        .map_err(error::Error::from) // ndc_client::apis::Error -> InternalError -> Error
                })
            },
        )
        .await
}

// This function appends project-id (if present) to the HeaderMap defined by the data_connector object
pub fn append_project_id_to_headers(
    mut headers: HeaderMap,
    project_id: Option<ProjectId>,
) -> Result<HeaderMap, error::Error> {
    match project_id {
        None => Ok(headers),
        Some(project_id) => {
            headers.append(
                "project-id",
                reqwest::header::HeaderValue::from_str(&project_id.0)
                    .map_err(error::Error::ProjectIdConversionError)?,
            );
            Ok(headers)
        }
    }
}

/// Executes a NDC mutation
pub(crate) async fn execute_ndc_mutation<'n, 's, 'ir>(
    http_client: &reqwest::Client,
    query: ndc::models::MutationRequest,
    data_connector: &resolved::data_connector::DataConnector,
    selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
    execution_span_attribute: String,
    field_span_attribute: String,
    process_response_as: ProcessResponseAs<'ir>,
    project_id: Option<ProjectId>,
) -> Result<json::Value, error::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async("execute_ndc_mutation", SpanVisibility::User, || {
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
                    http_client,
                    query,
                    data_connector,
                    project_id,
                )
                .await?;
                // Post process the response to add the `__typename` fields
                tracer.in_span("process_response", SpanVisibility::Internal, || {
                    // NOTE: NDC returns a `Vec<RowSet>` (to account for
                    // variables). We don't use variables in NDC queries yet,
                    // hence we always pick the first `RowSet`.
                    let mutation_results = connector_response
                        .operation_results
                        .into_iter()
                        .next()
                        .ok_or(error::InternalDeveloperError::BadGDCResponse {
                            summary: "missing rowset".into(),
                        })?;
                    match process_response_as {
                        ProcessResponseAs::CommandResponse {
                            command_name,
                            type_container,
                        } => {
                            let result = process_command_rows(
                                command_name,
                                mutation_results.returning,
                                selection_set,
                                type_container,
                            )?;
                            Ok(json::to_value(result).map_err(error::Error::from))
                        }
                        _ => Err(error::Error::from(
                            error::InternalEngineError::InternalGeneric {
                                description: "mutations without commands are not supported yet"
                                    .into(),
                            },
                        )),
                    }?
                })
            })
        })
        .await
}

pub(crate) async fn fetch_from_data_connector_mutation<'s>(
    http_client: &reqwest::Client,
    query_request: ndc::models::MutationRequest,
    data_connector: &resolved::data_connector::DataConnector,
    project_id: Option<ProjectId>,
) -> Result<ndc::models::MutationResponse, error::Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "fetch_from_data_connector",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let headers =
                        append_project_id_to_headers(data_connector.headers.0.clone(), project_id)?;
                    let ndc_config = ndc::apis::configuration::Configuration {
                        base_path: data_connector.url.get_url(ast::OperationType::Mutation),
                        user_agent: None,
                        // This is isn't expensive, reqwest::Client is behind an Arc
                        client: http_client.clone(),
                        headers,
                    };
                    ndc::apis::default_api::mutation_post(&ndc_config, query_request)
                        .await
                        .map_err(error::Error::from) // ndc_client::apis::Error -> InternalError -> Error
                })
            },
        )
        .await
}
