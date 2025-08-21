use std::sync::Arc;

use metadata_resolve::http::SerializableHeaderMap;
use serde_json as json;
use tracing_util::SpanVisibility;

use std::collections::BTreeMap;
use std::collections::hash_map::RandomState;

use base64::{Engine, engine::general_purpose};
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast, Alias, TypeName};
use lang_graphql::normalized_ast;
use open_dds::commands::CommandName;
use open_dds::types::FieldName;

use graphql_ir::{GLOBAL_ID_VERSION, global_id_col_format};
use graphql_schema::{AggregateOutputAnnotation, Annotation, GDS, GlobalID, OutputAnnotation};
use metadata_resolve::Qualified;
use metadata_resolve::data_connectors;
use plan_types::FUNCTION_IR_VALUE_COLUMN_NAME;
use plan_types::{CommandReturnKind, ProcessResponseAs};

trait KeyValueResponse {
    fn remove(&mut self, key: &str) -> Option<json::Value>;
}
impl KeyValueResponse for IndexMap<ndc_models::FieldName, json::Value> {
    fn remove(&mut self, key: &str) -> Option<json::Value> {
        self.swap_remove(key)
    }
}
impl KeyValueResponse for IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue> {
    fn remove(&mut self, key: &str) -> Option<json::Value> {
        // Convert a ndc_models::RowFieldValue to json::Value if exits
        self.swap_remove(key).map(|row_field| row_field.0)
    }
}

// Workaround for the performance issue documented in ENG-1073
//
// Assumes the input is an object. Used for our performance workaround, bypassing RowSet, which
// hopefully we can revert
impl KeyValueResponse for serde_json::Value {
    fn remove(&mut self, key: &str) -> Option<serde_json::Value> {
        self.as_object_mut()?.swap_remove(key)
    }
}

// With the response headers forwarding feature, we also need to extract the
// response headers from NDC result. So that engine's server layer can use that
// to set response headers for the client
#[derive(Debug)]
pub struct ProcessedResponse {
    pub response_headers: Option<SerializableHeaderMap>,
    pub response: json::Value,
}

fn process_global_id_field<T>(
    row: &mut T,
    global_id_fields: &Vec<FieldName>,
    field_alias: &Alias,
    type_name: &TypeName,
) -> Result<json::Value, execute::FieldError>
where
    T: KeyValueResponse,
{
    let mut global_id_cols = BTreeMap::new();
    for field_name in global_id_fields {
        let global_id_ndc_field_name = global_id_col_format(field_alias, field_name);

        let field_json_value_result =
            row.remove(global_id_ndc_field_name.as_str())
                .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
                    summary: format!("missing field: {}", global_id_ndc_field_name.as_str()),
                })?;
        global_id_cols.insert(field_name.clone(), field_json_value_result);
    }
    let global_id = serde_json::to_string(&GlobalID {
        version: GLOBAL_ID_VERSION,
        typename: type_name.clone(),
        id: global_id_cols,
    })?;
    let global_id_value = general_purpose::STANDARD.encode(global_id);
    Ok(json::Value::String(global_id_value))
}

/// Processes a single NDC row and adds `__typename`
/// where needed.
fn process_single_query_response_row<T>(
    mut row: T,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<IndexMap<ast::Alias, json::Value>, execute::FieldError>
where
    T: KeyValueResponse + std::fmt::Debug,
{
    selection_set.as_object_selection_set(
        |type_name, field: &normalized_ast::Field<GDS>, field_call| {
            match field_call.info.generic {
                annotation @ Annotation::Output(field_annotation) => {
                    match field_annotation {
                        OutputAnnotation::GlobalIDField { global_id_fields } => {
                            Ok(process_global_id_field(
                                &mut row,
                                global_id_fields,
                                &field.alias,
                                type_name,
                            )?)
                        }
                        OutputAnnotation::RelayNodeInterfaceID { typename_mappings } => {
                            let global_id_fields = typename_mappings.get(type_name).ok_or(
                                execute::FieldInternalError::GlobalIdTypenameMappingNotFound {
                                    type_name: type_name.clone(),
                                },
                            )?;

                            Ok(process_global_id_field(
                                &mut row,
                                global_id_fields,
                                &field.alias,
                                type_name,
                            )?)
                        }
                        OutputAnnotation::Field { .. } => {
                            let value = row.remove(field.alias.0.as_str()).ok_or_else(|| {
                                execute::NDCUnexpectedError::BadNDCResponse {
                                    summary: format!("missing field: {}", field.alias.clone()),
                                }
                            })?;

                            if field.type_container.is_list() {
                                process_field_selection_as_list(
                                    value,
                                    &field.selection_set,
                                    response_config,
                                )
                            } else {
                                process_field_selection_as_object(
                                    value,
                                    &field.selection_set,
                                    response_config,
                                )
                            }
                        }
                        OutputAnnotation::RelationshipToModel { .. } => {
                            let mut field_json_value_result = row
                                .remove(field.alias.0.as_str())
                                .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
                                    summary: format!("missing field: {}", field.alias.clone()),
                                })?;

                            let rows_set_rows = field_json_value_result
                                .get_mut("rows")
                                .and_then(|j| j.as_array_mut())
                                .map(std::mem::take);
                            // Depending upon the field's type (list or object),
                            // process the selection set accordingly.
                            if field.type_container.is_list() {
                                process_selection_set_as_list(
                                    rows_set_rows,
                                    &field.selection_set,
                                    response_config,
                                )
                                // NOTE: I assume a Null returned here is internal error, but
                                // this behavior is preserved for now:
                                .map(|v| {
                                    v.map_or(json::Value::Null, json_ext::vec_alias_map_to_value)
                                })
                            } else {
                                process_selection_set_as_object(
                                    rows_set_rows,
                                    &field.selection_set,
                                    response_config,
                                )
                                .map(|v| v.map_or(json::Value::Null, json_ext::alias_map_to_value))
                            }
                        }
                        OutputAnnotation::RelationshipToModelAggregate { .. } => {
                            let field_json_value_result = row
                                .remove(field.alias.0.as_str())
                                .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
                                    summary: format!("missing field: {}", field.alias.clone()),
                                })?;
                            match serde_json::from_value(field_json_value_result) {
                                Err(err) => Err(execute::NDCUnexpectedError::BadNDCResponse {
                                    summary: format!("Unable to parse RowSet: {err}"),
                                })?,
                                Ok(row_set) => process_aggregate_requested_fields(
                                    row_set,
                                    &field.selection_set,
                                ),
                            }
                        }
                        OutputAnnotation::RelationshipToCommand(
                            command_relationship_annotation,
                        ) => {
                            let is_nullable = field.type_container.nullable;
                            let return_kind = if field.type_container.is_list() {
                                CommandReturnKind::Array
                            } else {
                                CommandReturnKind::Object
                            };

                            let field_json_value_result = row
                                .remove(field.alias.0.as_str())
                                .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
                                    summary: format!("missing field: {}", field.alias.clone()),
                                })?;
                            match serde_json::from_value::<ndc_models::RowSet>(
                                field_json_value_result,
                            ) {
                                Err(err) => Err(execute::NDCUnexpectedError::BadNDCResponse {
                                    summary: format!("Unable to parse RowSet: {err}"),
                                })?,
                                Ok(rows_set) => {
                                    // the output of a command is optional and can be None
                                    // so we match on the result and return null if the
                                    // command returned None
                                    process_command_rows(
                                        &command_relationship_annotation.command_name,
                                        rows_set.rows,
                                        &field.selection_set,
                                        is_nullable,
                                        return_kind,
                                        response_config,
                                    )
                                    .map(|v| match v {
                                        None => json::Value::Null,
                                        // NOTE: on relationship to a command with commands
                                        // response config, we ignore response header forwarding
                                        Some(v) => v.response,
                                    })
                                }
                            }
                        }
                        _ => Err(execute::FieldInternalError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        })?,
                    }
                }
                annotation => Err(execute::FieldInternalError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?,
            }
        },
    )
}

fn process_selection_set_as_list<T>(
    rows: Option<Vec<T>>,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<Option<Vec<IndexMap<ast::Alias, json::Value>>>, execute::FieldError>
where
    T: KeyValueResponse + std::fmt::Debug,
{
    let processed_response = rows
        .map(|rows| {
            rows.into_iter()
                .map(|row| process_single_query_response_row(row, selection_set, response_config))
                .collect()
        })
        .transpose()?;
    Ok(processed_response)
}

fn process_selection_set_as_object<T>(
    rows: Option<Vec<T>>,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<Option<IndexMap<ast::Alias, json::Value>>, execute::FieldError>
where
    T: KeyValueResponse + std::fmt::Debug,
{
    let processed_response = rows
        .and_then(|rows| rows.into_iter().next())
        .map(|row| process_single_query_response_row(row, selection_set, response_config))
        .transpose()?;
    Ok(processed_response)
}

pub fn process_field_selection_as_list(
    value: json::Value,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<json::Value, execute::FieldError> {
    if selection_set.fields.is_empty() || value.is_null() {
        // If selection set is empty we return the whole value without further processing.
        // If the value is null we have nothing to process so we return null.
        Ok(value)
    } else {
        let rows: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>> =
            json::from_value(value)?;
        let processed_rows: Vec<IndexMap<Alias, json::Value>> = rows
            .into_iter()
            .map(|row| process_single_query_response_row(row, selection_set, response_config))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(json::to_value(processed_rows)?)
    }
}

pub fn process_field_selection_as_object(
    value: json::Value,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<json::Value, execute::FieldError> {
    if selection_set.fields.is_empty() || value.is_null() {
        // If selection set is empty we return the whole value without further processing.
        // If the value is null we have nothing to process so we return null.
        Ok(value)
    } else {
        let row: IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue> =
            json::from_value(value)?;
        let processed_row = process_single_query_response_row(row, selection_set, response_config)?;
        Ok(json::to_value(processed_row)?)
    }
}

pub fn process_command_rows(
    command_name: &Qualified<CommandName>,
    rows: Option<Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue, RandomState>>>,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    is_nullable: bool,
    return_kind: CommandReturnKind,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<Option<ProcessedResponse>, execute::FieldError> {
    match rows {
        None => Err(execute::NDCUnexpectedError::BadNDCResponse {
            summary: format!(
                "Expected one row in the response for command, but no rows present: {command_name}"
            ),
        })?,
        Some(row_vector) => {
            if row_vector.len() > 1 {
                Err(execute::NDCUnexpectedError::BadNDCResponse {
                    summary: format!(
                        "Expected only one row in the response for command: {command_name}"
                    ),
                })?;
            }

            let processed_response = row_vector
                .into_iter()
                .next()
                .map(|row| {
                    process_command_response_row(
                        row,
                        selection_set,
                        is_nullable,
                        return_kind,
                        response_config,
                    )
                })
                .transpose()?;
            Ok(processed_response)
        }
    }
}

fn process_command_response_row(
    mut row: IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    is_nullable: bool,
    return_kind: CommandReturnKind,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<ProcessedResponse, execute::FieldError> {
    let field_value_result = row
        .swap_remove(FUNCTION_IR_VALUE_COLUMN_NAME)
        .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
            summary: format!("missing field: {FUNCTION_IR_VALUE_COLUMN_NAME}"),
        })?;

    let ndc_result = extract_response_headers_and_result(field_value_result.0, response_config)?;
    let field_result = process_command_field_value(
        ndc_result.response,
        selection_set,
        is_nullable,
        return_kind,
        response_config,
    )?;
    Ok(ProcessedResponse {
        response_headers: ndc_result.response_headers,
        response: field_result,
    })
}

fn process_command_field_value(
    field_value_result: serde_json::Value,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    is_nullable: bool,
    return_kind: CommandReturnKind,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<json::Value, execute::FieldError> {
    // When no selection set for commands, return back the value from the
    // connector without any processing.
    if selection_set.fields.is_empty() {
        Ok(field_value_result)
    } else {
        // If the command has a selection set, then the structure of the
        // response should either be a `Array <Object>` or `<Object>` or null,
        // where `<Object>` is the map of the selection set field and it's
        // value.
        match field_value_result {
            json::Value::Null => {
                if is_nullable {
                    Ok(json::Value::Null)
                } else {
                    Err(execute::NDCUnexpectedError::BadNDCResponse {
                        summary: "Unable to parse response from NDC, null value expected".into(),
                    })?
                }
            }
            json::Value::Object(result_map) => match return_kind {
                CommandReturnKind::Array => Err(execute::NDCUnexpectedError::BadNDCResponse {
                    summary: "Unable to parse response from NDC, object value expected".into(),
                })?,
                CommandReturnKind::Object => {
                    let index_map: IndexMap<ndc_models::FieldName, json::Value> =
                        json::from_value(json::Value::Object(result_map))?;
                    let value = process_single_query_response_row(
                        index_map,
                        selection_set,
                        response_config,
                    )?;
                    Ok(json::to_value(value)?)
                }
            },
            json::Value::Array(values) => match return_kind {
                CommandReturnKind::Array => {
                    let array_values: Vec<IndexMap<ndc_models::FieldName, json::Value>> =
                        json::from_value(json::Value::Array(values))?;

                    let r: Vec<IndexMap<Alias, json::Value>> = array_values
                        .into_iter()
                        .map(|value| process_single_query_response_row(value, selection_set, response_config))
                        .collect::<Result<Vec<IndexMap<ast::Alias, json::Value>>, execute::FieldError>>(
                        )?;

                    Ok(json::to_value(r)?)
                }
                CommandReturnKind::Object => Err(execute::NDCUnexpectedError::BadNDCResponse {
                    summary: "Unable to parse response from NDC, object value expected".to_string(),
                })?,
            },
            _ => Err(execute::NDCUnexpectedError::BadNDCResponse {
                summary:
                    "Unable to parse response from NDC, either null, object or array value expected"
                        .to_string(),
            })?,
        }
    }
}

fn process_aggregate_requested_fields(
    row_set: ndc_models::RowSet,
    aggregate_output_selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<json::Value, execute::FieldError> {
    let mut aggregate_results = row_set.aggregates
        .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
            summary:
                "Unable to parse response from NDC, RowSet aggregates property was null when it was expected to be an object"
                    .to_owned(),
        })?;

    reshape_aggregate_fields(&mut aggregate_results, &[], aggregate_output_selection_set)
}

fn reshape_aggregate_fields(
    aggregate_results: &mut IndexMap<ndc_models::FieldName, json::Value>,
    graphql_field_path: &[&Alias],
    aggregate_output_selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<json::Value, execute::FieldError> {
    let result = aggregate_output_selection_set.as_object_selection_set(
        |_type_name, field, field_call| {
            let graphql_field_path = graphql_field_path
                .iter()
                .chain(std::iter::once(&&field.alias))
                .copied()
                .collect::<Vec<&Alias>>();

            match field_call.info.generic {
                Annotation::Output(OutputAnnotation::Aggregate(
                    AggregateOutputAnnotation::AggregationFunctionField(..),
                )) => {
                    let field_name =
                        graphql_ir::mk_alias_from_graphql_field_path(graphql_field_path.as_slice());
                    let aggregate_value = aggregate_results
                        .swap_remove(field_name.as_str())
                        .ok_or_else(|| execute::NDCUnexpectedError::BadNDCResponse {
                            summary: format!("missing aggregate field: {field_name}"),
                        })?;
                    Ok::<_, execute::FieldError>(aggregate_value)
                }

                Annotation::Output(OutputAnnotation::Aggregate(
                    AggregateOutputAnnotation::AggregatableField { .. },
                )) => {
                    let json_object = reshape_aggregate_fields(
                        aggregate_results,
                        graphql_field_path.as_slice(),
                        &field.selection_set,
                    )?;
                    Ok(json_object)
                }
                annotation => Err(execute::FieldInternalError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?,
            }
        },
    )?;
    let response = json::to_value(result).map_err(execute::FieldError::from)?;
    Ok(response)
}

pub fn process_response(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    rows_sets: Vec<ndc_models::RowSet>,
    process_response_as: &ProcessResponseAs,
) -> Result<ProcessedResponse, execute::FieldError> {
    let tracer = tracing_util::global_tracer();
    // Post process the response to add the `__typename` fields
    tracer.in_span(
        "process_response",
        "Process response",
        SpanVisibility::Internal,
        || {
            let row_set = get_single_rowset(rows_sets)?;
            match process_response_as {
                ProcessResponseAs::Array { .. } => {
                    let result = process_selection_set_as_list(row_set.rows, selection_set, None)?;
                    let response = json::to_value(result).map_err(execute::FieldError::from)?;
                    Ok(ProcessedResponse {
                        response,
                        response_headers: None,
                    })
                }
                ProcessResponseAs::Object { .. } => {
                    let result =
                        process_selection_set_as_object(row_set.rows, selection_set, None)?;
                    let response = json::to_value(result).map_err(execute::FieldError::from)?;
                    Ok(ProcessedResponse {
                        response,
                        response_headers: None,
                    })
                }
                ProcessResponseAs::CommandResponse {
                    command_name,
                    is_nullable,
                    return_kind,
                    response_config,
                } => {
                    let result = process_command_rows(
                        command_name,
                        row_set.rows,
                        selection_set,
                        *is_nullable,
                        *return_kind,
                        response_config.as_ref(),
                    )?;
                    match result {
                        None => Ok(ProcessedResponse {
                            response: json::Value::Null,
                            response_headers: None,
                        }),
                        Some(r) => Ok(r),
                    }
                    // json::to_value(result).map_err(execute::FieldError::from)
                }
                ProcessResponseAs::Aggregates => {
                    let result = process_aggregate_requested_fields(row_set, selection_set)?;
                    Ok(ProcessedResponse {
                        response_headers: None,
                        response: result,
                    })
                }
            }
        },
    )
}

pub fn process_command_mutation_response(
    mutation_result: ndc_models::MutationOperationResults,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    is_nullable: bool,
    return_kind: CommandReturnKind,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<ProcessedResponse, execute::FieldError> {
    match mutation_result {
        ndc_models::MutationOperationResults::Procedure { result } => {
            let ndc_result = extract_response_headers_and_result(result, response_config)?;
            let field_result = process_command_field_value(
                ndc_result.response,
                selection_set,
                is_nullable,
                return_kind,
                response_config,
            )?;
            Ok(ProcessedResponse {
                response_headers: ndc_result.response_headers,
                response: field_result,
            })
        }
    }
}

pub fn process_mutation_response(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    connector_response: ndc_models::MutationResponse,
    process_response_as: &ProcessResponseAs,
) -> Result<ProcessedResponse, execute::FieldError> {
    let tracer = tracing_util::global_tracer();

    // Post process the response to add the `__typename` fields
    tracer.in_span(
        "process_response",
        "Process NDC response",
        SpanVisibility::Internal,
        || {
            // NOTE: NDC returns a `Vec<RowSet>` (to account for
            // variables). We don't use variables in NDC queries yet,
            // hence we always pick the first `RowSet`.
            let mutation_results = connector_response
                .operation_results
                .into_iter()
                .next()
                .ok_or(execute::NDCUnexpectedError::BadNDCResponse {
                    summary: "missing rowset".to_string(),
                })?;

            match process_response_as {
                ProcessResponseAs::CommandResponse {
                    command_name: _,
                    is_nullable,
                    return_kind,
                    response_config,
                } => process_command_mutation_response(
                    mutation_results,
                    selection_set,
                    *is_nullable,
                    *return_kind,
                    response_config.as_ref(),
                ),
                _ => Err(execute::FieldInternalError::InternalGeneric {
                    description: "Only commands are supported for mutations".to_string(),
                })?,
            }
        },
    )
}

pub(crate) fn get_single_rowset(
    rows_sets: Vec<ndc_models::RowSet>,
) -> Result<ndc_models::RowSet, execute::FieldError> {
    Ok(rows_sets
        .into_iter()
        .next()
        .ok_or(execute::NDCUnexpectedError::BadNDCResponse {
            summary: "missing rowset".into(),
        })?)
}

/// Extract headers and actual response from a NDC function/procedure, according
/// to `DataConnectorLink.responseHeaders` config
fn extract_response_headers_and_result(
    result: serde_json::Value,
    response_config: Option<&Arc<data_connectors::CommandsResponseConfig>>,
) -> Result<ProcessedResponse, execute::FieldError> {
    if let Some(response_config) = response_config {
        match result {
            json::Value::Object(mut result_map) => {
                // We don't keep information on which NDC function/procedure is
                // configured to forward response headers. We just check if
                // configured headers/result field exist in a JSON object
                // response, if it does we extract, otherwise we skip extracting
                // response headers from it.
                if !result_map.contains_key(response_config.headers_field.as_str())
                    && !result_map.contains_key(response_config.result_field.as_str())
                {
                    return Ok(ProcessedResponse {
                        response_headers: None,
                        response: json::Value::Object(result_map),
                    });
                }

                // get the headers JSON from the response object
                let response_headers_value = result_map
                    .remove(response_config.headers_field.as_str())
                    .ok_or(execute::NDCUnexpectedError::BadNDCResponse {
                        summary: "Unable to find configured response headers field in NDC response"
                            .to_string(),
                    })?;

                // parse the headers JSON into header map
                let mut response_headers_map: SerializableHeaderMap =
                    serde_json::from_value(response_headers_value).map_err(|e| {
                        execute::NDCUnexpectedError::BadNDCResponse {
                            summary: format!(
                                "Unable to deserialize JSON into header map. Error: {e:}"
                            ),
                        }
                    })?;

                // filter the response headers based on the config
                let mut response_headers = axum::http::HeaderMap::new();
                // TODO: change `SerializableHeaderMap` into an Iterator to filter on it
                for forward_header in &response_config.forward_headers {
                    if let Some(header_value) = response_headers_map.0.remove(&forward_header.0) {
                        response_headers.insert(forward_header.0.clone(), header_value);
                    }
                }

                let result_value = result_map
                    .remove(response_config.result_field.as_str())
                    .ok_or(execute::NDCUnexpectedError::BadNDCResponse {
                        summary: "Unable to find configured result field in NDC response"
                            .to_string(),
                    })?;

                Ok(ProcessedResponse {
                    response_headers: Some(SerializableHeaderMap(response_headers)),
                    response: result_value,
                })
            }
            _ => Ok(ProcessedResponse {
                response_headers: None,
                response: result,
            }),
        }
    } else {
        Ok(ProcessedResponse {
            response_headers: None,
            response: result,
        })
    }
}
