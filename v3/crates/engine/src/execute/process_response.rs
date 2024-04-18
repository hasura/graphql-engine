use serde_json as json;
use tracing_util::SpanVisibility;

use std::collections::hash_map::RandomState;
use std::collections::BTreeMap;

use base64::{engine::general_purpose, Engine};
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast, Alias, TypeContainer, TypeName};
use lang_graphql::normalized_ast;
use ndc_models;
use open_dds::commands::CommandName;
use open_dds::types::FieldName;

use super::error;
use super::global_id::{global_id_col_format, GLOBAL_ID_VERSION};
use super::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use super::plan::ProcessResponseAs;
use crate::metadata::resolved::subgraph::Qualified;
use crate::schema::{
    types::{Annotation, GlobalID, OutputAnnotation},
    GDS,
};

trait KeyValueResponse {
    fn remove(&mut self, key: &str) -> Option<json::Value>;
    fn contains_key(&self, key: &str) -> bool;
}
impl KeyValueResponse for IndexMap<String, json::Value> {
    fn remove(&mut self, key: &str) -> Option<json::Value> {
        self.swap_remove(key)
    }
    fn contains_key(&self, key: &str) -> bool {
        self.contains_key(key)
    }
}
impl KeyValueResponse for IndexMap<String, ndc_models::RowFieldValue> {
    fn remove(&mut self, key: &str) -> Option<json::Value> {
        // Convert a ndc_models::RowFieldValue to json::Value if exits
        self.swap_remove(key).map(|row_field| row_field.0)
    }
    fn contains_key(&self, key: &str) -> bool {
        self.contains_key(key)
    }
}

fn process_global_id_field<T>(
    row: &mut T,
    global_id_fields: &Vec<FieldName>,
    field_alias: &Alias,
    type_name: &TypeName,
) -> Result<json::Value, error::Error>
where
    T: KeyValueResponse,
{
    let mut global_id_cols = BTreeMap::new();
    for field_name in global_id_fields {
        let global_id_ndc_field_name = global_id_col_format(field_alias, field_name);

        let field_json_value_result =
            row.remove(global_id_ndc_field_name.as_str())
                .ok_or_else(|| error::InternalDeveloperError::BadGDCResponse {
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
/// wherever needed.
fn process_single_query_response_row<T>(
    mut row: T,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<IndexMap<ast::Alias, json::Value>, error::Error>
where
    T: KeyValueResponse,
{
    selection_set.as_object_selection_set(
        |type_name, field: &normalized_ast::Field<GDS>, field_call| {
            if field_call.name.as_str() == "__typename" {
                Ok(json::Value::String(type_name.to_string()))
            } else {
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
                                    error::InternalDeveloperError::TypenameMappingNotFound {
                                        type_name: type_name.clone(),
                                        mapping_kind: "Global ID",
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
                                let value =
                                    row.remove(field.alias.0.as_str()).ok_or_else(|| {
                                        error::InternalDeveloperError::BadGDCResponse {
                                            summary: format!(
                                                "missing field: {}",
                                                field.alias.clone()
                                            ),
                                        }
                                    })?;

                                if field.type_container.is_list() {
                                    process_field_selection_as_list(value, &field.selection_set)
                                } else {
                                    process_field_selection_as_object(value, &field.selection_set)
                                }
                            }
                            OutputAnnotation::RelationshipToModel { .. } => {
                                let field_json_value_result =
                                    row.remove(field.alias.0.as_str()).ok_or_else(|| {
                                        error::InternalDeveloperError::BadGDCResponse {
                                            summary: format!(
                                                "missing field: {}",
                                                field.alias.clone()
                                            ),
                                        }
                                    })?;
                                let relationship_json_value_result =
                                    serde_json::from_value(field_json_value_result).ok();
                                match relationship_json_value_result {
                                    None => Err(error::InternalDeveloperError::BadGDCResponse {
                                        summary: "Unable to parse RowSet".into(),
                                    })?,
                                    Some(rows_set) => {
                                        // Depending upon the field's type (list or object),
                                        // process the selection set accordingly.
                                        if field.type_container.is_list() {
                                            process_selection_set_as_list(
                                                rows_set,
                                                &field.selection_set,
                                            )
                                            .and_then(|v| Ok(json::to_value(v)?))
                                        } else {
                                            process_selection_set_as_object(
                                                rows_set,
                                                &field.selection_set,
                                            )
                                            .and_then(|v| Ok(json::to_value(v)?))
                                        }
                                    }
                                }
                            }
                            OutputAnnotation::RelationshipToCommand(
                                command_relationship_annotation,
                            ) => {
                                let field_json_value_result =
                                    row.remove(field.alias.0.as_str()).ok_or_else(|| {
                                        error::InternalDeveloperError::BadGDCResponse {
                                            summary: format!(
                                                "missing field: {}",
                                                field.alias.clone()
                                            ),
                                        }
                                    })?;
                                let relationship_json_value_result: Option<ndc_models::RowSet> =
                                    serde_json::from_value(field_json_value_result).ok();

                                match relationship_json_value_result {
                                    None => Err(error::InternalDeveloperError::BadGDCResponse {
                                        summary: "Unable to parse RowSet".into(),
                                    })?,
                                    Some(rows_set) => {
                                        // the output of a command is optional and can be None
                                        // so we match on the result and return null if the
                                        // command returned None
                                        process_command_rows(
                                            &command_relationship_annotation.command_name,
                                            rows_set.rows,
                                            &field.selection_set,
                                            &field.type_container,
                                        )
                                        .map(|v| match v {
                                            None => json::Value::Null,
                                            Some(v) => v,
                                        })
                                    }
                                }
                            }
                            _ => Err(error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            })?,
                        }
                    }
                    annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?,
                }
            }
        },
    )
}

pub fn process_selection_set_as_list(
    row_set: ndc_models::RowSet,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<Option<Vec<IndexMap<ast::Alias, json::Value>>>, error::Error> {
    let processed_response = row_set
        .rows
        .map(|rows| {
            rows.into_iter()
                .map(|row| process_single_query_response_row(row, selection_set))
                .collect()
        })
        .transpose()?;
    Ok(processed_response)
}

pub fn process_selection_set_as_object(
    row_set: ndc_models::RowSet,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<Option<IndexMap<ast::Alias, json::Value>>, error::Error> {
    let processed_response = row_set
        .rows
        .and_then(|rows| rows.into_iter().next())
        .map(|row| process_single_query_response_row(row, selection_set))
        .transpose()?;
    Ok(processed_response)
}

pub fn process_field_selection_as_list(
    value: json::Value,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<json::Value, error::Error> {
    if selection_set.fields.is_empty() || value.is_null() {
        // If selection set is empty we return the whole value without further processing.
        // If the value is null we have nothing to process so we return null.
        Ok(value)
    } else {
        let rows: Vec<IndexMap<String, ndc_models::RowFieldValue>> = json::from_value(value)?;
        let processed_rows: Vec<IndexMap<Alias, json::Value>> = rows
            .into_iter()
            .map(|row| process_single_query_response_row(row, selection_set))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(json::to_value(processed_rows)?)
    }
}

pub fn process_field_selection_as_object(
    value: json::Value,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<json::Value, error::Error> {
    if selection_set.fields.is_empty() || value.is_null() {
        // If selection set is empty we return the whole value without further processing.
        // If the value is null we have nothing to process so we return null.
        Ok(value)
    } else {
        let row: IndexMap<String, ndc_models::RowFieldValue> = json::from_value(value)?;
        let processed_row = process_single_query_response_row(row, selection_set)?;
        Ok(json::to_value(processed_row)?)
    }
}

pub fn process_command_rows(
    command_name: &Qualified<CommandName>,
    rows: Option<Vec<IndexMap<String, ndc_models::RowFieldValue, RandomState>>>,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    type_container: &TypeContainer<TypeName>,
) -> Result<Option<json::Value>, error::Error> {
    match rows {
        None => Err(error::InternalDeveloperError::BadGDCResponse {
            summary: format!(
                "Expected one row in the response for command, but no rows present: {}",
                command_name
            ),
        })?,
        Some(row_vector) => {
            if row_vector.len() > 1 {
                Err(error::InternalDeveloperError::BadGDCResponse {
                    summary: format!(
                        "Expected only one row in the response for command: {}",
                        command_name
                    ),
                })?;
            }

            let processed_response = row_vector
                .into_iter()
                .next()
                .map(|row| process_command_response_row(row, selection_set, type_container))
                .transpose()?;
            Ok(processed_response)
        }
    }
}

fn process_command_response_row(
    mut row: IndexMap<String, ndc_models::RowFieldValue>,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    type_container: &TypeContainer<TypeName>,
) -> Result<json::Value, error::Error> {
    let field_value_result = row
        .swap_remove(FUNCTION_IR_VALUE_COLUMN_NAME)
        .ok_or_else(|| error::InternalDeveloperError::BadGDCResponse {
            summary: format!("missing field: {}", FUNCTION_IR_VALUE_COLUMN_NAME),
        })?;

    process_command_field_value(field_value_result.0, selection_set, type_container)
}

pub fn process_command_mutation_response(
    mutation_result: ndc_models::MutationOperationResults,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    type_container: &TypeContainer<TypeName>,
) -> Result<json::Value, error::Error> {
    match mutation_result {
        ndc_models::MutationOperationResults::Procedure { result } => {
            process_command_field_value(result, selection_set, type_container)
        }
    }
}

fn process_command_field_value(
    field_value_result: serde_json::Value,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    type_container: &TypeContainer<TypeName>,
) -> Result<json::Value, error::Error> {
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
                if type_container.nullable {
                    Ok(json::Value::Null)
                } else {
                    Err(error::InternalDeveloperError::BadGDCResponse {
                        summary: "Unable to parse response from NDC, null value expected".into(),
                    })?
                }
            }
            json::Value::Object(result_map) => {
                if type_container.is_list() {
                    Err(error::InternalDeveloperError::BadGDCResponse {
                        summary: "Unable to parse response from NDC, object value expected".into(),
                    })?
                } else {
                    let index_map: IndexMap<String, json::Value> =
                        json::from_value(json::Value::Object(result_map))?;
                    let value = process_single_query_response_row(index_map, selection_set)?;
                    Ok(json::to_value(value)?)
                }
            }
            json::Value::Array(values) => {
                if type_container.is_list() {
                    let array_values: Vec<IndexMap<String, json::Value>> =
                        json::from_value(json::Value::Array(values))?;

                    let r: Vec<IndexMap<Alias, json::Value>> = array_values
                        .into_iter()
                        .map(|value| process_single_query_response_row(value, selection_set))
                        .collect::<Result<Vec<IndexMap<ast::Alias, json::Value>>, error::Error>>(
                        )?;

                    Ok(json::to_value(r)?)
                } else {
                    Err(error::InternalDeveloperError::BadGDCResponse {
                        summary: "Unable to parse response from NDC, array value expected".into(),
                    })?
                }
            }
            _ => Err(error::InternalDeveloperError::BadGDCResponse {
                summary:
                    "Unable to parse response from NDC, either null, object or array value expected"
                        .into(),
            })?,
        }
    }
}

pub fn process_response(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    rows_sets: Vec<ndc_models::RowSet>,
    process_response_as: &ProcessResponseAs,
) -> Result<json::Value, error::Error> {
    let tracer = tracing_util::global_tracer();
    // Post process the response to add the `__typename` fields
    tracer.in_span(
        "process_response",
        "Process response".into(),
        SpanVisibility::Internal,
        || {
            let row_set = get_single_rowset(rows_sets)?;
            match process_response_as {
                ProcessResponseAs::Array { .. } => {
                    let result = process_selection_set_as_list(row_set, selection_set)?;
                    json::to_value(result).map_err(error::Error::from)
                }
                ProcessResponseAs::Object { .. } => {
                    let result = process_selection_set_as_object(row_set, selection_set)?;
                    json::to_value(result).map_err(error::Error::from)
                }
                ProcessResponseAs::CommandResponse {
                    command_name,
                    type_container,
                } => {
                    let result = process_command_rows(
                        command_name,
                        row_set.rows,
                        selection_set,
                        type_container,
                    )?;
                    json::to_value(result).map_err(error::Error::from)
                }
            }
        },
    )
}

fn get_single_rowset(
    rows_sets: Vec<ndc_models::RowSet>,
) -> Result<ndc_models::RowSet, error::Error> {
    Ok(rows_sets
        .into_iter()
        .next()
        .ok_or(error::InternalDeveloperError::BadGDCResponse {
            summary: "missing rowset".into(),
        })?)
}
