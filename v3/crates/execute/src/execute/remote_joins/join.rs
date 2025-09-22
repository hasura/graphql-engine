//! Implements the join phase of Remote Joins execution

use indexmap::IndexMap;
use ndc_models::{self};
use nonempty::NonEmpty;
use serde_json as json;
use std::collections::HashMap;

use plan_types::{FUNCTION_IR_VALUE_COLUMN_NAME, ProcessResponseAs};

use super::collect::LocationInfo;
use super::{collect, error};
use plan_types::{LocationKind, RemoteJoin, RemoteJoinVariableSet};

/// Inserts values in the LHS response from values in RHS response, based on the
/// mapping field
///
/// Lookup the argument in `rhs_response` and substitute that value in
/// `lhs_response`
pub(crate) fn join_responses(
    location_path: &[LocationInfo],
    join_node: &RemoteJoin,
    remote_alias: &str,
    lhs_response: &mut [ndc_models::RowSet],
    lhs_response_type: &ProcessResponseAs,
    rhs_response: &HashMap<RemoteJoinVariableSet, ndc_models::RowSet>,
) -> Result<(), error::FieldError> {
    for row_set in lhs_response.iter_mut() {
        if let Some(rows) = row_set.rows.as_mut() {
            for row in rows.iter_mut() {
                // TODO: have a better interface of traversing through the
                // response tree, especially for commands
                let command_result_value = row.get_mut(FUNCTION_IR_VALUE_COLUMN_NAME);
                match command_result_value {
                    // it's a command response; traversing the response tree is
                    // different
                    Some(row_field_value) => join_command_response(
                        location_path,
                        lhs_response_type,
                        join_node,
                        remote_alias,
                        row_field_value,
                        rhs_response,
                    )?,
                    None => {
                        insert_value_into_row(
                            location_path,
                            join_node,
                            row,
                            ndc_models::FieldName::from(remote_alias),
                            rhs_response,
                        )?;
                    }
                }
            }
        }
    }
    Ok(())
}

/// In case of a command response, the response tree traversing is different.
/// This helper function handles only that case.
///
/// NDC returns response to a command in a special column name
/// 'FUNCTION_IR_VALUE_COLUMN_NAME', which is a serde::json::Value. We
/// destructure the serde value, handle the Object and Array case to insert the
/// RHS response appropriately.
fn join_command_response(
    location_path: &[LocationInfo],
    lhs_response_type: &ProcessResponseAs,
    join_node: &RemoteJoin,
    remote_alias: &str,
    row_field_value: &mut ndc_models::RowFieldValue,
    rhs_response: &HashMap<RemoteJoinVariableSet, ndc_models::RowSet>,
) -> Result<(), error::FieldError> {
    // we may need to unwrap the response to remove any headers
    let target_field = match lhs_response_type {
        plan_types::ProcessResponseAs::CommandResponse {
            response_config: Some(commands_response_config),
            ..
        } => {
            let response_field = commands_response_config.result_field.as_str();
            let headers_field = commands_response_config.headers_field.as_str();

            // if the response and headers fields are present, we need to unwrap
            if row_field_value.0.get(response_field).is_some()
                && row_field_value.0.get(headers_field).is_some()
            {
                row_field_value
                .0
                .get_mut(response_field)
                .ok_or_else(|| error::NDCUnexpectedError::BadNDCResponse {
                    summary: format!("While processing remote join response, expected a response field '{response_field}' in the response, but it was not found"),
                })?
            } else {
                &mut row_field_value.0
            }
        }
        _ => &mut row_field_value.0,
    };

    match target_field {
        json::Value::Array(arr) => {
            for command_row in arr.iter_mut() {
                let new_val = command_row.clone();
                let mut command_row_parsed: IndexMap<
                    ndc_models::FieldName,
                    ndc_models::RowFieldValue,
                > = json::from_value(new_val)?;
                insert_value_into_row(
                    location_path,
                    join_node,
                    &mut command_row_parsed,
                    ndc_models::FieldName::from(remote_alias),
                    rhs_response,
                )?;
                *command_row = json::to_value(command_row_parsed)?;
            }
        }
        json::Value::Object(obj) => {
            // Build a mutable row map from the current object without moving out of it
            let mut command_row: IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue> = obj
                .iter()
                .map(|(k, v)| {
                    (
                        ndc_models::FieldName::from(k.as_str()),
                        ndc_models::RowFieldValue(v.clone()),
                    )
                })
                .collect();

            // Insert the RHS value into this row
            insert_value_into_row(
                location_path,
                join_node,
                &mut command_row,
                ndc_models::FieldName::from(remote_alias),
                rhs_response,
            )?;

            let command_row_json = json::to_value(command_row)?;

            // Write the updated row back into the target field (preserving any outer wrapper like headers)
            if let json::Value::Object(new_obj) = command_row_json {
                *obj = new_obj;
            } else {
                return Err(error::FieldError::from(
                    error::FieldInternalError::InternalGeneric {
                        description: format!(
                            "unexpected command response: {command_row_json}; Object"
                        ),
                    },
                ));
            }
        }
        command_json_val => {
            return Err(error::FieldError::from(
                error::FieldInternalError::InternalGeneric {
                    description: format!(
                        "unexpected command response: {command_json_val}; expected Array or Object"
                    ),
                },
            ));
        }
    }
    Ok(())
}

/// Traverse 'LocationInfo' and insert corresponding RHS response value in a LHS
/// response row.
fn insert_value_into_row(
    location_path: &[LocationInfo],
    join_node: &RemoteJoin,
    row: &mut IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
    remote_alias: ndc_models::FieldName,
    rhs_response: &HashMap<RemoteJoinVariableSet, ndc_models::RowSet>,
) -> Result<(), error::FieldError> {
    match NonEmpty::from_slice(location_path) {
        // no location path; so remote join available at this level
        None => {
            let join_fields = collect::get_join_fields(join_node);
            let argument = collect::extract_variable_set(&join_fields, row);
            let rhs_value = json::to_value(rhs_response.get(&argument))?;
            row.insert(remote_alias, ndc_models::RowFieldValue(rhs_value));
            Ok(())
        }
        // if there is a location path, traverse the location path to get to
        // nested rows, and insert value
        Some(nonempty_path) => {
            let (
                LocationInfo {
                    alias,
                    location_kind,
                },
                path_tail,
            ) = nonempty_path.split_first();
            let row_field_val =
                row.get_mut(alias.as_str())
                    .ok_or(error::FieldInternalError::InternalGeneric {
                        description: "unexpected: could not find {key} in row".into(),
                    })?;
            visit_location_path_and_insert_value(
                *location_kind,
                row_field_val,
                path_tail,
                join_node,
                remote_alias,
                rhs_response,
            )?;
            Ok(())
        }
    }
}

/// If there is a location path (i.e. the remote join is nested in a
/// relationship or nested selection), traverse the location path of LHS row,
/// and insert the RHS response.
fn visit_location_path_and_insert_value(
    location_kind: LocationKind,
    row_field_val: &mut ndc_models::RowFieldValue,
    path_tail: &[LocationInfo],
    join_node: &RemoteJoin,
    remote_alias: ndc_models::FieldName,
    rhs_response: &HashMap<RemoteJoinVariableSet, ndc_models::RowSet>,
) -> Result<(), error::FieldError> {
    match location_kind {
        LocationKind::LocalRelationship => {
            let mut row_set: ndc_models::RowSet = json::from_value(row_field_val.0.clone())?;
            let mut rows = row_set
                .rows
                .ok_or(error::FieldInternalError::InternalGeneric {
                    description: "expected row; encountered null".into(),
                })?;

            for inner_row in &mut rows {
                insert_value_into_row(
                    path_tail,
                    join_node,
                    inner_row,
                    remote_alias.clone(),
                    rhs_response,
                )?;
            }
            row_set.rows = Some(rows);
            *row_field_val = ndc_models::RowFieldValue(json::to_value(row_set)?);
        }
        LocationKind::NestedData => match row_field_val.0 {
            serde_json::Value::Array(_) => {
                if let Ok(mut rows) = serde_json::from_value::<
                    Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>,
                >(row_field_val.0.clone())
                {
                    for inner_row in &mut rows {
                        insert_value_into_row(
                            path_tail,
                            join_node,
                            inner_row,
                            remote_alias.clone(),
                            rhs_response,
                        )?;
                    }
                    *row_field_val = ndc_models::RowFieldValue(json::to_value(rows)?);
                }
            }
            serde_json::Value::Object(_) => {
                if let Ok(mut inner_row) = serde_json::from_value::<
                    IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
                >(row_field_val.0.clone())
                {
                    insert_value_into_row(
                        path_tail,
                        join_node,
                        &mut inner_row,
                        remote_alias,
                        rhs_response,
                    )?;
                    *row_field_val = ndc_models::RowFieldValue(json::to_value(inner_row)?);
                }
            }
            _ => (),
        },
    }
    Ok(())
}
