//! Implements the join phase of Remote Joins execution

use indexmap::IndexMap;
use ndc_models;
use serde_json as json;
use std::collections::{BTreeMap, HashMap};

use crate::execute::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use crate::utils::json_ext::ValueExt;

use super::types::{Argument, JoinId, JoinNode, Location, LocationKind, RemoteJoin};
use super::{collect, error};

/// Inserts values in the LHS response from values in RHS response, based on the
/// mapping field
///
/// Lookup the argument in `rhs_response` and substitute that value in
/// `lhs_response`
pub(crate) fn join_responses(
    alias: &str,
    remote_alias: &str,
    location: &Location<(RemoteJoin<'_, '_>, JoinId)>,
    lhs_response: &mut [ndc_models::RowSet],
    rhs_response: &HashMap<BTreeMap<String, ValueExt>, ndc_models::RowSet>,
) -> Result<(), error::Error> {
    for row_set in lhs_response.iter_mut() {
        if let Some(rows) = row_set.rows.as_mut() {
            for row in rows.iter_mut() {
                // TODO: have a better interface of traversing through the
                // response tree, especially for commands
                let command_result_value = row.get_mut(FUNCTION_IR_VALUE_COLUMN_NAME);
                match command_result_value {
                    // it's a command response; traversing the response tree is
                    // different
                    Some(x) => match &mut x.0 {
                        json::Value::Array(ref mut arr) => {
                            for command_row in arr.iter_mut() {
                                let new_val = command_row.clone();
                                let mut command_row_parsed: IndexMap<
                                    String,
                                    ndc_models::RowFieldValue,
                                > = json::from_value(new_val)?;
                                follow_location_and_insert_value(
                                    location,
                                    &mut command_row_parsed,
                                    remote_alias.to_owned(),
                                    alias,
                                    rhs_response,
                                )?;
                                *command_row = json::to_value(command_row_parsed)?;
                            }
                        }
                        json::Value::Object(obj) => {
                            let mut command_row = obj
                                .into_iter()
                                .map(|(k, v)| (k.clone(), ndc_models::RowFieldValue(v.clone())))
                                .collect();
                            follow_location_and_insert_value(
                                location,
                                &mut command_row,
                                remote_alias.to_owned(),
                                alias,
                                rhs_response,
                            )?;
                            *x = ndc_models::RowFieldValue(json::to_value(command_row)?);
                        }
                        command_json_val => {
                            return Err(error::Error::from(
                                error::InternalEngineError::InternalGeneric {
                                    description: format!(
                                        "unexpected command response: {}; expected Array or Object",
                                        command_json_val
                                    ),
                                },
                            ));
                        }
                    },
                    None => {
                        follow_location_and_insert_value(
                            location,
                            row,
                            remote_alias.to_owned(),
                            alias,
                            rhs_response,
                        )?;
                    }
                };
            }
        }
    }
    Ok(())
}

/// Walk the 'Location' tree and insert corresponding `rhs_response` value in
/// the `row`
fn follow_location_and_insert_value(
    location: &Location<(RemoteJoin<'_, '_>, JoinId)>,
    row: &mut IndexMap<String, ndc_models::RowFieldValue>,
    remote_alias: String,
    key: &str,
    rhs_response: &HashMap<Argument, ndc_models::RowSet>,
) -> Result<(), error::Error> {
    match &location.join_node {
        JoinNode::Remote((join_node, _join_id)) => {
            let argument = collect::create_argument(join_node, row);
            let rhs_value = json::to_value(rhs_response.get(&argument))?;
            row.insert(remote_alias, ndc_models::RowFieldValue(rhs_value));
            Ok(())
        }
        JoinNode::Local(location_kind) => {
            let row_field_val =
                row.get_mut(key)
                    .ok_or(error::InternalEngineError::InternalGeneric {
                        description: "unexpected: could not find {key} in row".into(),
                    })?;
            match location_kind {
                LocationKind::LocalRelationship => {
                    let mut row_set: ndc_models::RowSet =
                        json::from_value(row_field_val.0.clone())?;
                    let mut rows =
                        row_set
                            .rows
                            .ok_or(error::InternalEngineError::InternalGeneric {
                                description: "expected row; encountered null".into(),
                            })?;

                    for inner_row in rows.iter_mut() {
                        for (sub_key, sub_location) in &location.rest.locations {
                            follow_location_and_insert_value(
                                sub_location,
                                inner_row,
                                remote_alias.to_string(),
                                sub_key,
                                rhs_response,
                            )?;
                        }
                    }
                    row_set.rows = Some(rows);
                    *row_field_val = ndc_models::RowFieldValue(json::to_value(row_set)?);
                }
                LocationKind::NestedData => {
                    match row_field_val.0 {
                        serde_json::Value::Array(_) => {
                            if let Ok(mut rows) =
                                serde_json::from_value::<
                                    Vec<IndexMap<String, ndc_models::RowFieldValue>>,
                                >(row_field_val.0.clone())
                            {
                                for inner_row in rows.iter_mut() {
                                    for (sub_key, sub_location) in &location.rest.locations {
                                        follow_location_and_insert_value(
                                            sub_location,
                                            inner_row,
                                            remote_alias.to_string(),
                                            sub_key,
                                            rhs_response,
                                        )?
                                    }
                                }
                                *row_field_val = ndc_models::RowFieldValue(json::to_value(rows)?);
                            }
                        }
                        serde_json::Value::Object(_) => {
                            if let Ok(mut inner_row) = serde_json::from_value::<
                                IndexMap<String, ndc_models::RowFieldValue>,
                            >(
                                row_field_val.0.clone()
                            ) {
                                for (sub_key, sub_location) in &location.rest.locations {
                                    follow_location_and_insert_value(
                                        sub_location,
                                        &mut inner_row,
                                        remote_alias.to_string(),
                                        sub_key,
                                        rhs_response,
                                    )?
                                }
                                *row_field_val =
                                    ndc_models::RowFieldValue(json::to_value(inner_row)?);
                            }
                        }
                        _ => (),
                    };
                }
            }
            Ok(())
        }
    }
}
