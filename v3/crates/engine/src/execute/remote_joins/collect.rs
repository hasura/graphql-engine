//! Implements the collection phase of Remote Joins execution
//!
//! Collection phase is where engine receives a response from a connector, and
//! then traverses the response to collect all the values defined in the
//! relationship field mapping.

use indexmap::IndexMap;
use lang_graphql::ast::common::{TypeContainer, TypeName};
use ndc_models;
use serde_json as json;
use std::collections::BTreeMap;

use crate::execute::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use crate::execute::plan::ProcessResponseAs;
use crate::utils::json_ext::ValueExt;

use super::error;
use super::types::{
    Argument, Arguments, JoinId, JoinLocations, JoinNode, Location, LocationKind, MonotonicCounter,
    RemoteJoin, TargetField,
};

pub(crate) struct CollectArgumentResult<'s, 'ir> {
    pub(crate) arguments: Arguments,
    pub(crate) join_node: RemoteJoin<'s, 'ir>,
    pub(crate) sub_tree: JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    pub(crate) remote_alias: String,
}

/// Given a LHS response and `Location`, extract the join values from the
/// response and return it as the `Arguments` data structure.
pub(crate) fn collect_arguments<'s, 'ir>(
    lhs_response: &Vec<ndc_models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    key: &str,
    location: &Location<(RemoteJoin<'s, 'ir>, JoinId)>,
) -> Result<Option<CollectArgumentResult<'s, 'ir>>, error::Error> {
    if lhs_response.is_empty() {
        return Ok(None);
    }
    let mut arguments = Arguments::new();
    let mut argument_id_counter = MonotonicCounter::new();
    let mut remote_join = None;
    let mut sub_tree = JoinLocations::new();
    let mut remote_alias = String::new();

    for row_set in lhs_response {
        if let Some(ref rows) = row_set.rows {
            for row in rows.iter() {
                match lhs_response_type {
                    ProcessResponseAs::Array { .. } | ProcessResponseAs::Object { .. } => {
                        collect_argument_from_row(
                            row,
                            key,
                            location,
                            &mut arguments,
                            &mut argument_id_counter,
                            &mut remote_join,
                            &mut sub_tree,
                            &mut remote_alias,
                        )?;
                    }
                    ProcessResponseAs::CommandResponse {
                        command_name: _,
                        type_container,
                    } => {
                        let mut command_rows = resolve_command_response_row(row, type_container)?;
                        for command_row in command_rows.iter_mut() {
                            collect_argument_from_row(
                                command_row,
                                key,
                                location,
                                &mut arguments,
                                &mut argument_id_counter,
                                &mut remote_join,
                                &mut sub_tree,
                                &mut remote_alias,
                            )?;
                        }
                    }
                }
            }
        }
    }
    match (remote_join, arguments.is_empty()) {
        (None, true) => Ok(None),
        (None, false) => Err(error::Error::from(
            error::InternalEngineError::InternalGeneric {
                description: "unexpected: remote join empty".to_string(),
            },
        )),
        (Some(remote_join), _) => Ok(Some(CollectArgumentResult {
            arguments,
            join_node: remote_join,
            sub_tree,
            remote_alias,
        })),
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_argument_from_row<'s, 'ir>(
    row: &IndexMap<String, ndc_models::RowFieldValue>,
    key: &str,
    location: &Location<(RemoteJoin<'s, 'ir>, JoinId)>,
    arguments: &mut Arguments,
    argument_id_counter: &mut MonotonicCounter,
    remote_join: &mut Option<RemoteJoin<'s, 'ir>>,
    sub_tree: &mut JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    remote_alias: &mut String,
) -> Result<(), error::Error> {
    if location.join_node.is_local() && location.rest.locations.is_empty() {
        return Err(error::Error::from(
            error::InternalEngineError::InternalGeneric {
                description: "unexpected: join_node and locations tree both are empty".to_string(),
            },
        ));
    }
    match &location.join_node {
        JoinNode::Remote((join_node, _join_id)) => {
            let argument = create_argument(join_node, row);
            // de-duplicate arguments
            if let std::collections::hash_map::Entry::Vacant(e) = arguments.entry(argument) {
                let argument_id = argument_id_counter.get_next();
                e.insert(argument_id);
            }
            *remote_join = Some(join_node.clone());
            *sub_tree = location.rest.clone();
            *remote_alias = key.to_string();
            Ok(())
        }
        JoinNode::Local(location_kind) => {
            let nested_val = row
                .get(key)
                .ok_or(error::InternalEngineError::InternalGeneric {
                    description: "invalid NDC response; could not find {key} in response"
                        .to_string(),
                })?;

            let rows = rows_from_row_field_value(*location_kind, nested_val)?;
            if let Some(mut rows) = rows {
                for (sub_key, sub_location) in &location.rest.locations {
                    for row in rows.iter_mut() {
                        collect_argument_from_row(
                            row,
                            sub_key,
                            sub_location,
                            arguments,
                            argument_id_counter,
                            remote_join,
                            sub_tree,
                            remote_alias,
                        )?;
                    }
                }
            }
            Ok(())
        }
    }
}

pub(crate) fn create_argument(
    join_node: &RemoteJoin,
    row: &IndexMap<String, ndc_models::RowFieldValue>,
) -> Argument {
    let mut argument = BTreeMap::new();
    for (src_alias, target_field) in join_node.join_mapping.values() {
        match target_field {
            TargetField::ModelField((_, field_mapping)) => {
                let val = get_value(src_alias, row);
                // use the target field name here to create the variable
                // name to be used in RHS
                let variable_name = format!("${}", &field_mapping.column);
                argument.insert(variable_name, ValueExt::from(val.clone()));
            }
            TargetField::CommandField(argument_name) => {
                let val = get_value(src_alias, row);
                // use the target field name here to create the variable
                // name to be used in RHS
                let variable_name = format!("${}", &argument_name);
                argument.insert(variable_name, ValueExt::from(val.clone()));
            }
        }
    }
    argument
}

fn rows_from_row_field_value(
    location_kind: LocationKind,
    nested_val: &ndc_models::RowFieldValue,
) -> Result<Option<Vec<IndexMap<String, ndc_models::RowFieldValue>>>, error::Error> {
    let rows: Option<Vec<IndexMap<String, ndc_models::RowFieldValue>>> = match location_kind {
        LocationKind::NestedData => Some(
            {
                let this = nested_val.clone();
                match this.0 {
                    serde_json::Value::Array(_) => serde_json::from_value(this.0).ok(),
                    serde_json::Value::Object(_) => {
                        serde_json::from_value(this.0).ok().map(|v| vec![v])
                    }
                    serde_json::Value::Null => Some(vec![]),
                    _ => None,
                }
            }
            .ok_or(error::InternalEngineError::InternalGeneric {
                description: "unexpected: could not find rows in NDC nested response: ".to_string()
                    + &nested_val.0.to_string(),
            }),
        )
        .transpose(),
        LocationKind::LocalRelationship => {
            // Get the NDC response with nested selection (i.e. in case of
            // relationships) as a RowSet
            let row_set = nested_val
                // TODO: remove clone -> depends on ndc-client providing an API e.g. as_mut_rowset()
                .clone()
                .as_rowset()
                .ok_or(error::InternalEngineError::InternalGeneric {
                    description: "unexpected: could not find RowSet in NDC nested response: "
                        .to_string()
                        + &nested_val.0.to_string(),
                })?;
            Ok(row_set.rows)
        }
    }?;
    Ok(rows)
}

pub(crate) fn get_value<'n>(
    pick_alias: &String,
    row: &'n IndexMap<String, ndc_models::RowFieldValue>,
) -> &'n json::Value {
    match row.get(pick_alias) {
        Some(v) => &v.0,
        None => &json::Value::Null,
    }
}

/// resolve/process the command response for remote join execution
fn resolve_command_response_row(
    row: &IndexMap<String, ndc_models::RowFieldValue>,
    type_container: &TypeContainer<TypeName>,
) -> Result<Vec<IndexMap<String, ndc_models::RowFieldValue>>, error::Error> {
    let field_value_result = row.get(FUNCTION_IR_VALUE_COLUMN_NAME).ok_or_else(|| {
        error::InternalDeveloperError::BadGDCResponse {
            summary: format!("missing field: {}", FUNCTION_IR_VALUE_COLUMN_NAME),
        }
    })?;

    // If the command has a selection set, then the structure of the
    // response should either be a `Array <Object>` or `<Object>` or null,
    // where `<Object>` is the map of the selection set field and it's
    // value.
    match &field_value_result.0 {
        json::Value::String(_) | json::Value::Bool(_) | json::Value::Number(_) => {
            Err(error::InternalDeveloperError::BadGDCResponse {
                summary: "Unable to parse response from NDC, object or array value expected for relationship".into(),
            })?
        }
        json::Value::Null => {
            if type_container.nullable {
                Ok(Vec::new())
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
                let index_map: IndexMap<String, ndc_models::RowFieldValue> =
                    json::from_value(json::Value::Object(result_map.clone()))?;
                Ok(vec![index_map])
            }
        }
        json::Value::Array(values) => {
            // There can be cases when the command returns an array of objects,
            // but the type container is not a list. This can happen when the
            // command is used in a relationship whose RHS returns a list of objects
            // which can have the same value for the field on which the relationship
            // is defined.
            // In case the container is not a list, we take the first object from
            // the array and use that as the value for the relationship otherwise
            // we return the array of objects.
            let array_values: Vec<IndexMap<String, ndc_models::RowFieldValue>> =
                    json::from_value(json::Value::Array(values.to_vec()))?;

            if type_container.is_list(){
                Ok(array_values)
            } else {
                Ok(vec![array_values.into_iter().next().ok_or(error::InternalDeveloperError::BadGDCResponse {
                    summary: "Unable to parse response from NDC, rowset is empty".into(),
                })?])
            }
        }
    }
}
