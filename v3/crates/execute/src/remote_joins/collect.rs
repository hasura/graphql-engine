//! Implements the collection phase of Remote Joins execution
//!
//! Collection phase is where engine receives a response from a connector, and
//! then traverses the response to collect all the values defined in the
//! relationship field mapping.

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use nonempty::NonEmpty;
use serde_json as json;
use std::collections::{BTreeMap, HashSet};

use json_ext::ValueExt;

use super::error;
use super::types::{
    Argument, JoinId, JoinLocations, JoinNode, LocationKind, RemoteJoin, SourceFieldAlias,
    TargetField,
};
use crate::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use crate::plan::ProcessResponseAs;
use ir::VariableName;

/// An executable join node is a remote join node, it's collected join values
/// from a LHS response, and the rest of the join sub-tree
#[derive(Debug)]
pub(crate) struct ExecutableJoinNode<'s, 'ir> {
    pub(crate) join_node: RemoteJoin<'s, 'ir>,
    pub(crate) remote_alias: String,
    pub(crate) location_path: Vec<LocationInfo>,
    pub(crate) arguments: HashSet<Argument>,
    pub(crate) sub_tree: JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
}

/// Indicates a field alias which might have more nesting inside
#[derive(Clone, Debug)]
pub(crate) struct LocationInfo {
    pub(crate) alias: String,
    pub(crate) location_kind: LocationKind,
}

/// Given a LHS response and `JoinLocations` tree, get the next executable join
/// nodes down the tree. Also, extract the join values from the response.
pub(crate) fn collect_next_join_nodes<'s, 'ir>(
    lhs_response: &Vec<ndc_models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    join_locations: &JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    path: &mut [LocationInfo],
) -> Result<Vec<ExecutableJoinNode<'s, 'ir>>, error::FieldError> {
    let mut arguments_results = Vec::new();

    // if lhs_response is empty, there are no rows to collect arguments from
    if lhs_response.is_empty() {
        return Ok(arguments_results);
    }

    for (alias, location) in &join_locations.locations {
        match &location.join_node {
            JoinNode::Remote((join_node, _join_id)) => {
                let join_fields = get_join_fields(join_node);
                let arguments = collect_argument_from_rows(
                    lhs_response,
                    lhs_response_type,
                    &join_fields,
                    path,
                )?;
                arguments_results.push(ExecutableJoinNode {
                    arguments,
                    location_path: path.to_owned(),
                    join_node: join_node.clone(),
                    sub_tree: location.rest.clone(),
                    remote_alias: alias.clone(),
                });
            }
            JoinNode::Local(location_kind) => {
                let mut new_path = path.to_owned();
                new_path.push(LocationInfo {
                    alias: alias.clone(),
                    location_kind: *location_kind,
                });
                let inner_arguments_by_remote = collect_next_join_nodes(
                    lhs_response,
                    lhs_response_type,
                    &location.rest,
                    &mut new_path,
                )?;
                arguments_results.extend(inner_arguments_by_remote);
            }
        }
    }
    Ok(arguments_results)
}

/// Iterate over the `Vec<RowSet>` structure to get to each row, and collect
/// arguments from each row
fn collect_argument_from_rows(
    lhs_response: &Vec<ndc_models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    join_fields: &Vec<(&SourceFieldAlias, VariableName)>,
    path: &[LocationInfo],
) -> Result<HashSet<Argument>, error::FieldError> {
    let mut arguments = HashSet::new();
    for row_set in lhs_response {
        if let Some(ref rows) = row_set.rows {
            for row in rows {
                match lhs_response_type {
                    ProcessResponseAs::Array { .. } | ProcessResponseAs::Object { .. } => {
                        collect_argument_from_row(row, join_fields, path, &mut arguments)?;
                    }
                    ProcessResponseAs::Aggregates { .. } => {
                        return Err(error::FieldInternalError::InternalGeneric {
                            description:
                                "Unexpected aggregate response on the LHS of a remote join"
                                    .to_owned(),
                        }
                        .into())
                    }
                    ProcessResponseAs::CommandResponse {
                        command_name: _,
                        type_container,
                        response_config: _,
                    } => {
                        let mut command_rows = resolve_command_response_row(row, type_container)?;
                        for command_row in &mut command_rows {
                            collect_argument_from_row(
                                command_row,
                                join_fields,
                                path,
                                &mut arguments,
                            )?;
                        }
                    }
                }
            }
        }
    }
    Ok(arguments)
}

/// From each row gather arguments based on join fields
fn collect_argument_from_row(
    row: &IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
    join_fields: &Vec<(&SourceFieldAlias, VariableName)>,
    path: &[LocationInfo],
    arguments: &mut HashSet<Argument>,
) -> Result<(), error::FieldError> {
    match NonEmpty::from_slice(path) {
        None => {
            let argument = create_argument(join_fields, row);
            // de-duplicate arguments
            arguments.insert(argument);
        }
        Some(nonempty_path) => {
            let (
                LocationInfo {
                    alias,
                    location_kind,
                },
                path_tail,
            ) = nonempty_path.split_first();
            let nested_val =
                row.get(alias.as_str())
                    .ok_or(error::FieldInternalError::InternalGeneric {
                        description: "invalid NDC response; could not find {key} in response"
                            .to_string(),
                    })?;
            if let Some(parsed_rows) = rows_from_row_field_value(*location_kind, nested_val)? {
                for inner_row in parsed_rows {
                    collect_argument_from_row(&inner_row, join_fields, path_tail, arguments)?;
                }
            }
        }
    }
    Ok(())
}

/// Get all the field aliases in LHS used for the join (i.e. fields used in the
/// Relationship mapping), and also the variables used in the RHS IR
pub(crate) fn get_join_fields<'ir>(
    join_node: &'ir RemoteJoin,
) -> Vec<(&'ir SourceFieldAlias, VariableName)> {
    let mut join_fields = vec![];
    for (src_alias, target_field) in join_node.join_mapping.values() {
        match target_field {
            TargetField::ModelField((_, field_mapping)) => {
                // use the target field name here to create the variable
                // name to be used in RHS
                let variable_name = VariableName(format!("${}", &field_mapping.column));
                join_fields.push((src_alias, variable_name));
            }
            TargetField::CommandField(argument_name) => {
                // use the target argument name here to create the variable
                // name to be used in RHS
                let variable_name = VariableName(format!("${}", &argument_name));
                join_fields.push((src_alias, variable_name));
            }
        }
    }
    join_fields
}

/// From a row, given the join fields, collect the values of the fields and
/// return them as 'Argument'
pub(crate) fn create_argument(
    join_fields: &Vec<(&SourceFieldAlias, VariableName)>,
    row: &IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
) -> Argument {
    let mut argument = BTreeMap::new();
    for (src_alias, variable_name) in join_fields {
        let val = get_value(src_alias, row);
        argument.insert(variable_name.clone(), ValueExt::from(val.clone()));
    }
    argument
}

pub(crate) fn get_value<'n>(
    pick_alias: &SourceFieldAlias,
    row: &'n IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
) -> &'n json::Value {
    match row.get(pick_alias.0.as_str()) {
        Some(v) => &v.0,
        None => &json::Value::Null,
    }
}

fn rows_from_row_field_value(
    location_kind: LocationKind,
    nested_val: &ndc_models::RowFieldValue,
) -> Result<
    Option<Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>>,
    error::FieldError,
> {
    let rows: Option<Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>> =
        match location_kind {
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
                .ok_or_else(|| error::FieldInternalError::InternalGeneric {
                    description: "unexpected: could not find rows in NDC nested response: "
                        .to_string()
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
                    .ok_or_else(|| error::FieldInternalError::InternalGeneric {
                        description: "unexpected: could not find RowSet in NDC nested response: "
                            .to_string()
                            + &nested_val.0.to_string(),
                    })?;
                Ok(row_set.rows)
            }
        }?;
    Ok(rows)
}

/// resolve/process the command response for remote join execution
fn resolve_command_response_row(
    row: &IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>,
    type_container: &ast::TypeContainer<ast::TypeName>,
) -> Result<Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>, error::FieldError> {
    let field_value_result = row.get(FUNCTION_IR_VALUE_COLUMN_NAME).ok_or_else(|| {
        error::NDCUnexpectedError::BadNDCResponse {
            summary: format!("missing field: {FUNCTION_IR_VALUE_COLUMN_NAME}"),
        }
    })?;

    // If the command has a selection set, then the structure of the
    // response should either be a `Array <Object>` or `<Object>` or null,
    // where `<Object>` is the map of the selection set field and it's
    // value.
    match &field_value_result.0 {
        json::Value::String(_) | json::Value::Bool(_) | json::Value::Number(_) => {
            Err(error::NDCUnexpectedError::BadNDCResponse {
                summary: "Unable to parse response from NDC, object or array value expected for relationship".into(),
            })?
        }
        json::Value::Null => {
            if type_container.nullable {
                Ok(Vec::new())
            } else {
                Err(error::NDCUnexpectedError::BadNDCResponse {
                    summary: "Unable to parse response from NDC, null value expected".into(),
                })?
            }
        }
        json::Value::Object(result_map) => {
            if type_container.is_list() {
                Err(error::NDCUnexpectedError::BadNDCResponse {
                    summary: "Unable to parse response from NDC, object value expected".into(),
                })?
            } else {
                let index_map: IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue> =
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
            let array_values: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>> =
                    json::from_value(json::Value::Array(values.clone()))?;

            if type_container.is_list(){
                Ok(array_values)
            } else {
                Ok(vec![array_values.into_iter().next().ok_or(error::NDCUnexpectedError::BadNDCResponse {
                    summary: "Unable to parse response from NDC, rowset is empty".into(),
                })?])
            }
        }
    }
}
