//! Implements execution of Remote Joins
//!
//! # Introduction
//! "Remote Join" is a feature where engine can fetch data from different data
//! connectors, and based on some field mapping, can
//! [join](https://cghlewis.com/blog/joins/) the dataset from the two different
//! data sources.
//!
//! Let's consider the following simple example. Consider there are two data
//! connectors -
//!   1. Geographical information about cities in `connector A`
//!
//! `cities (name text, code text)`
//!
//!   2. Weather data about cities in `connector B`
//!
//! `weather (city_code text, temperature float, wind_speed float, humidity float)`
//!
//!
//! Given the above, and assuming there are corresponding models defined for
//! these two tables in the metadata, one can define a [remote
//! relationship](https://hasura.io/docs/3.0/supergraph-modeling/relationships/).
//! In the above two tables, the "city code" is guaranteed to be a unique field.
//! So one can use that as the mapping field and define a relationship.
//!
//! Once a relationship is defined, one can make a GraphQL query like -
//!
//! ```graphql
//! city {
//!   name
//!   weather {
//!     temperature
//!     humidity
//!   }
//! }
//! ```
//! #### Note
//! There has to be one or more fields across the two datasets which are unique.
//! The remote relationship mapping should be defined using those fields. If
//! there are no collection of unique fields, then a remote relationship cannot
//! be defined.
//!
//! # How it works
//!
//! ## IR generation
//! - Selection set IR generation function - [generate_selection_set_ir]
//! - Model Remote relationship - [build_remote_relationship]
//! - Command Remote relationship - [build_remote_command_relationship]
//!
//! ## Join Tree generation
//! - The join tree is generated as part of query plan. See [process_selection_set_ir] function.
//! - To know about the types for remote joins, see the [types] module.
//!
//! ## Execution
//! Following is the high-level algorithm how remote joins execution is performed.
//!
//! 1. Make the first top-level NDC query, and call the response as LHS response.
//!
//! 2. Traverse the join tree to get to the next remote join. Iterate through
//! all the rows in the LHS response, use the field mapping in the remote join node
//! to collect the values of those fields. In the above example, these would be
//! collecting the city codes from the LHS city query.
//!
//! 3. Get the NDC query from the remote join node, and attach the values in the
//! above step as variables in the NDC query. This NDC query already has a
//! "where" filter clause with a variable on the join mapping field. Make the
//! NDC query, and call the response as RHS response.
//!
//! 4. If there is a sub-tree from this remote join node, recursively perform
//! this algorithm.
//!
//! 5. Perform join on LHS response and RHS response
//!
//! [process_selection_set_ir]: crate::execute::query_plan::selection_set::process_selection_set_ir
//! [generate_selection_set_ir]: crate::execute::ir::selection_set::generate_selection_set_ir
//! [build_remote_relationship]: crate::execute::ir::relationship::build_remote_relationship
//! [build_remote_command_relationship]: crate::execute::ir::relationship::build_remote_command_relationship

use crate::utils::json_ext::ValueExt;
use async_recursion::async_recursion;
use indexmap::IndexMap;
use lang_graphql::ast::common::{TypeContainer, TypeName};
use serde_json as json;
use std::collections::{BTreeMap, HashMap};
use tracing_util::SpanVisibility;

use ndc_client as ndc;

use self::types::{JoinNode, LocationKind, TargetField};

use super::ndc::{execute_ndc_query, FUNCTION_IR_VALUE_COLUMN_NAME};
use super::query_plan::ProcessResponseAs;
use super::{error, ProjectId};

use types::{
    ArgumentId, Arguments, JoinId, JoinLocations, Location, MonotonicCounter, RemoteJoin,
    ReplacementToken, ReplacementTokenRows,
};

pub(crate) mod types;

/// Execute remote joins. As an entry-point it assumes the response is available
/// for the top-level query, and executes further remote joins recursively.
#[async_recursion]
pub async fn execute_join_locations<'ir>(
    http_client: &reqwest::Client,
    execution_span_attribute: String,
    field_span_attribute: String,
    lhs_response: &mut Vec<ndc::models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    join_locations: JoinLocations<(RemoteJoin<'async_recursion, 'ir>, JoinId)>,
    project_id: Option<ProjectId>,
) -> Result<(), error::Error>
where
    'ir: 'async_recursion,
{
    let tracer = tracing_util::global_tracer();
    for (key, location) in join_locations.locations {
        // collect the join column arguments from the LHS response, also get
        // the replacement tokens
        let mut arguments = Arguments::new();
        let collect_arg_res =
            tracer.in_span("collect_arguments", SpanVisibility::Internal, || {
                collect_arguments(
                    lhs_response,
                    lhs_response_type,
                    &key,
                    &location,
                    &mut arguments,
                )
            })?;
        if let Some(CollectArgumentResult {
            mut join_node,
            sub_tree,
            remote_alias,
            replacement_tokens,
        }) = collect_arg_res
        {
            // patch the target/RHS IR with variable values
            let (join_variables_, argument_ids): (Vec<_>, Vec<_>) = arguments.into_iter().unzip();
            let join_variables: Vec<BTreeMap<String, json::Value>> = join_variables_
                .into_iter()
                .map(|bmap| bmap.into_iter().map(|(k, v)| (k, v.0)).collect())
                .collect();
            join_node.target_ndc_ir.variables = Some(join_variables);

            // execute the remote query
            let mut target_response = tracer
                .in_span_async(
                    "execute_remote_join_query",
                    SpanVisibility::Internal,
                    || {
                        Box::pin(execute_ndc_query(
                            http_client,
                            join_node.target_ndc_ir,
                            join_node.target_data_connector,
                            execution_span_attribute.clone(),
                            remote_alias.clone(),
                            project_id.clone(),
                        ))
                    },
                )
                .await?;

            // if there is a `location.rest`, recursively process the tree; which
            // will modify the `target_response` with all joins down the tree
            if !location.rest.locations.is_empty() {
                execute_join_locations(
                    http_client,
                    execution_span_attribute.clone(),
                    // TODO: is this field span correct?
                    field_span_attribute.clone(),
                    &mut target_response,
                    &join_node.process_response_as,
                    sub_tree,
                    project_id.clone(),
                )
                .await?;
            }

            tracer.in_span("response_join", SpanVisibility::Internal, || {
                // from `Vec<RowSet>` create `HashMap<ArgumentId, RowSet>`
                let responses: HashMap<ArgumentId, ndc::models::RowSet> = argument_ids
                    .iter()
                    .zip(target_response)
                    .map(|(arg_id, row_set)| (*arg_id, row_set))
                    .collect();
                // use the replacement tokens to lookup the argument id `responses`
                // and substitute that value in `lhs_response`
                replace_replacement_tokens(
                    &key,
                    &remote_alias,
                    &location,
                    lhs_response,
                    replacement_tokens,
                    responses,
                )
            })?;
        }
    }
    Ok(())
}

pub struct CollectArgumentResult<'s, 'ir> {
    join_node: RemoteJoin<'s, 'ir>,
    sub_tree: JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    remote_alias: String,
    replacement_tokens: ReplacementTokenRows,
}

/// Given a LHS response and `Location`, extract the join values from the
/// response and return it as the `Arguments` data structure. This also returns
/// a structure of `ReplacementToken`s.
pub fn collect_arguments<'s, 'ir>(
    lhs_response: &Vec<ndc::models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    key: &str,
    location: &Location<(RemoteJoin<'s, 'ir>, JoinId)>,
    arguments: &mut Arguments,
) -> Result<Option<CollectArgumentResult<'s, 'ir>>, error::Error> {
    if lhs_response.is_empty() {
        return Ok(None);
    }
    let mut argument_id_counter = MonotonicCounter::new();
    let mut remote_join = None;
    let mut sub_tree = JoinLocations::new();
    let mut replacement_tokens = vec![];
    let mut remote_alias = String::new();

    for row_set in lhs_response {
        if let Some(ref rows) = row_set.rows {
            let mut row_set_replacement_tokens: Vec<ReplacementToken> = vec![];
            for row in rows.iter() {
                let mut replacement_token = (0, 0);

                match lhs_response_type {
                    ProcessResponseAs::Array { .. } | ProcessResponseAs::Object { .. } => {
                        collect_argument_from_row(
                            row,
                            key,
                            location,
                            arguments,
                            &mut argument_id_counter,
                            &mut remote_join,
                            &mut sub_tree,
                            &mut replacement_token,
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
                                arguments,
                                &mut argument_id_counter,
                                &mut remote_join,
                                &mut sub_tree,
                                &mut replacement_token,
                                &mut remote_alias,
                            )?;
                            row_set_replacement_tokens.push(replacement_token);
                        }
                    }
                }
                row_set_replacement_tokens.push(replacement_token);
            }
            replacement_tokens.push(Some(row_set_replacement_tokens));
        } else {
            replacement_tokens.push(None);
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
            join_node: remote_join,
            sub_tree,
            remote_alias,
            replacement_tokens,
        })),
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_argument_from_row<'s, 'ir>(
    row: &IndexMap<String, ndc::models::RowFieldValue>,
    key: &str,
    location: &Location<(RemoteJoin<'s, 'ir>, JoinId)>,
    arguments: &mut Arguments,
    argument_id_counter: &mut MonotonicCounter,
    remote_join: &mut Option<RemoteJoin<'s, 'ir>>,
    sub_tree: &mut JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    replacement_token: &mut ReplacementToken,
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
        JoinNode::Remote((join_node, join_id)) => {
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
            // de-duplicate arguments
            let argument_id: i16;
            if let std::collections::hash_map::Entry::Vacant(e) = arguments.entry(argument) {
                argument_id = argument_id_counter.get_next();
                e.insert(argument_id);
            } else {
                argument_id = argument_id_counter.get();
            }
            // inject a replacement token into the row of LHS response
            let token = (*join_id, argument_id);

            *remote_join = Some(join_node.clone());
            *sub_tree = location.rest.clone();
            *replacement_token = token;
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

            let rows = rows_from_row_field_value(location_kind, nested_val)?;
            if let Some(mut rows) = rows {
                for row in rows.iter_mut() {
                    for (sub_key, sub_location) in &location.rest.locations {
                        collect_argument_from_row(
                            row,
                            sub_key,
                            sub_location,
                            arguments,
                            argument_id_counter,
                            remote_join,
                            sub_tree,
                            replacement_token,
                            remote_alias,
                        )?;
                    }
                }
            }
            Ok(())
        }
    }
}

fn rows_from_row_field_value(
    location_kind: &LocationKind,
    nested_val: &ndc::models::RowFieldValue,
) -> Result<Option<Vec<IndexMap<String, ndc::models::RowFieldValue>>>, error::Error> {
    let rows: Option<Vec<IndexMap<String, ndc::models::RowFieldValue>>> = match location_kind {
        LocationKind::NestedData => Some(
            {
                let this = nested_val.clone();
                match this.0 {
                    serde_json::Value::Array(_) => serde_json::from_value(this.0).ok(),
                    serde_json::Value::Object(_) => {
                        serde_json::from_value(this.0).ok().map(|v| vec![v])
                    }
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

fn get_value<'n>(
    pick_alias: &String,
    row: &'n IndexMap<String, ndc::models::RowFieldValue>,
) -> &'n json::Value {
    match row.get(pick_alias) {
        Some(v) => &v.0,
        None => &json::Value::Null,
    }
}

/// resolve/process the command response for remote join execution
fn resolve_command_response_row(
    row: &IndexMap<String, ndc::models::RowFieldValue>,
    type_container: &TypeContainer<TypeName>,
) -> Result<Vec<IndexMap<String, ndc::models::RowFieldValue>>, error::Error> {
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
                let index_map: IndexMap<String, ndc::models::RowFieldValue> =
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
            let array_values: Vec<IndexMap<String, ndc::models::RowFieldValue>> =
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

/// Given a LHS response, RHS response and replacement tokens, replace values in
/// the LHS response from values in RHS response.
///
/// Uses the replacement tokens to lookup the argument id RHS response and
/// substitute that value in LHS response.
pub fn replace_replacement_tokens(
    alias: &str,
    remote_alias: &str,
    location: &Location<(RemoteJoin<'_, '_>, JoinId)>,
    lhs_response: &mut [ndc::models::RowSet],
    replacement_tokens: Vec<Option<Vec<ReplacementToken>>>,
    rhs_resp: HashMap<ArgumentId, ndc::models::RowSet>,
) -> Result<(), error::Error> {
    for (row_set, row_set_tokens) in lhs_response.iter_mut().zip(replacement_tokens) {
        if let Some((rows, tokens)) = row_set.rows.as_mut().zip(row_set_tokens) {
            for (row, token) in rows.iter_mut().zip(tokens.clone()) {
                // TODO: have a better interface of traversing through the
                // response tree, especially for commands
                let command_result_value = row.get_mut(FUNCTION_IR_VALUE_COLUMN_NAME);
                match command_result_value {
                    // it's a command response; traversing the response tree is
                    // different
                    Some(x) => match &mut x.0 {
                        json::Value::Array(ref mut arr) => {
                            for (command_row, command_token) in arr.iter_mut().zip(tokens.clone()) {
                                let new_val = command_row.clone();
                                let mut command_row_parsed: IndexMap<
                                    String,
                                    ndc::models::RowFieldValue,
                                > = json::from_value(new_val)?;
                                let rhs_val = json::to_value(rhs_resp.get(&command_token.1))?;
                                traverse_path_and_insert_value(
                                    location,
                                    &mut command_row_parsed,
                                    remote_alias.to_owned(),
                                    alias.to_owned(),
                                    ndc::models::RowFieldValue(rhs_val),
                                )?;
                                *command_row = json::to_value(command_row_parsed)?;
                            }
                        }
                        json::Value::Object(obj) => {
                            let rhs_val = json::to_value(rhs_resp.get(&token.1))?;
                            let mut command_row = obj
                                .into_iter()
                                .map(|(k, v)| (k.clone(), ndc::models::RowFieldValue(v.clone())))
                                .collect();
                            traverse_path_and_insert_value(
                                location,
                                &mut command_row,
                                remote_alias.to_owned(),
                                alias.to_owned(),
                                ndc::models::RowFieldValue(rhs_val),
                            )?;
                            *x = ndc::models::RowFieldValue(json::to_value(command_row)?);
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
                        let rhs_val = json::to_value(rhs_resp.get(&token.1))?;
                        traverse_path_and_insert_value(
                            location,
                            row,
                            remote_alias.to_owned(),
                            alias.to_owned(),
                            ndc::models::RowFieldValue(rhs_val),
                        )?;
                    }
                };
            }
        }
    }
    Ok(())
}

fn traverse_path_and_insert_value(
    location: &Location<(RemoteJoin<'_, '_>, JoinId)>,
    row: &mut IndexMap<String, ndc::models::RowFieldValue>,
    remote_alias: String,
    key: String,
    value: ndc::models::RowFieldValue,
) -> Result<(), error::Error> {
    match &location.join_node {
        JoinNode::Remote(_) => {
            row.insert(remote_alias, value);
            Ok(())
        }
        JoinNode::Local(location_kind) => {
            let row_field_val =
                row.get_mut(&key)
                    .ok_or(error::InternalEngineError::InternalGeneric {
                        description: "unexpected: could not find {key} in row".into(),
                    })?;
            match location_kind {
                LocationKind::LocalRelationship => {
                    let mut row_set: ndc::models::RowSet =
                        json::from_value(row_field_val.0.clone())?;
                    let mut rows =
                        row_set
                            .rows
                            .ok_or(error::InternalEngineError::InternalGeneric {
                                description: "expected row; encountered null".into(),
                            })?;
                    for inner_row in rows.iter_mut() {
                        for (sub_key, sub_location) in &location.rest.locations {
                            traverse_path_and_insert_value(
                                sub_location,
                                inner_row,
                                remote_alias.to_string(),
                                sub_key.to_string(),
                                value.clone(),
                            )?
                        }
                    }
                    row_set.rows = Some(rows);
                    *row_field_val = ndc::models::RowFieldValue(json::to_value(row_set)?);
                }
                LocationKind::NestedData => {
                    match row_field_val.0 {
                        serde_json::Value::Array(_) => {
                            if let Ok(mut rows) =
                                serde_json::from_value::<
                                    Vec<IndexMap<String, ndc::models::RowFieldValue>>,
                                >(row_field_val.0.clone())
                            {
                                for inner_row in rows.iter_mut() {
                                    for (sub_key, sub_location) in &location.rest.locations {
                                        traverse_path_and_insert_value(
                                            sub_location,
                                            inner_row,
                                            remote_alias.to_string(),
                                            sub_key.to_string(),
                                            value.clone(),
                                        )?
                                    }
                                }
                                *row_field_val = ndc::models::RowFieldValue(json::to_value(rows)?);
                            }
                        }
                        serde_json::Value::Object(_) => {
                            if let Ok(mut inner_row) = serde_json::from_value::<
                                IndexMap<String, ndc::models::RowFieldValue>,
                            >(
                                row_field_val.0.clone()
                            ) {
                                for (sub_key, sub_location) in &location.rest.locations {
                                    traverse_path_and_insert_value(
                                        sub_location,
                                        &mut inner_row,
                                        remote_alias.to_string(),
                                        sub_key.to_string(),
                                        value.clone(),
                                    )?
                                }
                                *row_field_val =
                                    ndc::models::RowFieldValue(json::to_value(inner_row)?);
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
