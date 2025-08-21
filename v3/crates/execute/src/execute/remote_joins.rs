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
//! ## Join Tree generation
//! - The join tree is generated as part of query plan.
//!
//! ## Execution
//! Following is the high-level algorithm how remote joins execution is performed.
//!
//! 1. Make the first top-level NDC query, and call the response as LHS response.
//!
//! 2. Traverse the join tree to get to the next remote join. Iterate through
//!    all the rows in the LHS response, use the field mapping in the remote join node
//!    to collect the values of those fields. In the above example, these would be
//!    collecting the city codes from the LHS city query.
//!
//! 3. Get the NDC query from the remote join node, and attach the values in the
//!    above step as variables in the NDC query. This NDC query already has a
//!    "where" filter clause with a variable on the join mapping field. Make the
//!    NDC query, and call the response as RHS response.
//!
//! 4. If there is a sub-tree from this remote join node, recursively perform
//!    this algorithm.
//!
//! 5. Perform join on LHS response and RHS response
use metadata_resolve::{Qualified, QualifiedTypeName, QualifiedTypeReference};
use open_dds::types::CustomTypeName;
use serde_json as json;
use std::collections::{BTreeMap, HashMap};
use tracing_util::SpanVisibility;

use plan_types::{ProcessResponseAs, RemoteJoinObjectFieldMapping};

use crate::error;
use crate::ndc::execute_ndc_query;
use engine_types::{HttpContext, ProjectId};
use hasura_authn_core::Session;
use metadata_resolve::LifecyclePluginConfigs;

use collect::ExecutableJoinNode;
use plan_types::{JoinLocations, RemoteJoinVariableSet};
mod collect;
mod join;

use async_recursion::async_recursion;

/// Execute remote joins. As an entry-point it assumes the response is available
/// for the top-level query, and executes further remote joins recursively.
#[async_recursion]
pub async fn execute_join_locations(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    execution_span_attribute: &'static str,
    lhs_response: &mut Vec<ndc_models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    join_locations: &JoinLocations,
    project_id: Option<&ProjectId>,
) -> Result<(), error::FieldError> {
    let tracer = tracing_util::global_tracer();

    // collect the join column arguments from the LHS response
    let location_path = &[];
    let next_join_nodes = tracer.in_span(
        "collect_arguments",
        "Collect arguments for join",
        SpanVisibility::Internal,
        || {
            collect::collect_next_join_nodes(
                lhs_response,
                lhs_response_type,
                join_locations,
                location_path,
            )
        },
    )?;

    for executable_join_node in next_join_nodes {
        let ExecutableJoinNode {
            variable_sets,
            location_path,
            mut join_node,
            sub_tree,
            remote_alias,
        } = executable_join_node;

        // if we do not get any join arguments back, we have nothing on the RHS
        // to execute. Skip execution.
        if variable_sets.is_empty() {
            continue;
        }

        // patch the target/RHS IR with variable values
        let foreach_variables: Vec<BTreeMap<plan_types::VariableName, json::Value>> = variable_sets
            .iter()
            .map(|variable_set| {
                variable_set
                    .iter()
                    .map(|(variable_name, variable)| {
                        let mapped_variable_value =
                            map_remote_join_variable_value_to_target_connector(
                                &variable.value,
                                &variable.variable_type,
                                &join_node.object_type_field_mappings,
                            )?;
                        Ok((variable_name.clone(), mapped_variable_value))
                    })
                    .collect::<Result<_, _>>()
            })
            .collect::<Result<_, error::FieldError>>()?;

        join_node.target_ndc_execution.variables = Some(foreach_variables);

        let ndc_query =
            super::ndc_request::make_ndc_query_request(join_node.target_ndc_execution.clone())?;

        // execute the remote query
        let mut target_response = tracer
            .in_span_async(
                "execute_remote_join_query",
                "Execute remote query for join",
                SpanVisibility::Internal,
                || {
                    Box::pin(execute_ndc_query(
                        http_context,
                        plugins,
                        session,
                        &ndc_query,
                        &join_node.target_data_connector,
                        execution_span_attribute,
                        remote_alias.clone(),
                        project_id,
                    ))
                },
            )
            .await?
            .as_latest_rowsets();

        // if the sub-tree is not empty, recursively process the sub-tree; which
        // will modify the `target_response` with all joins down the tree
        if !sub_tree.locations.is_empty() {
            execute_join_locations(
                http_context,
                plugins,
                session,
                execution_span_attribute,
                &mut target_response,
                &join_node.process_response_as,
                &sub_tree,
                project_id,
            )
            .await?;
        }
        tracer.in_span(
            "response_join",
            "Join responses for remote query",
            SpanVisibility::Internal,
            || {
                // from `Vec<RowSet>` create `HashMap<Argument, RowSet>`
                let rhs_response: HashMap<RemoteJoinVariableSet, ndc_models::RowSet> =
                    variable_sets.into_iter().zip(target_response).collect();

                join::join_responses(
                    &location_path,
                    &join_node,
                    &remote_alias,
                    lhs_response,
                    lhs_response_type,
                    &rhs_response,
                )
            },
        )?;
    }
    Ok(())
}

fn map_remote_join_variable_value_to_target_connector(
    value: &serde_json::Value,
    value_type: &QualifiedTypeReference,
    object_type_field_mappings: &BTreeMap<Qualified<CustomTypeName>, RemoteJoinObjectFieldMapping>,
) -> Result<serde_json::Value, error::FieldError> {
    // If the value is null, there's nothing to map
    if value.is_null() {
        return Ok(serde_json::Value::Null);
    }

    match &value_type.underlying_type {
        // Scalar inbuilt types: we can just pass through untouched
        metadata_resolve::QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(_)) => {
            Ok(value.clone())
        }
        metadata_resolve::QualifiedBaseType::Named(QualifiedTypeName::Custom(value_type_name)) => {
            // If the named type is an object type, we'll have a mapping for it
            if let Some(object_type_field_mapping) = object_type_field_mappings.get(value_type_name)
            {
                let mapped_object = value
                    .as_object()
                    .ok_or_else(|| error::NDCUnexpectedError::BadNDCResponse {
                        summary: format!("While mapping remote join variable values, expected an object value for type '{value_type}', but got a JSON {}", get_json_value_type(value)),
                    })?
                    .iter()
                    .map(|(field_name, field_value)| {
                        let target_field_info = object_type_field_mapping
                            .get(field_name.as_str())
                            .ok_or_else(|| error::NDCUnexpectedError::BadNDCResponse {
                                summary: format!("While mapping remote join variable values, found an unexpected field '{field_name}' in the object value with expected type '{value_type}'"),
                            })?;

                        let mapped_field_value =
                            map_remote_join_variable_value_to_target_connector(
                                field_value,
                                &target_field_info.field_type,
                                object_type_field_mappings,
                            )?;

                        Ok((
                            target_field_info.name.as_str().to_owned(),
                            mapped_field_value,
                        ))
                    })
                    .collect::<Result<serde_json::Map<_, _>, error::FieldError>>()?;

                Ok(serde_json::Value::Object(mapped_object))
            }
            // It must be a custom scalar type, so we can just pass through untouched
            else {
                Ok(value.clone())
            }
        }
        // Array types: we need to map each of the elements
        metadata_resolve::QualifiedBaseType::List(element_type) => {
            let mapped_array = value
                .as_array()
                .ok_or_else(|| error::NDCUnexpectedError::BadNDCResponse {
                    summary: format!("While mapping remote join variable values, expected an array value for type '{value_type}', but got a JSON {}", get_json_value_type(value)),
                })?
                .iter()
                .map(|element| {
                    map_remote_join_variable_value_to_target_connector(
                        element,
                        element_type,
                        object_type_field_mappings,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(serde_json::Value::Array(mapped_array))
        }
    }
}

fn get_json_value_type(value: &serde_json::Value) -> &'static str {
    match value {
        serde_json::Value::Null => "null",
        serde_json::Value::Bool(_) => "boolean",
        serde_json::Value::Number(_) => "number",
        serde_json::Value::String(_) => "string",
        serde_json::Value::Array(_) => "array",
        serde_json::Value::Object(_) => "object",
    }
}
