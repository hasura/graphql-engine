mod arguments;
mod command;
pub mod field_selection;
pub mod filter;
mod model;
pub mod model_target;
mod permissions;
mod relationships;
mod types;
use crate::types::PlanError;
pub use arguments::{
    ArgumentPresetExecutionError, MapFieldNamesError, UnresolvedArgument,
    process_argument_presets_for_command, process_argument_presets_for_model,
};
pub use command::{CommandPlan, FromCommand, from_command};
pub use filter::{build_relationship_comparison_expression, plan_expression};
use indexmap::IndexMap;
pub use model::{from_model_aggregate_selection, from_model_group_by, from_model_selection};
pub use permissions::process_model_predicate;
pub use relationships::{
    RelationshipFieldMappingError, collect_remote_join_object_type_field_mappings,
    get_relationship_field_mapping_of_field_name, process_command_relationship_definition,
    process_model_relationship_definition,
};

use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use open_dds::query::{Alias, Query, QueryRequest};
use plan_types::{QueryExecutionTree, UniqueNumber};

// these types should probably live in `plan-types`
#[derive(Debug)]
pub enum SingleNodeExecutionPlan {
    Query(plan_types::QueryExecutionTree),
    Mutation(plan_types::MutationExecutionTree),
}

#[derive(Debug)]
pub enum ExecutionPlan {
    Queries(IndexMap<Alias, QueryExecutionTree>),
    Mutation(plan_types::MutationExecutionTree), // currently only support a single mutation
}

// make a query execution plan from OpenDD IR
pub fn plan_query_request<'req, 'metadata>(
    query_request: &'req QueryRequest,
    metadata: &'metadata Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<ExecutionPlan, PlanError>
where
    'metadata: 'req,
{
    let QueryRequest::V1(query_request_v1) = query_request;
    let mut unique_number = UniqueNumber::new();

    let mut queries = IndexMap::new();
    let mut mutation = None;

    for (alias, query) in &query_request_v1.queries {
        let single_node = query_to_plan(
            query,
            metadata,
            session,
            request_headers,
            &mut unique_number,
        )?;

        match single_node {
            SingleNodeExecutionPlan::Query(execution_tree) => {
                queries.insert(alias.clone(), execution_tree);
            }
            SingleNodeExecutionPlan::Mutation(execution_tree) => {
                if mutation.is_some() {
                    return Err(PlanError::Internal(
                        "Multiple mutations not currently supported in OpenDD pipeline".into(),
                    ));
                }
                mutation = Some(execution_tree);
            }
        }
    }
    if let Some(mutation) = mutation {
        if queries.is_empty() {
            Ok(ExecutionPlan::Mutation(mutation))
        } else {
            Err(PlanError::Internal(
                "Mixture of queries and mutations is not supported in OpenDD pipeline".into(),
            ))
        }
    } else {
        Ok(ExecutionPlan::Queries(queries))
    }
}

// turn a single OpenDD IR Query into a query execution plan
pub fn query_to_plan<'req, 'metadata>(
    query: &'req Query,
    metadata: &'metadata Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<SingleNodeExecutionPlan, PlanError>
where
    'metadata: 'req,
{
    match query {
        open_dds::query::Query::Model(model_selection) => {
            let execution_tree = model::from_model_selection(
                model_selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            Ok(SingleNodeExecutionPlan::Query(execution_tree))
        }
        open_dds::query::Query::ModelAggregate(model_aggregate) => {
            let execution_tree = model::from_model_aggregate_selection(
                &model_aggregate.target,
                &model_aggregate.selection,
                metadata,
                session,
                None,
                request_headers,
                unique_number,
            )?;

            Ok(SingleNodeExecutionPlan::Query(execution_tree))
        }
        open_dds::query::Query::ModelGroups(model_groups) => {
            let execution_tree = model::from_model_group_by(
                &model_groups.target,
                &model_groups.selection,
                &model_groups.dimensions,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            Ok(SingleNodeExecutionPlan::Query(execution_tree))
        }
        open_dds::query::Query::Command(command_selection) => {
            let command::FromCommand {
                command_plan,
                extract_response_from: _, // TODO something for commands that return headers here?
            } = command::from_command(
                command_selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;
            match command_plan {
                command::CommandPlan::Function(execution_tree) => {
                    Ok(SingleNodeExecutionPlan::Query(execution_tree))
                }
                command::CommandPlan::Procedure(execution_tree) => {
                    Ok(SingleNodeExecutionPlan::Mutation(execution_tree))
                }
            }
        }
    }
}
