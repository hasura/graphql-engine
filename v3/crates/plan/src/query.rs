mod arguments;
mod boolean_expression;
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
    process_argument_presets_for_command, process_argument_presets_for_model, UnresolvedArgument,
};
pub use command::{from_command, CommandPlan, FromCommand};
pub use filter::{
    build_relationship_comparison_expression, get_field_mapping_of_field_name, plan_expression,
};
use indexmap::IndexMap;
pub use model::{from_model_aggregate_selection, from_model_group_by, from_model_selection};
pub use permissions::process_model_predicate;
pub use relationships::{
    process_command_relationship_definition, process_model_relationship_definition,
};
use std::sync::Arc;

use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use open_dds::query::{Alias, Query, QueryRequest};
use plan_types::{ExecutionTree, UniqueNumber};

// these types should probably live in `plan-types`
pub enum SingleNodeExecutionPlan {
    Query(plan_types::ExecutionTree),
    Mutation(plan_types::MutationExecutionPlan),
}

pub enum ExecutionPlan {
    Queries(IndexMap<Alias, ExecutionTree>),
    Mutation(plan_types::MutationExecutionPlan), // currently only support a single mutation
}

// make a query execution plan from OpenDD IR
pub fn plan_query_request<'req, 'metadata>(
    query_request: &'req QueryRequest,
    metadata: &'metadata Metadata,
    session: &Arc<Session>,
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
            SingleNodeExecutionPlan::Mutation(mutation_execution_plan) => {
                if mutation.is_some() {
                    return Err(PlanError::Internal(
                        "Multiple mutations not currently supported in OpenDD pipeline".into(),
                    ));
                }
                mutation = Some(mutation_execution_plan);
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
    session: &Arc<Session>,
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
                request_headers,
                unique_number,
            )?;

            Ok(SingleNodeExecutionPlan::Query(execution_tree))
        }

        open_dds::query::Query::Command(command_selection) => {
            let command::FromCommand {
                command_plan,
                extract_response_from: _,
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
                command::CommandPlan::Procedure(mutation_execution_plan) => {
                    Ok(SingleNodeExecutionPlan::Mutation(mutation_execution_plan))
                }
            }
        }
    }
}
