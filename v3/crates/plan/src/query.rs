mod boolean_expression;
mod command;
pub mod field_selection;
mod model;
pub mod model_target;
mod types;
use crate::types::PlanError;
pub use command::{
    execute_plan_from_function, execute_plan_from_procedure, from_command, CommandPlan, FromCommand,
};
use indexmap::IndexMap;
pub use model::{
    from_model_aggregate_selection, from_model_selection, ndc_query_to_query_execution_plan,
    ModelAggregateSelection,
};
use std::sync::Arc;
pub use types::{NDCFunction, NDCProcedure, NDCQuery, QueryContext};

use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use open_dds::query::{Alias, Query, QueryRequest};
use plan_types::{
    ExecutionTree, FieldsSelection, JoinLocations, PredicateQueryTrees, UniqueNumber,
};

// these types should probably live in `plan-types`
pub enum SingleNodeExecutionPlan {
    Query(plan_types::ExecutionTree),
    Mutation(plan_types::MutationExecutionPlan),
}

pub struct QueryExecution {
    pub execution_tree: ExecutionTree,
    pub query_context: QueryContext,
}

pub enum ExecutionPlan {
    Queries(IndexMap<Alias, QueryExecution>),
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
        let (single_node, query_context) = query_to_plan(
            query,
            metadata,
            session,
            request_headers,
            &mut unique_number,
        )?;

        match single_node {
            SingleNodeExecutionPlan::Query(execution_tree) => {
                queries.insert(
                    alias.clone(),
                    QueryExecution {
                        execution_tree,
                        query_context,
                    },
                );
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
fn query_to_plan<'req, 'metadata>(
    query: &'req Query,
    metadata: &'metadata Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<(SingleNodeExecutionPlan, QueryContext), PlanError>
where
    'metadata: 'req,
{
    match query {
        open_dds::query::Query::Model(model_selection) => {
            let (type_name, ndc_query, fields) = model::from_model_selection(
                model_selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            let query_execution_plan =
                model::ndc_query_to_query_execution_plan(&ndc_query, &fields, &IndexMap::new());
            let query_context = QueryContext { type_name };
            let execution_tree = ExecutionTree {
                query_execution_plan,
                remote_predicates: PredicateQueryTrees::new(),
                remote_join_executions: JoinLocations::new(),
            };

            Ok((
                SingleNodeExecutionPlan::Query(execution_tree),
                query_context,
            ))
        }
        open_dds::query::Query::ModelAggregate(model_aggregate) => {
            // we have to use `String` rather than `Alias` in the planning code so not to restrict ourselves to aliases
            // that are valid GraphQL types. Probably want to change `Alias` in OpenDD to something
            // less frontend specific, then we can avoid this cloning.
            let selection = model_aggregate
                .selection
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect();

            let ModelAggregateSelection {
                object_type_name: type_name,
                query: ndc_query,
                fields: aggregate_fields,
            } = model::from_model_aggregate_selection(
                &model_aggregate.target,
                &selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            let query_execution_plan = model::ndc_query_to_query_execution_plan(
                &ndc_query,
                &FieldsSelection {
                    fields: IndexMap::new(),
                },
                &aggregate_fields,
            );
            let query_context = QueryContext { type_name };
            let execution_tree = ExecutionTree {
                query_execution_plan,
                remote_predicates: PredicateQueryTrees::new(),
                remote_join_executions: JoinLocations::new(),
            };
            Ok((
                SingleNodeExecutionPlan::Query(execution_tree),
                query_context,
            ))
        }

        open_dds::query::Query::Command(command_selection) => {
            let command::FromCommand {
                command_plan,
                output_object_type_name,
                extract_response_from: _,
            } = command::from_command(
                command_selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;
            match command_plan {
                command::CommandPlan::Function(ndc_function) => {
                    let query_execution_plan = command::execute_plan_from_function(&ndc_function);

                    let query_context = QueryContext {
                        type_name: output_object_type_name,
                    };
                    let execution_tree = ExecutionTree {
                        query_execution_plan,
                        remote_predicates: PredicateQueryTrees::new(),
                        remote_join_executions: JoinLocations::new(),
                    };

                    Ok((
                        SingleNodeExecutionPlan::Query(execution_tree),
                        query_context,
                    ))
                }
                command::CommandPlan::Procedure(ndc_procedure) => {
                    let query_execution_plan = command::execute_plan_from_procedure(&ndc_procedure);

                    let query_context = QueryContext {
                        type_name: output_object_type_name,
                    };
                    Ok((
                        SingleNodeExecutionPlan::Mutation(query_execution_plan),
                        query_context,
                    ))
                }
            }
        }
    }
}
