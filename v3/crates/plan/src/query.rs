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

use engine_types::HttpContext;
use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use open_dds::query::{Query, QueryRequest};
use plan_types::{FieldsSelection, UniqueNumber};

// temporary type, we assume only one node atm
pub enum SingleNodeExecutionPlan {
    Query(plan_types::QueryExecutionPlan),
    Mutation(plan_types::MutationExecutionPlan),
}

// make a query execution plan, assuming an OpenDD IR with a single model request
pub fn plan_query_request<'req, 'metadata>(
    query_request: &'req QueryRequest,
    metadata: &'metadata Metadata,
    session: &Arc<Session>,
    http_context: &Arc<HttpContext>,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<(SingleNodeExecutionPlan, QueryContext), PlanError>
where
    'metadata: 'req,
{
    let QueryRequest::V1(query_request_v1) = query_request;
    let mut unique_number = UniqueNumber::new();

    // to limit scope, let's assume there's one item and explode otherwise
    let (_alias, query) = query_request_v1.queries.first().unwrap();

    // return plan for a single query (again, wrong, but let's unblock ourselves for now)
    query_to_plan(
        query,
        metadata,
        session,
        http_context,
        request_headers,
        &mut unique_number,
    )
}

// turn a single OpenDD IR Query into a query execution plan
fn query_to_plan<'req, 'metadata>(
    query: &'req Query,
    metadata: &'metadata Metadata,
    session: &Arc<Session>,
    http_context: &Arc<HttpContext>,
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
                http_context,
                request_headers,
                unique_number,
            )?;

            let query_execution_plan =
                model::ndc_query_to_query_execution_plan(&ndc_query, &fields, &IndexMap::new());
            let query_context = QueryContext { type_name };
            Ok((
                SingleNodeExecutionPlan::Query(query_execution_plan),
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
            Ok((
                SingleNodeExecutionPlan::Query(query_execution_plan),
                query_context,
            ))
        }

        open_dds::query::Query::Command(command_selection) => {
            let command::FromCommand {
                command_plan,
                output_object_type_name,
                extract_response_from: _,
            } = command::from_command(command_selection, metadata, session, request_headers)?;
            match command_plan {
                command::CommandPlan::Function(ndc_function) => {
                    let query_execution_plan = command::execute_plan_from_function(&ndc_function);

                    let query_context = QueryContext {
                        type_name: output_object_type_name,
                    };
                    Ok((
                        SingleNodeExecutionPlan::Query(query_execution_plan),
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
