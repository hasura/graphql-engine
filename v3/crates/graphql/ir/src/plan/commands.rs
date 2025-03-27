use super::error;
use crate::{FunctionBasedCommand, ProcedureBasedCommand};
use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use plan_types::{MutationExecutionTree, QueryExecutionTree, UniqueNumber};

pub(crate) fn plan_query_execution(
    ir: &FunctionBasedCommand<'_>,
    metadata: &'_ Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<QueryExecutionTree, error::Error> {
    // TODO: expose more specific function in `plan` for command selections
    let single_node_execution_plan = plan::query_to_plan(
        &open_dds::query::Query::Command(ir.command_info.selection.clone()),
        metadata,
        session,
        request_headers,
        unique_number,
    )?;
    match single_node_execution_plan {
        plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
        plan::SingleNodeExecutionPlan::Mutation(_) => {
            // this may be unavoidable as we don't know ahead of time which kind of function we're
            // invoking yet
            Err(error::Error::PlanExpectedQueryGotMutation)
        }
    }
}

pub(crate) fn plan_mutation_execution(
    ir: &ProcedureBasedCommand<'_>,
    metadata: &'_ Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<MutationExecutionTree, error::Error> {
    let single_node_execution_plan = plan::query_to_plan(
        &open_dds::query::Query::Command(ir.command_info.selection.clone()),
        metadata,
        session,
        request_headers,
        unique_number,
    )?;
    match single_node_execution_plan {
        plan::SingleNodeExecutionPlan::Query(_) => {
            // this may be unavoidable as we don't know ahead of time which kind of function we're
            // invoking yet
            Err(error::Error::PlanExpectedMutationGotQuery)
        }
        plan::SingleNodeExecutionPlan::Mutation(execution_tree) => Ok(execution_tree),
    }
}
