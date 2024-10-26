use datafusion::{common::DFSchemaRef, error::Result, physical_plan::ExecutionPlan};
use std::sync::Arc;

use hasura_authn_core::Session;
use open_dds::query::CommandSelection;

pub mod function;
pub mod procedure;
use crate::execute::planner::common::from_plan_error;
pub(crate) use function::CommandOutput;
pub(crate) use function::NDCFunctionPushDown;
use plan::{from_command, CommandPlan, FromCommand};
pub(crate) use procedure::NDCProcedurePushDown;

pub fn build_execution_plan(
    request_headers: &reqwest::header::HeaderMap,
    metadata: &metadata_resolve::Metadata,
    http_context: &Arc<execute::HttpContext>,
    session: &Arc<Session>,
    command_selection: &CommandSelection,
    // schema of the output of the command selection
    schema: &DFSchemaRef,
    output: &CommandOutput,
) -> Result<Arc<dyn ExecutionPlan>> {
    let FromCommand {
        command_plan,
        extract_response_from,
        ..
    } = from_command(command_selection, metadata, session, request_headers)
        .map_err(from_plan_error)?;

    match command_plan {
        CommandPlan::Function(function) => {
            let ndc_pushdown = NDCFunctionPushDown::new(
                function,
                http_context.clone(),
                schema,
                output.clone(),
                extract_response_from,
            );
            Ok(Arc::new(ndc_pushdown))
        }
        CommandPlan::Procedure(procedure) => {
            let ndc_pushdown = NDCProcedurePushDown::new(
                procedure,
                command_selection.target.clone(),
                http_context.clone(),
                schema,
                output.clone(),
                extract_response_from,
            );
            Ok(Arc::new(ndc_pushdown))
        }
    }
}
