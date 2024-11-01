pub(crate) mod command;
pub(crate) mod common;
pub(crate) mod model;
pub(crate) mod scalar;

use command::build_execution_plan;
use hasura_authn_core::Session;
use metadata_resolve as resolved;
use std::sync::Arc;

use datafusion::{
    error::Result,
    execution::context::{QueryPlanner, SessionState},
    logical_expr::{LogicalPlan, UserDefinedLogicalNode},
    physical_plan::ExecutionPlan,
    physical_planner::{DefaultPhysicalPlanner, ExtensionPlanner, PhysicalPlanner},
};

use async_trait::async_trait;

pub(crate) struct OpenDDQueryPlanner {
    pub(crate) request_headers: Arc<reqwest::header::HeaderMap>,
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) metadata: Arc<resolved::Metadata>,
}

#[async_trait]
impl QueryPlanner for OpenDDQueryPlanner {
    /// Given a `LogicalPlan` created from above, create an
    /// `ExecutionPlan` suitable for execution
    async fn create_physical_plan(
        &self,
        logical_plan: &LogicalPlan,
        session_state: &SessionState,
    ) -> Result<Arc<dyn ExecutionPlan>> {
        // Teach the default physical planner how to plan TopK nodes.
        let physical_planner =
            DefaultPhysicalPlanner::with_extension_planners(vec![Arc::new(NDCPushDownPlanner {
                request_headers: self.request_headers.clone(),
                session: self.session.clone(),
                http_context: self.http_context.clone(),
                metadata: self.metadata.clone(),
            })]);
        // Delegate most work of physical planning to the default physical planner
        physical_planner
            .create_physical_plan(logical_plan, session_state)
            .await
    }
}

pub(crate) struct NDCPushDownPlanner {
    pub(crate) request_headers: Arc<reqwest::header::HeaderMap>,
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) metadata: Arc<resolved::Metadata>,
}

impl NDCPushDownPlanner {}

#[async_trait]
impl ExtensionPlanner for NDCPushDownPlanner {
    /// Create a physical plan for an extension node
    async fn plan_extension(
        &self,
        _planner: &dyn PhysicalPlanner,
        node: &dyn UserDefinedLogicalNode,
        logical_inputs: &[&LogicalPlan],
        physical_inputs: &[Arc<dyn ExecutionPlan>],
        _session_state: &SessionState,
    ) -> Result<Option<Arc<dyn ExecutionPlan>>> {
        if let Some(model_query) = node.as_any().downcast_ref::<model::ModelQuery>() {
            assert_eq!(logical_inputs.len(), 0, "Inconsistent number of inputs");
            assert_eq!(physical_inputs.len(), 0, "Inconsistent number of inputs");
            let ndc_pushdown = model_query
                .to_physical_node(
                    &self.session,
                    &self.http_context,
                    &self.metadata,
                    &self.request_headers,
                )
                .await?;
            Ok(Some(Arc::new(ndc_pushdown)))
        } else if let Some(command_query) = node.as_any().downcast_ref::<command::CommandQuery>() {
            assert_eq!(logical_inputs.len(), 0, "Inconsistent number of inputs");
            assert_eq!(physical_inputs.len(), 0, "Inconsistent number of inputs");
            build_execution_plan(
                &self.request_headers,
                &self.metadata,
                &self.http_context,
                &self.session,
                &command_query.command_selection,
                &command_query.schema,
                &command_query.output,
            )
            .map(Some)
        } else if let Some(model_aggregate) = node.as_any().downcast_ref::<model::ModelAggregate>()
        {
            assert_eq!(logical_inputs.len(), 0, "Inconsistent number of inputs");
            assert_eq!(physical_inputs.len(), 0, "Inconsistent number of inputs");

            let ndc_pushdown = model_aggregate
                .to_physical_node(
                    &self.session,
                    &self.http_context,
                    &self.metadata,
                    &self.request_headers,
                )
                .await?;

            Ok(Some(Arc::new(ndc_pushdown)))
        } else {
            Ok(None)
        }
    }
}
