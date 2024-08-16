pub(crate) mod filter;
pub(crate) mod model;

use hasura_authn_core::Session;
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
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) catalog: Arc<crate::catalog::Catalog>,
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
                session: self.session.clone(),
                http_context: self.http_context.clone(),
                catalog: self.catalog.clone(),
            })]);
        // Delegate most work of physical planning to the default physical planner
        physical_planner
            .create_physical_plan(logical_plan, session_state)
            .await
    }
}

pub(crate) struct NDCPushDownPlanner {
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) catalog: Arc<crate::catalog::Catalog>,
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
                .to_physical_node(&self.session, &self.http_context, &self.catalog.metadata)
                .await?;
            Ok(Some(Arc::new(ndc_pushdown)))
        } else {
            Ok(None)
        }
    }
}
