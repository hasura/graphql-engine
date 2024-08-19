use std::sync::Arc;

use datafusion::{
    common::{internal_err, tree_node::Transformed},
    error::Result,
    logical_expr::{Extension, LogicalPlan},
    optimizer::{optimizer::ApplyOrder, OptimizerConfig, OptimizerRule},
};

use crate::execute::planner::model::ModelQuery;

pub struct NDCPushDownLimit {}

impl OptimizerRule for NDCPushDownLimit {
    fn try_optimize(
        &self,
        _plan: &LogicalPlan,
        _config: &dyn OptimizerConfig,
    ) -> Result<Option<LogicalPlan>> {
        internal_err!("Should have called NDCPushDownLimit::rewrite")
    }

    fn name(&self) -> &str {
        "ndc_pushdown_limit"
    }
    fn apply_order(&self) -> Option<ApplyOrder> {
        Some(ApplyOrder::BottomUp)
    }

    fn supports_rewrite(&self) -> bool {
        true
    }

    fn rewrite(
        &self,
        plan: LogicalPlan,
        _config: &dyn OptimizerConfig,
    ) -> Result<Transformed<LogicalPlan>> {
        let Some((limit, offset, model_query)) = (match plan {
            LogicalPlan::Limit(ref limit) => match limit.input.as_ref() {
                LogicalPlan::Extension(Extension { node }) => node
                    .as_ref()
                    .as_any()
                    .downcast_ref::<ModelQuery>()
                    .map(move |model_query| (limit.fetch, limit.skip, model_query)),
                _ => None,
            },
            _ => None,
        }) else {
            return Ok(Transformed::no(plan));
        };

        let limited_query = model_query.paginate(limit, offset);
        let plan = LogicalPlan::Extension(Extension {
            node: Arc::new(limited_query),
        });
        Ok(Transformed::yes(plan))
    }
}
