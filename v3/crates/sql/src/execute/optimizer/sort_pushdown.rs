use std::sync::Arc;

use datafusion::{
    common::{internal_err, tree_node::Transformed},
    error::Result,
    logical_expr::{Extension, LogicalPlan},
    optimizer::{optimizer::ApplyOrder, OptimizerConfig, OptimizerRule},
};

use crate::execute::planner::model::ModelQuery;

pub struct NDCPushDownSort {}

impl OptimizerRule for NDCPushDownSort {
    fn try_optimize(
        &self,
        _plan: &LogicalPlan,
        _config: &dyn OptimizerConfig,
    ) -> Result<Option<LogicalPlan>> {
        internal_err!("Should have called NDCPushDownSort::rewrite")
    }

    fn name(&self) -> &str {
        "ndc_pushdown_sort"
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
        let Some((sort_by, fetch, model_query)) = (match plan {
            LogicalPlan::Sort(ref sort) => match sort.input.as_ref() {
                LogicalPlan::Extension(Extension { node }) => node
                    .as_ref()
                    .as_any()
                    .downcast_ref::<ModelQuery>()
                    .map(move |model_query| (&sort.expr, &sort.fetch, model_query)),
                _ => None,
            },
            _ => None,
        }) else {
            return Ok(Transformed::no(plan));
        };

        if model_query.model_selection.target.limit.is_some() {
            return Ok(Transformed::no(plan));
        }

        if model_query.model_selection.target.offset.is_some() {
            return Ok(Transformed::no(plan));
        }

        if let Some(sorted_query) = model_query.sort(sort_by, *fetch)? {
            let plan = LogicalPlan::Extension(Extension {
                node: Arc::new(sorted_query),
            });
            Ok(Transformed::yes(plan))
        } else {
            Ok(Transformed::no(plan))
        }
    }
}
