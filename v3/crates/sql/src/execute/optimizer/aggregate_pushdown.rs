use std::sync::Arc;

use datafusion::{
    common::{internal_err, tree_node::Transformed},
    error::Result,
    logical_expr::{Extension, LogicalPlan},
    optimizer::{optimizer::ApplyOrder, OptimizerConfig, OptimizerRule},
};

use crate::execute::planner::model::ModelQuery;

pub struct NDCPushDownAggregate {}

impl OptimizerRule for NDCPushDownAggregate {
    fn try_optimize(
        &self,
        _plan: &LogicalPlan,
        _config: &dyn OptimizerConfig,
    ) -> Result<Option<LogicalPlan>> {
        internal_err!("Should have called NDCPushDownAggregate::rewrite")
    }

    fn name(&self) -> &str {
        "ndc_pushdown_aggregate"
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
        let Some((measures, groupings, schema, model_query)) = (match plan {
            LogicalPlan::Aggregate(ref aggregate) => match aggregate.input.as_ref() {
                LogicalPlan::Extension(Extension { node }) => node
                    .as_ref()
                    .as_any()
                    .downcast_ref::<ModelQuery>()
                    .map(move |model_query| {
                        (
                            &aggregate.aggr_expr,
                            &aggregate.group_expr,
                            &aggregate.schema,
                            model_query,
                        )
                    }),
                _ => None,
            },
            _ => None,
        }) else {
            return Ok(Transformed::no(plan));
        };

        if !groupings.is_empty() {
            return Ok(Transformed::no(plan));
        }

        if let Some(aggregate_query) = model_query.aggregate(measures, schema.clone())? {
            let plan = LogicalPlan::Extension(Extension {
                node: Arc::new(aggregate_query),
            });
            Ok(Transformed::yes(plan))
        } else {
            Ok(Transformed::no(plan))
        }
    }
}
