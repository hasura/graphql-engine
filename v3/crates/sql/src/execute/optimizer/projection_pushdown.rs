use std::sync::Arc;

use datafusion::{
    common::{internal_err, tree_node::Transformed},
    error::Result,
    logical_expr::{Expr, Extension, LogicalPlan},
    optimizer::{optimizer::ApplyOrder, OptimizerConfig, OptimizerRule},
};

pub(crate) struct OpenDdPushDownProjection {}

impl OptimizerRule for OpenDdPushDownProjection {
    fn try_optimize(
        &self,
        _plan: &LogicalPlan,
        _config: &dyn OptimizerConfig,
    ) -> Result<Option<LogicalPlan>> {
        internal_err!("Should have called OpenDdPushDownProjection::rewrite")
    }

    fn name(&self) -> &str {
        "open_dd_pushdown_projection"
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
        if let Some((projections, projected_schema, model_query)) = {
            match plan {
                LogicalPlan::Projection(ref projection) => match projection.input.as_ref() {
                    LogicalPlan::Extension(Extension { node }) => node
                        .as_ref()
                        .as_any()
                        .downcast_ref::<crate::plan::ModelQuery>()
                        .map(|model_query| {
                            (&projection.expr, &projection.schema, model_query.clone())
                        }),
                    _ => None,
                },
                _ => None,
            }
        } {
            let projected_columns = projections_to_columns(projections)?;
            let projected_query = model_query.project(projected_schema.clone(), &projected_columns);
            let plan = LogicalPlan::Extension(Extension {
                node: Arc::new(projected_query),
            });
            Ok(Transformed::yes(plan))
        } else {
            Ok(Transformed::no(plan))
        }
    }
}

fn projections_to_columns(projections: &[Expr]) -> Result<Vec<String>> {
    projections
        .iter()
        .map(|expr| match expr {
            Expr::Column(column) => Ok(column.name.clone()),
            _ => internal_err!("non-column found in projection of OpenDD query: {}", expr),
        })
        .collect()
}
