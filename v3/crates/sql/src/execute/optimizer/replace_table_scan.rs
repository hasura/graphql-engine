use datafusion::{
    common::{internal_err, tree_node::Transformed},
    datasource::source_as_provider,
    error::Result,
    logical_expr::{LogicalPlan, TableScan},
    optimizer::{optimizer::ApplyOrder, OptimizerConfig, OptimizerRule},
};

use crate::catalog::model;

pub struct ReplaceTableScan {}

impl OptimizerRule for ReplaceTableScan {
    fn try_optimize(
        &self,
        _plan: &LogicalPlan,
        _config: &dyn OptimizerConfig,
    ) -> Result<Option<LogicalPlan>> {
        internal_err!("Should have called ReplaceTableScan::rewrite")
    }

    fn name(&self) -> &str {
        "replace_table_scan"
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
        match plan {
            LogicalPlan::TableScan(TableScan {
                table_name: _,
                ref source,
                projection: _,
                ref projected_schema,
                ref filters,
                fetch: _,
            }) => {
                if let Some(opendd_table) = source_as_provider(source)?
                    .as_ref()
                    .as_any()
                    .downcast_ref::<model::Table>()
                {
                    let plan = opendd_table.to_logical_plan(projected_schema.clone(), filters)?;
                    Ok(Transformed::yes(plan))
                } else {
                    Ok(Transformed::no(plan))
                }
            }
            _ => Ok(Transformed::no(plan)),
        }
    }
}
