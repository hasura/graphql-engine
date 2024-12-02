use datafusion::error::DataFusionError;
use plan::PlanError;

#[derive(Debug, Clone)]
pub(crate) struct PhysicalPlanOptions {
    pub(crate) disallow_mutations: bool,
}

impl PhysicalPlanOptions {
    pub(crate) fn new(disallow_mutations: bool) -> Self {
        Self { disallow_mutations }
    }
}

// turn a `plan::PlanError` into a `DataFusionError`.
pub fn from_plan_error(plan: PlanError) -> DataFusionError {
    match plan {
        PlanError::Internal(msg) => DataFusionError::Internal(msg),
        PlanError::Permission(msg) | PlanError::Relationship(msg) => DataFusionError::Plan(msg),
        PlanError::External(error) => DataFusionError::External(error),
    }
}
