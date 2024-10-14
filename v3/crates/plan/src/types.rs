#[derive(Debug, derive_more::Display)]
pub enum PlanError {
    Internal(String),   // equivalent to DataFusionError::Internal
    Permission(String), // equivalent to DataFusionError::Plan
    External(Box<dyn std::error::Error + Send + Sync>), //equivalent to DataFusionError::External
}
