use crate::stages::object_boolean_expressions;

/// Warnings for the user raised during metadata generation
/// These are things that don't break the build, but may do so in future
#[derive(Debug, thiserror::Error)]
pub enum Warning {
    #[error("{0}")]
    ObjectBooleanExpressionWarning(
        #[from] object_boolean_expressions::ObjectBooleanExpressionWarning,
    ),
}
