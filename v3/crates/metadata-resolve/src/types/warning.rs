use crate::stages::{
    boolean_expressions, commands, data_connectors, models, object_boolean_expressions,
};

/// Warnings for the user raised during metadata generation
/// These are things that don't break the build, but may do so in future
#[derive(Debug, thiserror::Error)]
pub enum Warning {
    #[error("{0}")]
    ObjectBooleanExpressionWarning(
        #[from] object_boolean_expressions::ObjectBooleanExpressionWarning,
    ),
    #[error("{0}")]
    DataConnectorIssue(#[from] data_connectors::NamedDataConnectorIssue),
    #[error("{0}")]
    BooleanExpressionIssue(#[from] boolean_expressions::BooleanExpressionIssue),
    #[error("{0}")]
    ModelIssue(#[from] models::ModelsIssue),
    #[error("{0}")]
    CommandIssue(#[from] commands::CommandsIssue),
}
