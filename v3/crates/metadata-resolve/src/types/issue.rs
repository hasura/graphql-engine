use open_dds::flags;

use crate::stages::{
    aggregates, boolean_expressions, commands, data_connectors, models, object_boolean_expressions,
};

use super::error::ShouldBeAnError;

/// Warnings for the user raised during metadata generation
/// These are things that don't break the build, but may do so in future
#[derive(Debug, thiserror::Error)]
pub enum Issue {
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
    #[error("{0}")]
    AggregateExpressionIssue(#[from] aggregates::AggregateExpressionIssue),
}

impl ShouldBeAnError for Issue {
    fn should_be_an_error(&self, flags: &flags::Flags) -> bool {
        match self {
            Issue::DataConnectorIssue(issue) => issue.should_be_an_error(flags),
            _ => false,
        }
    }
}
