use open_dds::flags;

use crate::stages::{
    aggregate_boolean_expressions, aggregates, boolean_expressions, commands, data_connectors,
    models, models_graphql, object_boolean_expressions, scalar_boolean_expressions, scalar_types,
};

use super::error::ShouldBeAnError;

/// Warnings for the user raised during metadata generation
/// These are things that don't break the build, but may do so in future
#[derive(Debug, thiserror::Error)]
pub enum Warning {
    #[error("{0}")]
    ObjectBooleanExpressionIssue(#[from] object_boolean_expressions::ObjectBooleanExpressionIssue),
    #[error("{0}")]
    DataConnectorIssue(#[from] data_connectors::NamedDataConnectorIssue),
    #[error("{0}")]
    ScalarBooleanExpressionIssue(
        #[from] scalar_boolean_expressions::ScalarBooleanExpressionTypeIssue,
    ),
    #[error("{0}")]
    BooleanExpressionIssue(#[from] boolean_expressions::BooleanExpressionIssue),
    #[error("{0}")]
    AggregateBooleanExpressionIssue(
        #[from] aggregate_boolean_expressions::NamedAggregateBooleanExpressionIssue,
    ),
    #[error("{0}")]
    ModelIssue(#[from] models::ModelsIssue),
    #[error("{0}")]
    ModelGraphqlIssue(#[from] models_graphql::ModelGraphqlIssue),
    #[error("{0}")]
    CommandIssue(#[from] commands::CommandsIssue),
    #[error("{0}")]
    AggregateExpressionIssue(#[from] aggregates::AggregateExpressionIssue),
    #[error("{0}")]
    ScalarTypesIssue(#[from] scalar_types::ScalarTypesIssue),
}

impl ShouldBeAnError for Warning {
    fn should_be_an_error(&self, flags: &flags::Flags) -> bool {
        match self {
            Warning::DataConnectorIssue(issue) => issue.should_be_an_error(flags),
            Warning::BooleanExpressionIssue(issue) => issue.should_be_an_error(flags),
            Warning::ObjectBooleanExpressionIssue(issue) => issue.should_be_an_error(flags),
            Warning::ScalarTypesIssue(issue) => issue.should_be_an_error(flags),
            Warning::ModelGraphqlIssue(issue) => issue.should_be_an_error(flags),
            Warning::CommandIssue(issue) => issue.should_be_an_error(flags),
            Warning::ScalarBooleanExpressionIssue(issue) => issue.should_be_an_error(flags),
            _ => false,
        }
    }
}
