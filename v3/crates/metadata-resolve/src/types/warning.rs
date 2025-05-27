use derive_more::derive::Display;
use open_dds::{flags, types::CustomTypeName};

use crate::{
    Qualified,
    stages::{
        aggregate_boolean_expressions, aggregates, arguments, boolean_expressions,
        command_permissions, commands, data_connectors, glossaries, model_permissions, models,
        models_graphql, object_relationships, object_types, order_by_expressions,
        scalar_boolean_expressions, scalar_types, type_permissions,
    },
};

use super::error::{ContextualError, ShouldBeAnError};

/// Warnings for the user raised during metadata generation
/// These are things that don't break the build, but may do so in future
#[derive(Debug, thiserror::Error)]
pub enum Warning {
    #[error("{0}")]
    ObjectTypesIssue(#[from] object_types::ObjectTypesIssue),
    #[error("{0}")]
    TypePermissionIssue(#[from] type_permissions::TypePermissionIssue),
    #[error("{0}")]
    DataConnectorIssue(#[from] data_connectors::NamedDataConnectorIssue),
    #[error("{0}")]
    ScalarBooleanExpressionIssue(
        #[from] scalar_boolean_expressions::ScalarBooleanExpressionTypeIssue,
    ),
    #[error("{0}")]
    OrderByExpressionIssue(#[from] order_by_expressions::OrderByExpressionIssue),
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
    ModelPermissionIssue(#[from] model_permissions::ModelPermissionIssue),
    #[error("{0}")]
    CommandIssue(#[from] commands::CommandsIssue),
    #[error("{0}")]
    CommandPermissionIssue(#[from] command_permissions::CommandPermissionIssue),
    #[error("{0}")]
    AggregateExpressionIssue(#[from] aggregates::AggregateExpressionIssue),
    #[error("{0}")]
    ScalarTypesIssue(#[from] scalar_types::ScalarTypesIssue),
    #[error("{0}")]
    ConflictingNameAcrossTypes(ConflictingNameAcrossTypes),
    #[error("{0}")]
    ObjectRelationshipsIssue(#[from] object_relationships::ObjectRelationshipsIssue),
    #[error("{0}")]
    ArgumentIssue(#[from] arguments::ArgumentIssue),
    #[error("{0}")]
    GlossaryIssue(#[from] glossaries::GlossaryIssue),
}

impl ShouldBeAnError for Warning {
    fn should_be_an_error(&self, flags: &flags::OpenDdFlags) -> bool {
        match self {
            Warning::DataConnectorIssue(issue) => issue.should_be_an_error(flags),
            Warning::BooleanExpressionIssue(issue) => issue.should_be_an_error(flags),
            Warning::ObjectTypesIssue(issue) => issue.should_be_an_error(flags),
            Warning::OrderByExpressionIssue(issue) => issue.should_be_an_error(flags),
            Warning::ScalarTypesIssue(issue) => issue.should_be_an_error(flags),
            Warning::ModelGraphqlIssue(issue) => issue.should_be_an_error(flags),
            Warning::CommandIssue(issue) => issue.should_be_an_error(flags),
            Warning::ScalarBooleanExpressionIssue(issue) => issue.should_be_an_error(flags),
            Warning::ModelIssue(issue) => issue.should_be_an_error(flags),
            Warning::ModelPermissionIssue(issue) => issue.should_be_an_error(flags),
            Warning::TypePermissionIssue(issue) => issue.should_be_an_error(flags),
            Warning::CommandPermissionIssue(issue) => issue.should_be_an_error(flags),
            Warning::ObjectRelationshipsIssue(issue) => issue.should_be_an_error(flags),
            Warning::GlossaryIssue(issue) => issue.should_be_an_error(flags),
            _ => false,
        }
    }
}

impl ContextualError for Warning {
    fn create_error_context(&self) -> Option<error_context::Context> {
        match self {
            Warning::ModelPermissionIssue(issue) => issue.create_error_context(),
            Warning::BooleanExpressionIssue(issue) => issue.create_error_context(),
            Warning::GlossaryIssue(issue) => issue.create_error_context(),
            _ => None,
        }
    }
}

/// Represents the source of a type definition
#[derive(Debug, Display)]
pub enum TypeSource {
    /// Indicates the type is a scalar type
    #[display("ScalarType")]
    Scalar,
    /// Indicates the type is an object type
    #[display("ObjectType")]
    Object,
    /// Indicates the type is a boolean expression type
    #[display("BooleanExpressionType")]
    BooleanExpression,
}

/// Represents a collection of conflicting type sources
#[derive(Debug, Display)]
#[display("{}", _0.into_iter().map(std::string::ToString::to_string).collect::<Vec<_>>().join(", "))]
pub struct ConflictingSources(pub nonempty::NonEmpty<TypeSource>);

/// Represents an error when a type name conflicts across different type sources
#[derive(Debug, thiserror::Error)]
#[error("Types with conflicting name {name} found in {conflicting_sources}")]
pub struct ConflictingNameAcrossTypes {
    /// The name of the conflicting type
    pub name: Qualified<CustomTypeName>,
    /// The sources where the conflicting type was found
    pub conflicting_sources: ConflictingSources,
}
