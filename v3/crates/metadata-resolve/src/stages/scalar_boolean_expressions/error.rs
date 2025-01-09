use crate::Qualified;
use crate::{stages::graphql_config, types::error::ShouldBeAnError};
use open_dds::flags;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorScalarType},
    types::{CustomTypeName, OperatorName},
};

#[derive(Debug, thiserror::Error)]
pub enum ScalarBooleanExpressionTypeError {
    #[error("unknown data connector {data_connector:} referenced in scalar type representation of {scalar_type:}")]
    ScalarTypeFromUnknownDataConnector {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("cannot find scalar type {scalar_type:} in data connector {data_connector:}")]
    UnknownScalarTypeInDataConnector {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("cannot find type {custom_type:} when resolving arguments for comparison operator {operator_name:} for {boolean_expression_type:}")]
    UnknownCustomTypeInComparisonOperatorArgument {
        custom_type: Qualified<CustomTypeName>,
        operator_name: OperatorName,
        boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("data connector {data_connector:} could not be found")]
    DataConnectorNotFound {
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("scalar representations for data connector {data_connector:} could not be found")]
    DataConnectorScalarRepresentationsNotFound {
        data_connector: Qualified<DataConnectorName>,
    },
    #[error(
        "scalar type representation required for type {scalar_type:} in data connector {data_connector:}"
    )]
    DataConnectorScalarRepresentationRequired {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("Predicate types in data connectors are unsupported")]
    PredicateTypesUnsupported,
    #[error("{0}")]
    GraphqlError(#[from] graphql_config::GraphqlConfigError),
}

#[derive(Debug, thiserror::Error)]
pub enum ScalarBooleanExpressionTypeIssue {
    // Because the GraphqlConfig's filterInputConfig requires all properties to be set, the effective meaning of not
    // having the is_null operator set is that the entire boolean expression cannot be rendered in GraphQL and so we
    // communicate that effect here, even though this issue is only about logical operators specifically.
    #[error("a graphql section is defined in boolean expression type '{type_name}' but it will not appear in the GraphQL API unless logical operator field names are also configured in the GraphqlConfig in query.filterInputConfig")]
    MissingLogicalOperatorNamesInGraphqlConfig {
        type_name: Qualified<CustomTypeName>,
    },
    // Because the GraphqlConfig's filterInputConfig requires all properties to be set, the effective meaning of not
    // having the is_null operator set is that the entire boolean expression cannot be rendered in GraphQL and so we
    // communicate that effect here, even though this issue is only about the is null operator specifically.
    #[error("a graphql section is defined in boolean expression type '{type_name}' but it will not appear in the GraphQL API unless the is_null operator field name is also configured in the GraphqlConfig in query.filterInputConfig")]
    MissingIsNullOperatorNameInGraphqlConfig {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the boolean expression '{type_name}' has enabled logical operators, but they will not appear in the GraphQL API unless you update your CompatibilityConfig date to at least 2024-11-26")]
    LogicalOperatorsUnavailable {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the boolean expression '{type_name}' has a GraphQL field name conflict between the '{name}' {name_source_1} and the '{name}' {name_source_2}. One of these will need to be renamed.")]
    GraphqlFieldNameConflict {
        type_name: Qualified<CustomTypeName>,
        name: String,
        name_source_1: FieldNameSource,
        name_source_2: FieldNameSource,
    },
    #[error("the comparable operator '{name}' is defined more than once in the boolean expression type '{type_name}'")]
    DuplicateComparableOperatorFound {
        type_name: Qualified<CustomTypeName>,
        name: OperatorName,
    },
}

impl ShouldBeAnError for ScalarBooleanExpressionTypeIssue {
    fn should_be_an_error(&self, flags: &flags::OpenDdFlags) -> bool {
        match self {
            ScalarBooleanExpressionTypeIssue::MissingLogicalOperatorNamesInGraphqlConfig {
                ..
            }
            | ScalarBooleanExpressionTypeIssue::MissingIsNullOperatorNameInGraphqlConfig {
                ..
            }
            | ScalarBooleanExpressionTypeIssue::LogicalOperatorsUnavailable { .. } => false,
            ScalarBooleanExpressionTypeIssue::GraphqlFieldNameConflict { .. }
            | ScalarBooleanExpressionTypeIssue::DuplicateComparableOperatorFound { .. } => {
                flags.contains(flags::Flag::DisallowDuplicateNamesInBooleanExpressions)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, derive_more::Display)]
#[allow(clippy::enum_variant_names)] // It's fine that they all end with "Operator" :/
pub enum FieldNameSource {
    #[display("comparable operator")]
    ComparisonOperator,
    #[display("logical operator")]
    LogicalOperator,
    #[display("is null operator")]
    IsNullOperator,
}
