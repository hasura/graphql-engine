use crate::stages::graphql_config;
use crate::Qualified;
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
    #[error("a graphql section is defined in boolean expression type '{type_name}' but it will not appear in the GraphQL API unless logical operator field names are also configured in the GraphqlConfig in query.filterInputConfig")]
    MissingLogicalOperatorNamesInGraphqlConfig {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the boolean expression '{type_name}' has enabled logical operators, but they will not appear in the GraphQL API unless you update your CompatibilityConfig date to at least 2024-11-26")]
    LogicalOperatorsUnavailable {
        type_name: Qualified<CustomTypeName>,
    },
}
