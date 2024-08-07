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
