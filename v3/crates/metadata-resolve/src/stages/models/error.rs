use crate::helpers::ndc_validation::NDCValidationError;
use crate::helpers::{argument::ArgumentMappingError, type_mappings::TypeMappingCollectionError};
use crate::stages::{
    aggregates, apollo, data_connectors, graphql_config, object_types, order_by_expressions, relay,
};
use crate::types::subgraph::{Qualified, QualifiedTypeName};

use open_dds::{
    aggregates::AggregateExpressionName,
    arguments::ArgumentName,
    data_connector::{CollectionName, DataConnectorName, DataConnectorScalarType},
    models::ModelName,
    order_by_expression::OrderByExpressionName,
    types::{CustomTypeName, FieldName},
};

#[derive(Debug, thiserror::Error)]
pub enum ModelsError {
    #[error("the data type {data_type:} for model {model_name:} has not been defined")]
    UnknownModelDataType {
        model_name: Qualified<ModelName>,
        data_type: Qualified<CustomTypeName>,
    },

    #[error("source for the following model is defined more than once: {model_name:}")]
    DuplicateModelSourceDefinition { model_name: Qualified<ModelName> },
    #[error(
        "the source data connector {data_connector:} for model {model_name:} has not been defined"
    )]
    UnknownModelDataConnector {
        model_name: Qualified<ModelName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error(
        "the following argument in model {model_name:} is defined more than once: {argument_name:}"
    )]
    DuplicateModelArgumentDefinition {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },

    #[error("the collection {collection:} in the data connector {data_connector:} for model {model_name:} has not been defined")]
    UnknownModelCollection {
        model_name: Qualified<ModelName>,
        data_connector: Qualified<DataConnectorName>,
        collection: CollectionName,
    },
    #[error("An error occurred while mapping arguments in the model {model_name:} to the collection {collection_name:} in the data connector {data_connector_name:}: {error:}")]
    ModelCollectionArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: CollectionName,
        error: ArgumentMappingError,
    },
    #[error("{error:} in model {model_name:}")]
    ModelTypeMappingCollectionError {
        model_name: Qualified<ModelName>,
        error: TypeMappingCollectionError,
    },
    #[error("type mapping required for type {type_name:} in model source {model_name:} backed by data connector {data_connector:}")]
    TypeMappingRequired {
        model_name: Qualified<ModelName>,
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("no field mapping was found for field {field_name:} of model {model_name:} used in {comparison_location}")]
    NoFieldMappingForComparedField {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("comparison for non-scalar field {field_name:} of model {model_name:} used in {comparison_location} is unsupported")]
    UncomparableNonScalarFieldType {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("no equality operator has been defined in the data connector for field {field_name:} of model {model_name:} used in {comparison_location}")]
    NoEqualOperatorForComparedField {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("multiple equality operators have been defined in the data connector for field {field_name:} of model {model_name:} used in {comparison_location}")]
    MultipleEqualOperatorsForComparedField {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("the following model is defined more than once: {name:}")]
    DuplicateModelDefinition { name: Qualified<ModelName> },
    #[error("Error in order by expression {order_by_expression_name}: {error}")]
    OrderByExpressionError {
        order_by_expression_name: Qualified<OrderByExpressionName>,
        error: order_by_expressions::OrderByExpressionError,
    },
    #[error("Error in orderable fields of model {model_name}: {error}")]
    ModelV1OrderableFieldsError {
        model_name: Qualified<ModelName>,
        error: order_by_expressions::OrderByExpressionError,
    },
    #[error(
        "Unknown order by expression {order_by_expression_identifier} in in model {model_name}"
    )]
    UnknownOrderByExpressionIdentifier {
        model_name: Qualified<ModelName>,
        order_by_expression_identifier:
            Qualified<order_by_expressions::OrderByExpressionIdentifier>,
    },
    #[error("Type of order by expression {order_by_expression_name} does not match object type of model {model_name}.  Model type: {model_type}; order by expression type: {order_by_expression_type}")]
    OrderByExpressionTypeMismatch {
        model_name: Qualified<ModelName>,
        model_type: Qualified<CustomTypeName>,
        order_by_expression_name: Qualified<OrderByExpressionName>,
        order_by_expression_type: Qualified<CustomTypeName>,
    },

    #[error("NDC validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
    #[error("{0}")]
    AggregateExpressionError(#[from] aggregates::AggregateExpressionError),

    #[error("{0}")]
    ApolloError(#[from] apollo::ApolloError),

    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),
    #[error("{0}")]
    RelayError(#[from] relay::RelayError),
    #[error("{0}")]
    ModelAggregateExpressionError(#[from] ModelAggregateExpressionError),
    #[error("{0}")]
    DataConnectorError(#[from] data_connectors::NamedDataConnectorError),
}

#[derive(Debug, thiserror::Error)]
pub enum ModelAggregateExpressionError {
    #[error("a source must be defined for model {model:} in order to use aggregate expressions")]
    CannotUseAggregateExpressionsWithoutSource { model: Qualified<ModelName> },
    #[error("the aggregate expression {aggregate_expression} used with model {model_name} has not been defined")]
    UnknownModelAggregateExpression {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} but its operand type {aggregate_operand_type} does not match the model's type {model_type}")]
    ModelAggregateExpressionOperandTypeMismatch {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        model_type: QualifiedTypeName,
        aggregate_operand_type: QualifiedTypeName,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} which has the countDistinct aggregation enabled, but countDistinct is not valid when aggregating a model as every object is already logically distinct")]
    ModelAggregateExpressionCountDistinctNotAllowed {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} but the NDC type of the field {field_name} for data connector {data_connector_name} was not a optionally nullable named type")]
    ModelAggregateExpressionUnexpectedDataConnectorType {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
        field_name: FieldName,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} but for the data connector {data_connector_name} and scalar type {data_connector_operand_type}, mappings are not provided for all aggregation functions in the aggregate expression")]
    ModelAggregateExpressionDataConnectorMappingMissing {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
        data_connector_operand_type: DataConnectorScalarType,
    },
    #[error("error in aggregate expression {aggregate_expression} used with the model {model_name}: {object_type_error}")]
    ModelAggregateObjectTypeError {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        object_type_error: object_types::ObjectTypesError,
    },
    #[error("{0}")]
    AggregateExpressionError(#[from] aggregates::AggregateExpressionError),
}
