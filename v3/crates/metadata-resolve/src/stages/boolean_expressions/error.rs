use super::BooleanExpressionTypeIdentifier;
use crate::helpers::type_mappings::TypeMappingCollectionError;
use crate::stages::{
    boolean_expressions, data_connectors, graphql_config, relationships, scalar_boolean_expressions,
};
use crate::types::error::TypePredicateError;
use crate::types::subgraph::{Qualified, QualifiedTypeName};
use crate::QualifiedTypeReference;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorObjectType},
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};

#[derive(Debug, thiserror::Error)]
pub enum BooleanExpressionError {
    #[error("unknown type '{type_name}' used in object boolean expression '{boolean_expression_type_name}'")]
    UnknownTypeInObjectBooleanExpressionType {
        boolean_expression_type_name: Qualified<CustomTypeName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unsupported type '{type_name}' used in object boolean expression. '{boolean_expression_type_name}'; only object types are supported")]
    UnsupportedTypeInObjectBooleanExpressionType {
        boolean_expression_type_name: Qualified<CustomTypeName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unknown data connector {data_connector:} referenced in object boolean expression type {object_boolean_expression_type:}")]
    UnknownDataConnectorInObjectBooleanExpressionType {
        data_connector: Qualified<DataConnectorName>,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("unknown data connector object type {data_connector_object_type:} (in data connector {data_connector:}) referenced in object boolean expression type {object_boolean_expression_type:}")]
    UnknownDataConnectorTypeInObjectBooleanExpressionType {
        data_connector: Qualified<DataConnectorName>,
        data_connector_object_type: DataConnectorObjectType,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("unknown field '{field_name:}' used in object boolean expression type {object_boolean_expression_type:}")]
    UnknownFieldInObjectBooleanExpressionType {
        field_name: FieldName,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("the object type '{object_type:}' used in boolean expression type {object_boolean_expression_type:} does not have a mapping to object {data_connector_object_type:} of data connector {data_connector:}")]
    NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
        object_type: Qualified<CustomTypeName>,
        object_boolean_expression_type: Qualified<CustomTypeName>,
        data_connector_object_type: DataConnectorObjectType,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("{error:} in boolean expression type {object_boolean_expression_type:}")]
    BooleanExpressionTypeMappingCollectionError {
        object_boolean_expression_type: Qualified<CustomTypeName>,
        error: TypeMappingCollectionError,
    },
    #[error("the following object boolean expression type is defined more than once: {name:}")]
    DuplicateObjectBooleanExpressionTypeDefinition { name: Qualified<CustomTypeName> },
    #[error("unknown object boolean expression type {name:} is used in model {model:}")]
    UnknownBooleanExpressionTypeInModel {
        name: Qualified<CustomTypeName>,
        model: Qualified<ModelName>,
    },
    #[error("could not find object boolean expression type {child_boolean_expression:} referenced within boolean expression {parent_boolean_expression:}")]
    BooleanExpressionCouldNotBeFound {
        parent_boolean_expression: Qualified<CustomTypeName>,
        child_boolean_expression: Qualified<CustomTypeName>,
    },
    #[error("could not find scalar boolean expression type {child_boolean_expression:} referenced within boolean expression {parent_boolean_expression:}")]
    ScalarBooleanExpressionCouldNotBeFound {
        parent_boolean_expression: Qualified<CustomTypeName>,
        child_boolean_expression: boolean_expressions::BooleanExpressionTypeIdentifier,
    },
    #[error("the boolean expression type {name:} used in model {model:} corresponds to object type {boolean_expression_object_type:} whereas the model's object type is {model_object_type:}")]
    BooleanExpressionTypeForInvalidObjectTypeInModel {
        name: Qualified<CustomTypeName>,
        model: Qualified<ModelName>,
        boolean_expression_object_type: Qualified<CustomTypeName>,
        model_object_type: Qualified<CustomTypeName>,
    },
    #[error("field {field:} is missing a mapping for data connector {data_connector_name:} in boolean expression {boolean_expression_name:}")]
    DataConnectorMappingMissingForField {
        boolean_expression_name: Qualified<CustomTypeName>,
        field: FieldName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("The data connector '{data_connector_name}' does not support filtering inside nested objects. The comparable field '{field_name}' within '{boolean_expression_type_name}' is of a nested object type: {field_type}")]
    DataConnectorDoesNotSupportNestedObjectFiltering {
        data_connector_name: Qualified<DataConnectorName>,
        boolean_expression_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },
    #[error("The data connector '{data_connector_name}' does not support filtering across nested relationships. The nested object field '{field_name}' within '{parent_boolean_expression_type_name}' references the boolean expression type '{nested_boolean_expression_type_name}' which has relationship comparisons, making them nested relationship comparisons.")]
    DataConnectorDoesNotSupportNestedRelationshipFiltering {
        data_connector_name: Qualified<DataConnectorName>,
        field_name: FieldName,
        parent_boolean_expression_type_name: Qualified<CustomTypeName>,
        nested_boolean_expression_type_name: Qualified<CustomTypeName>,
    },
    #[error("The field {field_name:} has type {field_type:} but the field's boolean expression type {field_boolean_expression_type_name:} has type {underlying_type:}")]
    FieldTypeMismatch {
        field_name: FieldName,
        field_type: QualifiedTypeName,
        field_boolean_expression_type_name: BooleanExpressionTypeIdentifier,
        underlying_type: QualifiedTypeName,
    },

    #[error("Scalar representations for data connector '{data_connector_name}' could not found for boolean expression type {boolean_expression_type}")]
    DataConnectorScalarRepresentationsNotFound {
        data_connector_name: Qualified<DataConnectorName>,
        boolean_expression_type: Qualified<CustomTypeName>,
    },

    #[error("Field level comparison operator configuration is not fully supported yet. Please use \"enableAll\":true.")]
    FieldLevelComparisonOperatorConfigurationNotSupported,

    #[error("Field level comparison operator configuration is not fully supported yet. Please add all fields in filterable_fields.")]
    FieldLevelComparisonOperatorNeedsAllFields,

    #[error("a source must be defined for model {model:} in order to use filter expressions")]
    CannotUseFilterExpressionsWithoutSource { model: Qualified<ModelName> },

    #[error(
        "Target model {model_name:} not found, referenced in relationship {relationship_name:}"
    )]
    TargetModelNotFound {
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
    },

    #[error("The relationship '{relationship_name}' cannot be used as a comparable relationship in boolean expression type '{boolean_expression_type_name}' because it is a remote relationship with an argument mapping target. Relationship '{relationship_name}' on type '{source_type}' has source field '{source_field}' mapped to target argument '{target_argument}'")]
    RemoteComparableRelationshipWithArgumentMappingTargetNotSupported {
        boolean_expression_type_name: Qualified<CustomTypeName>,
        relationship_name: open_dds::relationships::RelationshipName,
        source_type: Qualified<CustomTypeName>,
        source_field: FieldName,
        target_argument: open_dds::query::ArgumentName,
    },

    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),

    #[error("{0}")]
    ScalarBooleanExpressionTypeError(
        #[from] scalar_boolean_expressions::ScalarBooleanExpressionTypeError,
    ),

    #[error("data connector error in boolean expression {boolean_expression_name:}: {data_connector_error:}")]
    DataConnectorError {
        boolean_expression_name: Qualified<CustomTypeName>,
        data_connector_error: data_connectors::NamedDataConnectorError,
    },

    #[error("{0}")]
    TypePredicateError(#[from] TypePredicateError),

    #[error("{0}")]
    RelationshipError(#[from] relationships::RelationshipError),
}
