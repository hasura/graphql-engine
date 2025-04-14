use crate::stages::graphql_config;
use crate::types::error::ContextualError;
use crate::types::subgraph::{Qualified, QualifiedBaseType};

use open_dds::{
    order_by_expression::OrderByExpressionName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, TypeName},
};

#[derive(Debug, thiserror::Error)]
#[error("Error in order by expression {order_by_expression_name}: {error}")]
pub struct NamedOrderByExpressionError {
    pub order_by_expression_name: Qualified<OrderByExpressionName>,
    pub error: OrderByExpressionError,
}

impl ContextualError for NamedOrderByExpressionError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        None
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OrderByExpressionError {
    #[error("unknown field {field_name} in orderable fields")]
    UnknownFieldInOrderByExpression { field_name: FieldName },
    #[error("The data type {data_type} has not been defined")]
    UnknownOrderableType {
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "The relationship {relationship_name} on object type {object_type_name} could not be found"
    )]
    UnknownRelationship {
        relationship_name: RelationshipName,
        object_type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "Invalid orderable field {field_name}. Exactly one of `enable_order_by_directions` or `order_by_expression_name` must be specified."
    )]
    InvalidOrderByExpressionOrderableField { field_name: FieldName },
    #[error(
        "The order by expression {order_by_expression_name} referenced in field {field_name} has not been defined"
    )]
    UnknownOrderByExpressionNameInOrderableField {
        order_by_expression_name: OrderByExpressionName,
        field_name: FieldName,
    },
    #[error(
        "The order by expression {order_by_expression_name} referenced in orderable relationship {relationship_name} has not been defined"
    )]
    UnknownOrderByExpressionNameInOrderableRelationship {
        order_by_expression_name: OrderByExpressionName,
        relationship_name: RelationshipName,
    },
    #[error(
        "The type of the order by expression {order_by_expression_name} referenced in field {field_name} does not match the field type. Order by expression type: {order_by_expression_type}; field type: {field_type}. "
    )]
    OrderableFieldTypeError {
        order_by_expression_name: OrderByExpressionName,
        order_by_expression_type: TypeName,
        field_type: QualifiedBaseType,
        field_name: FieldName,
    },
    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),
    #[error("{message}")]
    UnsupportedFeature { message: String },
}

#[derive(Debug, thiserror::Error)]
#[allow(clippy::enum_variant_names)]
pub enum OrderableRelationshipError {
    #[error(
        "The orderable relationship '{relationship_name}' defined for '{orderable_type}' is a remote relationship and remote relationships are not supported in ordering"
    )]
    RemoteRelationshipsNotSupported {
        orderable_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error(
        "The orderable relationship '{relationship_name}' defined for '{orderable_type}' is not supported in ordering because the data connector '{data_connector_name}' does not support relationships"
    )]
    RelationshipsNotSupported {
        orderable_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<open_dds::data_connector::DataConnectorName>,
    },
    #[error(
        "The orderable relationship '{relationship_name}' defined for '{orderable_type}' is not supported in ordering because the data connector '{data_connector_name}' does not support nested relationships in ordering"
    )]
    NestedRelationshipsNotSupported {
        orderable_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<open_dds::data_connector::DataConnectorName>,
    },
}
