use crate::stages::graphql_config;
use crate::types::subgraph::{Qualified, QualifiedBaseType};

use open_dds::{
    order_by_expression::OrderByExpressionName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, TypeName},
};

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
    #[error("Invalid orderable field {field_name}. Exactly one of `enable_order_by_directions` or `order_by_expression_name` must be specified.")]
    InvalidOrderByExpressionOrderableField { field_name: FieldName },
    #[error("The order by expression {order_by_expression_name} referenced in field {field_name} has not been defined")]
    UnknownOrderByExpressionNameInOrderableField {
        order_by_expression_name: OrderByExpressionName,
        field_name: FieldName,
    },
    #[error("The order by expression {order_by_expression_name} referenced in orderable relationship {relationship_name} has not been defined")]
    UnknownOrderByExpressionNameInOrderableRelationship {
        order_by_expression_name: OrderByExpressionName,
        relationship_name: RelationshipName,
    },
    #[error("The type of the order by expression {order_by_expression_name} referenced in field {field_name} does not match the field type. Order by expression type: {order_by_expression_type}; field type: {field_type}. ")]
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
