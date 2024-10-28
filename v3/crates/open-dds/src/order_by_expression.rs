use serde::{Deserialize, Serialize};

use crate::{
    identifier::Identifier,
    models::{EnableAllOrSpecific, OrderByDirection},
    relationships::RelationshipName,
    str_newtype,
    types::{CustomTypeName, FieldName, GraphQlTypeName, TypeName},
};

/// Definition of an order by expression on an OpenDD type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "OrderByExpression",
        example = "OrderByExpression::object_example",
        example = "OrderByExpression::scalar_example",
    )
)]
pub enum OrderByExpression {
    V1(OrderByExpressionV1),
}

impl OrderByExpression {
    fn object_example() -> serde_json::Value {
        serde_json::json!(
          {
            "kind": "OrderByExpression",
            "version": "v1",
            "definition": {
              "name": "Album_order_by_exp",
              "operand": {
                "object": {
                  "orderedType": "Album",
                  "orderableFields": [
                    {
                      "fieldName": "AlbumId",
                      "orderByExpression": "Int_order_by_exp"
                    },
                    {
                      "fieldName": "ArtistId",
                      "orderByExpression": "Int_order_by_exp"
                    },
                    {
                      "fieldName": "Address",
                      "orderByExpression": "Address_order_by_default_exp"
                    }
                  ],
                  "orderableRelationships": [
                    {
                      "relationshipName": "artist",
                      "orderByExpression": "Artist_order_by_default_exp"
                    }
                  ]
                }
              },
              "graphql": {
                "expressionTypeName": "App_Album_order_by_exp"
              },
              "description": "Order by expression for Albums"
            }
          }
        )
    }

    fn scalar_example() -> serde_json::Value {
        serde_json::json!(
          {
            "kind": "OrderByExpression",
            "version": "v1",
            "definition": {
              "name": "Int_order_by_exp",
              "operand": {
                "scalar": {
                  "orderedType": "Int",
                  "enableOrderByDirections": {
                      "enableAll": true
                  }
                }
              },
              "graphql": {
                "expressionTypeName": "App_Int_order_by_exp"
              },
              "description": "Order by expression for Int"
            }
          }
        )
    }

    pub fn upgrade(self) -> OrderByExpressionV1 {
        match self {
            OrderByExpression::V1(v1) => v1,
        }
    }
}

str_newtype!(OrderByExpressionName over Identifier | doc "The name of an order by expression.");

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionV1",))]
/// Definition of a type representing an order by expression on an OpenDD type.
pub struct OrderByExpressionV1 {
    /// The name used to refer to this expression.
    /// This name is unique only in the context of the `orderedType`
    pub name: OrderByExpressionName,

    /// The type that this expression applies to.
    pub operand: OrderByExpressionOperand,

    /// Configuration for how this order by expression should appear in the GraphQL schema.
    pub graphql: Option<OrderByExpressionGraphQlConfiguration>,

    /// The description of the order by expression.
    pub description: Option<String>,
}

/// Configuration for object or scalar order by expression
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "OrderByExpressionOperand"))]
pub enum OrderByExpressionOperand {
    /// Definition of an order by expression on an OpenDD object type
    #[opendd(json_schema(title = "Object"))]
    Object(OrderByExpressionObjectOperand),
    /// Definition of an order by expression on an OpenDD scalar type
    #[opendd(json_schema(title = "Scalar"))]
    Scalar(OrderByExpressionScalarOperand),
}

/// Definition of an type representing an order by expression on an OpenDD object type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "OrderByExpressionObjectOperand"))]
pub struct OrderByExpressionObjectOperand {
    /// The type that this expression applies to.
    pub ordered_type: CustomTypeName,

    /// Orderable fields of the `orderedType`
    pub orderable_fields: Vec<OrderByExpressionOrderableField>,

    /// Orderable relationships
    pub orderable_relationships: Vec<OrderByExpressionOrderableRelationship>,
}

/// Definition of a type representing an order by expression on an OpenDD scalar type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "OrderByExpressionScalarOperand"))]
pub struct OrderByExpressionScalarOperand {
    /// The type that this expression applies to.
    pub ordered_type: TypeName,

    /// Order by directions supported by this scalar type.
    pub enable_order_by_directions: EnableAllOrSpecific<OrderByDirection>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionOrderableField",))]
/// Definition of a field in a type representing an order by expression on an OpenDD type.
pub struct OrderByExpressionOrderableField {
    pub field_name: FieldName,

    /// OrderByExpression to use for this field.
    pub order_by_expression: OrderByExpressionName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionOrderableRelationship",))]
/// Definition of a relationship in a type representing an order by expression on an OpenDD type.
pub struct OrderByExpressionOrderableRelationship {
    /// The name of the relationship.
    pub relationship_name: RelationshipName,

    /// The OrderByExpression to use for this relationship.
    /// This is optional for model relationships.
    /// If not specified we use the model's OrderByExpression configuration.
    /// For local command relationships this is required.
    pub order_by_expression: Option<OrderByExpressionName>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionGraphQlConfiguration",))]
/// GraphQL configuration settings for a type representing an order by expression on an OpenDD type.
pub struct OrderByExpressionGraphQlConfiguration {
    pub expression_type_name: GraphQlTypeName,
}
