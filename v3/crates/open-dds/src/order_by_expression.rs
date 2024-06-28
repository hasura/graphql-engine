use serde::{Deserialize, Serialize};

use crate::{
    identifier::Identifier,
    models::{EnableAllOrSpecific, OrderByDirection},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, GraphQlTypeName},
};

/// Definition of an order by expression on an OpenDD type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "OrderByExpression", example = "OrderByExpression::example")
)]
pub enum OrderByExpression {
    V1(OrderByExpressionV1),
}

impl OrderByExpression {
    fn example() -> serde_json::Value {
        serde_json::json!(
          {
            "kind": "OrderByExpression",
            "version": "v1",
            "definition": {
              "name": "Album_order_by_exp",
              "orderedType": "Album",
              "orderableFields": [
                {
                  "fieldName": "AlbumId",
                  "enableOrderByDirections": [
                    "Asc",
                    "Desc"
                  ]
                },
                {
                  "fieldName": "ArtistId",
                  "enableOrderByDirections": [
                    "Asc"
                  ]
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
              ],
              "graphql": {
                "expressionTypeName": "App_Album_order_by_exp"
              }
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

#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    PartialOrd,
    Ord,
    opendds_derive::OpenDd,
)]
pub struct OrderByExpressionName(pub Identifier);

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionV1",))]
pub struct OrderByExpressionV1 {
    /// The name used to refer to this expression.
    /// This name is unique only in the context of the `orderedType`
    pub name: OrderByExpressionName,

    /// The type that this expression applies to.
    pub ordered_type: CustomTypeName,
    pub orderable_fields: Vec<OrderByExpressionOrderableField>,

    pub orderable_relationships: Vec<OrderByExpressionOrderableRelationship>,
    pub graphql: Option<OrderByExpressionGraqphQlConfiguration>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionOrderableField",))]
pub struct OrderByExpressionOrderableField {
    pub field_name: FieldName,

    /// Order by directions supported by this field.
    /// Only applicable if the field has a scalar type.
    pub enable_order_by_directions: EnableAllOrSpecific<OrderByDirection>,

    /// OrderByExpression to use for this field.
    /// Only applicable if the field has an object type.
    pub order_by_expression: Option<OrderByExpressionName>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByExpressionOrderableRelationship",))]
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
pub struct OrderByExpressionGraqphQlConfiguration {
    pub expression_type_name: GraphQlTypeName,
}
