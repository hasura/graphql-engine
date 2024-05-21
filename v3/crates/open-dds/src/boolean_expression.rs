use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::{
    data_connector::{DataConnectorName, DataConnectorOperatorName, DataConnectorScalarType},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, GraphQlTypeName, OperatorName, TypeName, TypeReference},
};

/// Definition of a type representing a boolean expression on an OpenDD type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "BooleanExpressionType",
        example = "BooleanExpressionType::example"
    )
)]
pub enum BooleanExpressionType {
    V1(BooleanExpressionTypeV1),
}

impl BooleanExpressionType {
    fn example() -> serde_json::Value {
        serde_json::json!(
          {
            "kind": "BooleanExpressionType",
            "version": "v1",
            "definition": {
              "name": "Album_bool_exp",
              "operand": {
                "object": {
                  "type": "Album",
                  "comparableFields": [
                    {
                      "fieldName": "AlbumId",
                      "booleanExpressionType": "pg_Int_Comparison_exp"
                    },
                    {
                      "fieldName": "ArtistId",
                      "booleanExpressionType": "pg_Int_Comparison_exp_with_is_null"
                    },
                    {
                      "fieldName": "Address",
                      "booleanExpressionType": "Address_bool_exp"
                    }
                  ],
                  "comparableRelationships": [
                    {
                      "relationshipName": "artist",
                      "booleanExpressionType": "Artist_bool_exp"
                    }
                  ]
                }
              },
              "logicalOperators": {
                "enable": true
              },
              "isNull": {
                "enable": true
              },
              "graphql": {
                "typeName": "App_Album_bool_exp"
              }
            }
          }
        )
    }

    pub fn upgrade(self) -> BooleanExpressionTypeV1 {
        match self {
            BooleanExpressionType::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionTypeV1",))]
/// Definition of a type representing a boolean expression on an OpenDD object type.
pub struct BooleanExpressionTypeV1 {
    /// The name to give this boolean expression type, used to refer to it elsewhere in the metadata.
    /// Must be unique across all types defined in this subgraph.
    pub name: CustomTypeName,

    /// The type that this boolean expression applies to.
    pub operand: BooleanExpressionOperand,

    /// Whether to enable _and / _or / _not
    pub logical_operators: BooleanExpressionLogicalOperators,

    /// Whether to enable _is_null
    pub is_null: BooleanExpressionIsNull,

    /// Configuration for how this object type should appear in the GraphQL schema.
    pub graphql: Option<BooleanExpressionTypeGraphQlConfiguration>,
}

/// Configuration for is_null in boolean expressions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionIsNull"))]
pub struct BooleanExpressionIsNull {
    pub enable: bool,
}

/// Configuration for logical operators in boolean expressions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionLogicalOperators"))]
pub struct BooleanExpressionLogicalOperators {
    pub enable: bool,
}

/// GraphQL configuration of an OpenDD boolean expression type.
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionTypeGraphQlConfiguration"))]
pub struct BooleanExpressionTypeGraphQlConfiguration {
    /// The name to use for the GraphQL type representation of this boolean expression type.
    pub type_name: GraphQlTypeName,
}

/// Configuration for object or scalar boolean expression
#[derive(Serialize, Deserialize, JsonSchema, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionOperand"))]
pub enum BooleanExpressionOperand {
    #[serde(rename = "object")]
    /// Definition of a boolean expression on an OpenDD object type
    Object(BooleanExpressionObjectOperand),
    #[serde(rename = "scalar")]
    /// Definition of a boolean expression on a scalar tyoe
    Scalar(BooleanExpressionScalarOperand),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ComparisonOperator"))]
pub struct ComparisonOperator {
    /// Name you want to give the operator in OpenDD / GraphQL
    pub name: OperatorName,

    /// An OpenDD type
    pub argument_type: TypeReference,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "DataConnectorOperatorMapping"))]
pub struct DataConnectorOperatorMapping {
    /// Name of the data connector this mapping applies to
    pub data_connector_name: DataConnectorName,

    /// Name of the scalar type according to the data connector's schema
    pub data_connector_scalar_type: DataConnectorScalarType,

    /// Mapping between OpenDD operator names and the data connector's operator names
    /// Defaults to the same operator name (e.g. "_eq: _eq") if no explicit mapping is present.
    pub operator_mapping: BTreeMap<OperatorName, DataConnectorOperatorName>,
}

/// Definition of a scalar type representing a boolean expression on an OpenDD object type.
#[derive(Serialize, Deserialize, Clone, JsonSchema, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionScalarOperand"))]
pub struct BooleanExpressionScalarOperand {
    /// The OpenDD type name of the scalar type that this boolean expression applies to.
    #[opendd(rename = "type")]
    pub r#type: TypeName,

    /// The list of comparison operators that can used on this scalar type
    pub comparison_operators: Vec<ComparisonOperator>,

    /// The list of mappings between OpenDD operator names and the names used in the data
    /// connector schema
    pub data_connector_operator_mapping: Vec<DataConnectorOperatorMapping>,
}

/// Definition of an object type representing a boolean expression on an OpenDD object type.
#[derive(Serialize, Deserialize, JsonSchema, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionObjectOperand"))]
pub struct BooleanExpressionObjectOperand {
    /// The name of the object type that this boolean expression applies to.
    #[opendd(rename = "type")]
    pub r#type: CustomTypeName,

    /// The list of fields of the object type that can be used for comparison when evaluating this boolean expression.
    pub comparable_fields: Vec<BooleanExpressionComparableField>,

    /// The list of relationships of the object type that can be used for comparison when
    /// evaluating this boolean expression.
    pub comparable_relationships: Vec<BooleanExpressionComparableRelationship>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionComparableField"))]
pub struct BooleanExpressionComparableField {
    pub field_name: FieldName,
    pub boolean_expression_type: CustomTypeName,
}

/// Definition of a relationship that can be used for a comparison
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "BooleanExpressionComparableRelationship"))]
pub struct BooleanExpressionComparableRelationship {
    /// The name of the relationship to use for comparison
    relationship_name: RelationshipName,

    /// The boolean expression type to use for comparison. This is optional for relationships to
    /// models, and defaults to the filterExpressionType of the model
    boolean_expression_type: Option<CustomTypeName>,
}
