use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::{
    aggregates::{AggregateExpressionName, AggregationFunctionName},
    data_connector::{DataConnectorName, DataConnectorOperatorName, DataConnectorScalarType},
    models::ModelName,
    order_by_expression::OrderByExpressionName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, GraphQlTypeName, OperatorName, TypeName, TypeReference},
};

/// Definition of a type representing a boolean expression on an OpenDD type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
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
#[serde(rename_all = "camelCase", deny_unknown_fields)]
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
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionIsNull"))]
pub struct BooleanExpressionIsNull {
    pub enable: bool,
}

/// Configuration for logical operators in boolean expressions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionLogicalOperators"))]
pub struct BooleanExpressionLogicalOperators {
    pub enable: bool,
}

/// GraphQL configuration of an OpenDD boolean expression type.
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionTypeGraphQlConfiguration"))]
pub struct BooleanExpressionTypeGraphQlConfiguration {
    /// The name to use for the GraphQL type representation of this boolean expression type.
    pub type_name: GraphQlTypeName,
}

/// Configuration for object or scalar boolean expression
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "BooleanExpressionOperand"))]
pub enum BooleanExpressionOperand {
    /// Definition of a boolean expression on an OpenDD object type
    #[opendd(json_schema(title = "Object"))]
    Object(BooleanExpressionObjectOperand),
    /// Definition of a boolean expression on a scalar tyoe
    #[opendd(json_schema(title = "Scalar"))]
    Scalar(BooleanExpressionScalarOperand),
    /// Definition of a boolean expression on an aggregate of an OpenDD object type
    #[opendd(hidden, json_schema(title = "ObjectAggregate"))]
    ObjectAggregate(BooleanExpressionObjectAggregateOperand),
    /// Definition of a boolean expression on an aggregate of an OpenDD object type
    #[opendd(hidden, json_schema(title = "ScalarAggregate"))]
    ScalarAggregate(BooleanExpressionScalarAggregateOperand),
}

/// Definition of a comparison operator for a scalar type
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "ComparisonOperator"))]
pub struct ComparisonOperator {
    /// Name you want to give the operator in OpenDD / GraphQL
    pub name: OperatorName,

    /// An OpenDD type
    pub argument_type: TypeReference,
}

/// Mapping between OpenDD operator names and the names used in the data connector schema
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "DataConnectorOperatorMapping"))]
pub struct DataConnectorOperatorMapping {
    /// Name of the data connector this mapping applies to
    pub data_connector_name: DataConnectorName,

    /// Name of the scalar type according to the data connector's schema
    pub data_connector_scalar_type: DataConnectorScalarType,

    /// Mapping between OpenDD operator names and the data connector's operator names
    /// Defaults to the same operator name (e.g. "_eq: _eq") if no explicit mapping is present.
    #[opendd(json_schema(title = "operator_mapping"))]
    pub operator_mapping: BTreeMap<OperatorName, DataConnectorOperatorName>,
}

/// Definition of a scalar type representing a boolean expression on an OpenDD scalar type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
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
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
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

/// Definition of an object type representing a boolean expression on an OpenDD object type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionObjectAggregateOperand"))]
pub struct BooleanExpressionObjectAggregateOperand {
    /// The name of the object type that this boolean expression applies to.
    #[opendd(rename = "type")]
    pub r#type: CustomTypeName,

    /// The aggregate expression that describes how to aggregate the object type
    pub aggregate_expression: AggregateExpressionName,

    /// The list of fields of the object type that can be used for comparison when evaluating this boolean expression.
    pub comparable_fields: Vec<AggregateBooleanExpressionComparableField>,

    /// The list of relationships of the object type that can be used for comparison when
    /// evaluating this boolean expression.
    pub comparable_relationships: Vec<AggregateBooleanExpressionComparableRelationship>,

    /// Configures comparisons against the results of the count aggregate applied to the objectAggregate
    /// operand
    pub comparable_count: Option<AggregateBooleanExpressionComparableCount>,

    /// Configures comparisons against the results of the count distinct aggregate applied to the objectAggregate
    /// operand
    pub comparable_count_distinct: Option<AggregateBooleanExpressionComparableCount>,

    /// Configures how to filter the objects being aggregated over before they are aggregated
    pub filter_input: Option<ObjectAggregateFilterInput>,

    /// GraphQL configuration options for the object aggregate operand
    pub graphql: Option<BooleanExpressionAggregateOperandGraphqlConfiguration>,
}

/// Definition of an object type representing a boolean expression on an OpenDD object type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionScalarAggregateOperand"))]
pub struct BooleanExpressionScalarAggregateOperand {
    /// The name of the object type that this boolean expression applies to.
    #[opendd(rename = "type")]
    pub r#type: TypeName,

    /// The aggregate expression that describes how to aggregate the scalar type
    pub aggregate_expression: AggregateExpressionName,

    /// The list of aggregation functions whose results can be used for comparison when evaluating this boolean expression
    pub comparable_aggregation_functions: Vec<BooleanExpressionComparableAggregationFunction>,

    /// Configures comparisons against the results of the count aggregate applied to the scalarAggregate
    /// operand
    pub comparable_count: Option<AggregateBooleanExpressionComparableCount>,

    /// Configures comparisons against the results of the count distinct aggregate applied to the scalarAggregate
    /// operand
    pub comparable_count_distinct: Option<AggregateBooleanExpressionComparableCount>,
    // TODO: Required when supporting aggregations over nested arrays of scalars
    // /// Configures how to filter the objects being aggregated over before they are aggregated
    // pub filter_input: Option<CustomFilterInput>,

    // TODO: Required when supporting aggregations over nested arrays of scalars
    // /// GraphQL configuration options for the scalar aggregate operand
    // pub graphql: Option<BooleanExpressionAggregateOperandGraphqlConfiguration>,
}

/// Comparison configuration definition for a field that can be used for a comparison
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionComparableField"))]
pub struct BooleanExpressionComparableField {
    /// The name of the field that can be compared.
    pub field_name: FieldName,

    /// The boolean expression type that can be used for comparison against the type
    /// of the field.
    pub boolean_expression_type: CustomTypeName,
}

/// Comparison configuration definition for a field that can be used for a comparison in aggregate boolean expression
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateBooleanExpressionComparableField"))]
pub struct AggregateBooleanExpressionComparableField {
    /// The name of the field that can be compared.
    pub field_name: FieldName,

    /// The boolean expression type that can be used for comparison against an aggregate of the type
    /// of the field.
    pub aggregate_boolean_expression_type: CustomTypeName,
}

/// Definition of a relationship that can be used for a comparison
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionComparableRelationship"))]
pub struct BooleanExpressionComparableRelationship {
    /// The name of the relationship to use for comparison
    pub relationship_name: RelationshipName,

    /// The boolean expression type to use for comparison. This is optional for relationships to
    /// models, and defaults to the filterExpressionType of the model
    pub boolean_expression_type: Option<CustomTypeName>,

    /// The boolean expression type to use for comparisons against aggregates of the target of
    /// the relationship. The specified boolean expression must be an objectAggregate boolean expression.
    /// This is optional, but if omitted one will be unable to perform aggregate comparisons.
    #[opendd(hidden)]
    pub aggregate_boolean_expression_type: Option<CustomTypeName>,
}

/// Definition of a relationship that can be used for a comparison in an objectAggregate boolean expression
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateBooleanExpressionComparableRelationship"))]
pub struct AggregateBooleanExpressionComparableRelationship {
    /// The name of the relationship to use for comparison
    pub relationship_name: RelationshipName,

    /// The boolean expression type to use for comparisons against aggregates of the target of
    /// the relationship. The specified boolean expression must be an objectAggregate boolean expression.
    pub aggregate_boolean_expression_type: CustomTypeName,
}

/// Configures comparisons against the results of a count aggregate
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateBooleanExpressionComparableCount"))]
pub struct AggregateBooleanExpressionComparableCount {
    /// The boolean expression type to use for comparison against the result of the count aggregate
    pub boolean_expression_type: CustomTypeName,
}

/// Definition of how to filter the input to an aggregate before the aggregate is performed
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "ObjectAggregateFilterInput"))]
pub enum ObjectAggregateFilterInput {
    /// Source the filter input settings from a model
    #[opendd(json_schema(title = "Model"))]
    Model(ModelFilterInput),
    // TODO: Required when doing nested array aggregations
    // /// Configure custom filter input settings
    // #[opendd(json_schema(title = "Custom"))]
    // Custom(CustomFilterInput),
}

/// Definition of how to filter input to aggregates based on an existing Model's filtering settings
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "ModelFilterInput"))]
pub struct ModelFilterInput {
    /// Filtering will be performed the same as configured in the specified Model
    pub model_name: ModelName,
}

/// Custom definition of how to filter input to aggregates
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "CustomFilterInput"))]
pub struct CustomFilterInput {
    /// The boolean expression type that should be used to perform filtering.
    pub filter_expression_type: CustomTypeName,

    /// The order by expression that should be used to perform ordering.
    pub order_by_expression: OrderByExpressionName,

    /// GraphQL configuration of the custom filter input
    pub graphql: Option<CustomFilterInputGraphqlConfiguration>,
}

/// GraphQL configuration options for the aggregate filter input
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "CustomFilterInputGraphqlConfiguration"))]
pub struct CustomFilterInputGraphqlConfiguration {
    /// The name to use for the filter input GraphQL type
    pub type_name: GraphQlTypeName,
}

/// Definition of an aggregation function whose results can be compared against
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionComparableAggregationFunction"))]
pub struct BooleanExpressionComparableAggregationFunction {
    /// The name of the aggregation function
    pub name: AggregationFunctionName,

    /// The boolean expression type to use for comparison against the result of the aggregation function
    pub boolean_expression_type: CustomTypeName,
}

/// GraphQL configuration options for boolean expression aggregate operands
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "BooleanExpressionAggregateOperandGraphqlConfiguration"))]
pub struct BooleanExpressionAggregateOperandGraphqlConfiguration {
    /// The name of the GraphQL type that captures the filter input and predicate over the aggregate results
    pub predicate_type_name: GraphQlTypeName,
}
