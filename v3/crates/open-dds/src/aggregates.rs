use std::ops::Deref;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    data_connector::{DataConnectorName, DataConnectorScalarType},
    identifier::Identifier,
    str_newtype,
    types::{CustomTypeName, Deprecated, FieldName, GraphQlTypeName, TypeName, TypeReference},
};

str_newtype!(AggregateExpressionName over Identifier | doc "The name of an aggregate expression.");

/// Definition of an aggregate expression on an OpenDD type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "AggregateExpression",
        example = "AggregateExpression::example"
    )
)]
pub enum AggregateExpression {
    V1(AggregateExpressionV1),
}

impl AggregateExpression {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "kind": "AggregateExpression",
            "version": "v1",
            "definition": {
                "name": "Invoice_aggregate_exp",
                "operand": {
                    "object": {
                        "aggregatedType": "Invoice",
                        "aggregatableFields": [
                            {
                                "fieldName": "Total",
                                "aggregateExpression": "Float_aggregate_exp"
                            }
                        ]
                    }
                },
                "graphql": {
                    "selectTypeName": "Invoice_aggregate_fields",
                },
                "description": "Aggregate over Invoices",
            }
        })
    }

    pub fn upgrade(self) -> AggregateExpressionV1 {
        match self {
            AggregateExpression::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateExpressionV1"))]
/// Definition of how to aggregate over a particular operand type
pub struct AggregateExpressionV1 {
    /// The name of the aggregate expression.
    pub name: AggregateExpressionName,
    /// The type this aggregation expression aggregates over, and its associated configuration
    pub operand: AggregateOperand,
    /// Configuration for the count aggregate function used over the operand
    pub count: Option<AggregateCountDefinition>,
    /// Configuration for the count distinct aggregate function used over the operand
    pub count_distinct: Option<AggregateCountDefinition>,
    /// Configuration for how this command should appear in the GraphQL schema.
    pub graphql: Option<AggregateExpressionGraphQlDefinition>,
    /// The description of the aggregate expression.
    /// Gets added to the description of the command's root field in the GraphQL schema.
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
/// Definition of an aggregate expression's operand
#[opendd(externally_tagged, json_schema(title = "AggregateOperand"))]
pub enum AggregateOperand {
    /// If the operand is an object type
    #[opendd(json_schema(title = "Object"))]
    Object(ObjectAggregateOperand),
    /// If the operand is a scalar type
    #[opendd(json_schema(title = "Scalar"))]
    Scalar(ScalarAggregateOperand),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "ObjectAggregateOperand"))]
/// Definition of an aggregate over an object-typed operand
pub struct ObjectAggregateOperand {
    /// The name of the object type the aggregate expression is aggregating
    pub aggregated_type: CustomTypeName,
    /// The fields on the object that are aggregatable
    pub aggregatable_fields: Vec<AggregatableFieldDefinition>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregatableFieldDefinition"))]
/// Definition of an aggregatable field on an object type
pub struct AggregatableFieldDefinition {
    /// The name of the field on the operand aggregated type that is aggregatable
    pub field_name: FieldName,
    /// A description of the aggregatable field.
    /// Gets added to the description of the field in the GraphQL schema.
    pub description: Option<String>,
    /// The aggregate expression used to aggregate the type of the field
    pub aggregate_expression: AggregateExpressionName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "ScalarAggregateOperand"))]
/// Definition of an aggregate over a scalar-typed operand
pub struct ScalarAggregateOperand {
    /// The name of the scalar type the aggregate expression is aggregating
    pub aggregated_type: TypeName,
    /// The aggregation functions that operate over the scalar type
    pub aggregation_functions: Vec<AggregationFunctionDefinition>,
    /// Mapping of aggregation functions to corresponding aggregation functions in various data connectors
    pub data_connector_aggregation_function_mapping: Vec<DataConnectorAggregationFunctionMapping>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregationFunctionDefinition"))]
/// Definition of an aggregation function
pub struct AggregationFunctionDefinition {
    /// The name of the aggregation function
    pub name: AggregationFunctionName,
    /// A description of the aggregation function.
    /// Gets added to the description of the field in the GraphQL schema.
    pub description: Option<String>,
    // The type that the aggregation function returns
    pub return_type: TypeReference,
}

str_newtype!(AggregationFunctionName over Identifier | doc "The name of an aggregation function.");

str_newtype!(ExtractionFunctionName | doc "The name of an extraction function.");

str_newtype!(DataConnectorAggregationFunctionName | doc "The name of an aggregation function in a data connector");

str_newtype!(DataConnectorExtractionFunctionName | doc "The name of an extraction function in a data connector");

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "DataConnectorAggregationFunctionMapping"))]
/// Definition of how to map an aggregate expression's aggregation functions to
/// data connector aggregation functions.
pub struct DataConnectorAggregationFunctionMapping {
    /// The data connector being mapped to
    pub data_connector_name: DataConnectorName,
    /// The matching scalar type in the data connector for the operand scalar type
    pub data_connector_scalar_type: DataConnectorScalarType,
    /// Mapping from Open DD aggregation function to data connector aggregation function
    pub function_mapping: AggregationFunctionMappings,
}

#[derive(Serialize, Deserialize, Default, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "AggregationFunctionMappings"))]
/// Mapping of aggregation functions to their matching aggregation functions in the data connector.
// We wrap maps into newtype structs so that we have a type and title for them in the JSONSchema which
// makes it easier to auto-generate documentation.
pub struct AggregationFunctionMappings(
    pub IndexMap<AggregationFunctionName, AggregateFunctionMapping>,
);

impl Deref for AggregationFunctionMappings {
    type Target = IndexMap<AggregationFunctionName, AggregateFunctionMapping>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Serialize, Deserialize, Default, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ExtractionFunctionMappings"))]
/// Mapping of extraction functions to their matching extraction functions in the data connector.
pub struct ExtractionFunctionMappings(
    pub IndexMap<ExtractionFunctionName, ExtractionFunctionMapping>,
);

impl Deref for ExtractionFunctionMappings {
    type Target = IndexMap<ExtractionFunctionName, ExtractionFunctionMapping>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateFunctionMapping"))]
/// Definition of how to map the aggregation function to a function in the data connector
pub struct AggregateFunctionMapping {
    /// The name of the aggregation function in the data connector
    pub name: DataConnectorAggregationFunctionName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "ExtractionFunctionMapping"))]
/// Definition of how to map the extraction function to a function in the data connector
pub struct ExtractionFunctionMapping {
    /// The name of the extraction function in the data connector
    pub name: DataConnectorExtractionFunctionName,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateCountDefinition"))]
/// Definition of a count aggregation function
pub struct AggregateCountDefinition {
    /// Whether or not the aggregate function is available for use or not
    pub enable: bool,

    /// A description of the aggregation function.
    /// Gets added to the description of the field in the GraphQL schema.
    pub description: Option<String>,

    /// The scalar type that the count aggregation function returns.
    /// Must be an integer type. If omitted, Int is used as the default.
    pub return_type: Option<TypeName>,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(json_schema(
    title = "AggregateExpressionGraphQlDefinition",
    example = "AggregateExpressionGraphQlDefinition::example"
))]
/// The definition of how an aggregate expression should appear in the GraphQL API.
pub struct AggregateExpressionGraphQlDefinition {
    /// The type name to use for the aggregate selection type
    pub select_type_name: GraphQlTypeName,
    /// Whether this command root field is deprecated.
    /// If set, this will be added to the graphql schema as a deprecated field.
    pub deprecated: Option<Deprecated>,
}

impl AggregateExpressionGraphQlDefinition {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "selectTypeName": "Invoice_aggregate_fields",
        })
    }
}
