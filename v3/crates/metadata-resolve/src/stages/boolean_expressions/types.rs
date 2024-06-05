use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use open_dds::types::{CustomTypeName, FieldName, GraphQlTypeName, OperatorName, TypeReference};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use lang_graphql::ast::common as ast;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionTypes {
    pub objects: BTreeMap<Qualified<CustomTypeName>, ResolvedObjectBooleanExpressionType>,
    pub scalars: BTreeMap<Qualified<CustomTypeName>, ResolvedScalarBooleanExpressionType>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]

pub struct BooleanExpressionsOutput {
    pub boolean_expression_types: BooleanExpressionTypes,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub graphql: Option<BooleanExpressionGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedScalarBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,

    /// The list of comparison operators that can used on this scalar type
    pub comparison_operators: BTreeMap<OperatorName, TypeReference>,

    /// The list of mappings between OpenDD operator names and the names used in the data
    /// connector schema
    pub data_connector_operator_mappings: BTreeMap<
        Qualified<open_dds::data_connector::DataConnectorName>,
        open_dds::boolean_expression::DataConnectorOperatorMapping,
    >,

    // optional name for exposing this in the GraphQL schema
    pub graphql_name: Option<GraphQlTypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    pub type_name: ast::TypeName,
    pub operators: BTreeMap<OperatorName, QualifiedTypeReference>,
    pub is_null_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlFieldConfig {
    pub where_field_name: ast::Name,
    pub and_operator_name: ast::Name,
    pub or_operator_name: ast::Name,
    pub not_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub scalar_fields: BTreeMap<FieldName, ComparisonExpressionInfo>,
    pub graphql_config: BooleanExpressionGraphqlFieldConfig,
}
