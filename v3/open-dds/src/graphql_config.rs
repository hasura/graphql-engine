//! GraphqlConfig object tells us two things:
//!    1. How the Graphql schema should look like for the features (`where`, `order_by` etc) hasura provides
//!    2. What features should be enabled/disabled across the subgraphs
//!
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "GraphqlConfig")]

pub enum GraphqlConfig {
    V1(GraphqlConfigV1),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "GraphqlConfigV1")]
pub struct GraphqlConfigV1 {
    pub query: QueryGraphqlConfig,
    pub mutation: MutationGraphqlConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "QueryGraphqlConfig")]
pub struct QueryGraphqlConfig {
    pub root_operation_type_name: String,
    pub arguments_input: Option<ArgumentsInputGraphqlConfig>,
    pub limit_input: Option<LimitInputGraphqlConfig>,
    pub offset_input: Option<OffsetInputGraphqlConfig>,
    pub filter_input: Option<FilterInputGraphqlConfig>,
    pub order_by_input: Option<OrderByInputGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ArgumentsInputGraphqlConfig")]
pub struct ArgumentsInputGraphqlConfig {
    pub field_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LimitInputGraphqlConfig")]
pub struct LimitInputGraphqlConfig {
    pub field_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "OffsetInputGraphqlConfig")]
pub struct OffsetInputGraphqlConfig {
    pub field_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "FilterInputGraphqlConfig")]
pub struct FilterInputGraphqlConfig {
    pub field_name: String,
    pub operator_names: FilterInputOperatorNames,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "FilterInputOperatorNames")]
pub struct FilterInputOperatorNames {
    pub and: String,
    pub or: String,
    pub not: String,
    pub is_null: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, Eq, Hash)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "OrderByInputGraphqlConfig")]
pub struct OrderByInputGraphqlConfig {
    pub field_name: String,
    pub enum_direction_values: OrderByDirectionValues,
    pub enum_type_names: Vec<OrderByEnumTypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, Eq, Hash)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "OrderByDirectionValues")]
pub struct OrderByDirectionValues {
    pub asc: String,
    pub desc: String,
}

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, Eq, Hash, derive_more::Display,
)]
#[serde(deny_unknown_fields)]
#[schemars(title = "OrderByDirection")]
pub enum OrderByDirection {
    Asc,
    Desc,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, Eq, Hash)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "OrderByEnumTypeName")]
pub struct OrderByEnumTypeName {
    pub directions: Vec<OrderByDirection>,
    pub type_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "MutationGraphqlConfig")]
pub struct MutationGraphqlConfig {
    pub root_operation_type_name: String,
}
