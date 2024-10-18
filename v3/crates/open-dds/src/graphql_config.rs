//! GraphqlConfig object tells us two things:
//! 1. How the Graphql schema should look like for the features (`where`, `order_by` etc) Hasura provides
//! 2. What features should be enabled/disabled across the subgraphs

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::types::{GraphQlFieldName, GraphQlTypeName};

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(as_versioned_with_definition, json_schema(title = "GraphqlConfig"))]

/// GraphqlConfig object tells us two things:
///
/// 1. How the Graphql schema should look like for the features (`where`, `order_by` etc) Hasura provides
/// 2. What features should be enabled/disabled across the subgraphs
pub enum GraphqlConfig {
    V1(GraphqlConfigV1),
}

/// GraphqlConfig object tells us two things:
///
/// 1. How the Graphql schema should look like for the features (`where`, `order_by` etc) Hasura provides
/// 2. What features should be enabled/disabled across the subgraphs
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "GraphqlConfigV1"))]
pub struct GraphqlConfigV1 {
    pub query: QueryGraphqlConfig,
    pub mutation: MutationGraphqlConfig,
    pub subscription: Option<SubscriptionGraphqlConfig>,
    pub apollo_federation: Option<GraphqlApolloFederationConfig>,
}

/// Configuration for the GraphQL schema of Hasura features for queries.
/// `None` means disable the feature.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "QueryGraphqlConfig"))]
pub struct QueryGraphqlConfig {
    /// The name of the root operation type name for queries. Usually `query`.
    pub root_operation_type_name: GraphQlTypeName,
    /// Configuration for the arguments input.
    pub arguments_input: Option<ArgumentsInputGraphqlConfig>,
    /// Configuration for the limit operation.
    pub limit_input: Option<LimitInputGraphqlConfig>,
    /// Configuration for the offset operation.
    pub offset_input: Option<OffsetInputGraphqlConfig>,
    /// Configuration for the filter operation.
    pub filter_input: Option<FilterInputGraphqlConfig>,
    /// Configuration for the sort operation.
    pub order_by_input: Option<OrderByInputGraphqlConfig>,
    /// Configuration for aggregates
    pub aggregate: Option<AggregateGraphqlConfig>,
}

/// Configuration for the arguments input.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ArgumentsInputGraphqlConfig"))]
pub struct ArgumentsInputGraphqlConfig {
    /// The name of arguments passing field. Usually `args`.
    pub field_name: GraphQlFieldName,
}

/// Configuration for the limit operation.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "LimitInputGraphqlConfig"))]
pub struct LimitInputGraphqlConfig {
    /// The name of the limit operation field. Usually `limit`.
    pub field_name: GraphQlFieldName,
}

/// Configuration for the offset operation.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OffsetInputGraphqlConfig"))]
pub struct OffsetInputGraphqlConfig {
    /// The name of the offset operation field. Usually `offset`.
    pub field_name: GraphQlFieldName,
}

/// Configuration for the filter operation.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "FilterInputGraphqlConfig"))]
pub struct FilterInputGraphqlConfig {
    /// The name of the filter operation field. Usually `where`.
    pub field_name: GraphQlFieldName,
    /// The names of built-in filter operators.
    pub operator_names: FilterInputOperatorNames,
}

/// The names of built-in filter operators.
#[derive(Serialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "FilterInputOperatorNames"))]
pub struct FilterInputOperatorNames {
    /// The name of the `and` operator. Usually `_and`.
    pub and: GraphQlFieldName,
    /// The name of the `or` operator. Usually `_or`.
    pub or: GraphQlFieldName,
    /// The name of the `not` operator. Usually `_not`.
    pub not: GraphQlFieldName,
    /// The name of the `is null` operator. Usually `_is_null`.
    pub is_null: GraphQlFieldName,
}

/// Configuration for the sort operation.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByInputGraphqlConfig"))]
pub struct OrderByInputGraphqlConfig {
    /// The name of the filter operation field. Usually `order_by`.
    pub field_name: GraphQlFieldName,
    /// The names of the direction parameters.
    pub enum_direction_values: OrderByDirectionValues,
    pub enum_type_names: Vec<OrderByEnumTypeName>,
}

/// The names of the direction parameters.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByDirectionValues"))]
pub struct OrderByDirectionValues {
    /// The name of the ascending parameter. Usually `Asc`.
    pub asc: GraphQlFieldName,
    /// The name of the descending parameter. Usually `Desc`.
    pub desc: GraphQlFieldName,
}

/// Sort direction.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Copy,
    Debug,
    PartialEq,
    JsonSchema,
    Eq,
    Hash,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
#[serde(deny_unknown_fields)]
#[schemars(title = "OrderByDirection")]
pub enum OrderByDirection {
    /// Ascending.
    Asc,
    /// Descending.
    Desc,
}

/// Type name for a sort directions enum, with the given set of possible directions.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "OrderByEnumTypeName"))]
pub struct OrderByEnumTypeName {
    pub directions: Vec<OrderByDirection>,
    pub type_name: GraphQlTypeName,
}

/// Configuration for the GraphQL schema of Hasura features for mutations.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "MutationGraphqlConfig"))]
pub struct MutationGraphqlConfig {
    /// The name of the root operation type name for mutations. Usually `mutation`.
    pub root_operation_type_name: GraphQlTypeName,
}

/// Configuration for the GraphQL schema of Hasura features for subscriptions.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "SubscriptionGraphqlConfig"))]
pub struct SubscriptionGraphqlConfig {
    /// The name of the root operation type name for subscriptions. Usually `subscription`.
    pub root_operation_type_name: GraphQlTypeName,
}

/// Configuration for the GraphQL schema of Hasura features for Apollo Federation.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "GraphqlApolloFederationConfig"))]
pub struct GraphqlApolloFederationConfig {
    /// Adds the `_entities` and `_services` root fields required for Apollo Federation.
    pub enable_root_fields: bool,
}

/// Configuration for the GraphQL schema for aggregates.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "AggregateGraphqlConfig"))]
pub struct AggregateGraphqlConfig {
    /// The name of the filter input parameter of aggregate fields and field name in predicates
    pub filter_input_field_name: GraphQlFieldName,
    /// The name of the _count field used for the count aggregate function
    pub count_field_name: GraphQlFieldName,
    /// The name of the _count_distinct field used for the count distinct aggregate function
    pub count_distinct_field_name: GraphQlFieldName,
}
