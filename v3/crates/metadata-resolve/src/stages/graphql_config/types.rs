use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct GraphqlConfig {
    // The graphql configuration that needs to be applied to each model, depending on it's conditions
    pub query: QueryGraphqlConfig,
    // The grapqhl configuration that is global across the schema
    pub global: GlobalGraphqlConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[allow(clippy::struct_excessive_bools)]
pub struct GlobalGraphqlConfig {
    pub query_root_type_name: ast::TypeName,
    pub mutation_root_type_name: ast::TypeName,
    pub subscription_root_type_name: Option<ast::TypeName>,
    pub order_by_input: Option<OrderByInputGraphqlConfig>,
    pub enable_apollo_federation_fields: bool,
    pub bypass_relation_comparisons_ndc_capability: bool,
    pub propagate_boolean_expression_deprecation_status: bool,
    pub multiple_order_by_input_object_fields: MultipleOrderByInputObjectFields,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum MultipleOrderByInputObjectFields {
    /// Legacy behaviour
    Allow,

    /// Queries should reject input objects with multiple properties specified
    /// in a GraphQL query's order_by.
    /// For example: 'order_by: { name: asc, id: asc }' is invalid.
    /// Instead 'order_by: [{ name: asc }, { id: asc }]' should be used.
    /// Also, nested arrays are not allowed. For example:
    /// 'order_by: { nested: [ { name: asc }, { id: asc } ] }` is invalid.`
    Disallow,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByInputGraphqlConfig {
    pub asc_direction_field_value: ast::Name,
    pub desc_direction_field_value: ast::Name,
    pub enum_type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct QueryGraphqlConfig {
    pub arguments_field_name: Option<ast::Name>,
    pub limit_field_name: Option<ast::Name>,
    pub offset_field_name: Option<ast::Name>,
    pub filter_input_config: Option<FilterInputGraphqlConfig>,
    pub order_by_field_name: Option<ast::Name>,
    pub aggregate_config: Option<AggregateGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]

pub struct FilterInputGraphqlConfig {
    pub where_field_name: ast::Name,
    pub operator_names: FilterInputOperatorNames,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct FilterInputOperatorNames {
    pub and: ast::Name,
    pub or: ast::Name,
    pub not: ast::Name,
    pub is_null: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct AggregateGraphqlConfig {
    pub filter_input_field_name: ast::Name,
    pub count_field_name: ast::Name,
    pub count_distinct_field_name: ast::Name,
}
