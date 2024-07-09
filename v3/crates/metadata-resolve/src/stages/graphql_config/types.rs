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
pub struct GlobalGraphqlConfig {
    pub query_root_type_name: ast::TypeName,
    pub mutation_root_type_name: ast::TypeName,
    pub order_by_input: Option<OrderByInputGraphqlConfig>,
    pub enable_apollo_federation_fields: bool,
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
