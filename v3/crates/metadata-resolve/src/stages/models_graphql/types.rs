use std::collections::BTreeMap;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use lang_graphql::ast::common::{self as ast};
use open_dds::{
    aggregates::AggregateExpressionName,
    data_connector::{DataConnectorColumnName, DataConnectorName},
    models::ModelName,
    types::{Deprecated, FieldName},
};

use crate::helpers::types::NdcColumnForComparison;
use crate::stages::{boolean_expressions, models, object_boolean_expressions};
use crate::types::subgraph::{Qualified, QualifiedTypeReference};

/// A Model, once we have added filter expression and graphql for it
pub(crate) struct ModelWithGraphql {
    pub inner: models::Model,
    pub filter_expression_type: Option<ModelExpressionType>,
    pub graphql_api: ModelGraphQlApi,
}

pub(crate) type ModelsWithGraphql = IndexMap<Qualified<ModelName>, ModelWithGraphql>;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum ModelExpressionType {
    ObjectBooleanExpressionType(object_boolean_expressions::ObjectBooleanExpressionType),
    BooleanExpressionType(boolean_expressions::ResolvedObjectBooleanExpressionType),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct UniqueIdentifierField {
    pub field_type: QualifiedTypeReference,
    pub ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectUniqueGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub unique_identifier: IndexMap<FieldName, UniqueIdentifierField>,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectManyGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectAggregateGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
    pub aggregate_expression_name: Qualified<AggregateExpressionName>,
    pub filter_input_field_name: ast::Name,
}

// TODO: add support for aggregates
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByExpressionInfo {
    pub ndc_column: DataConnectorColumnName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelOrderByExpression {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub order_by_type_name: ast::TypeName,
    pub order_by_fields: BTreeMap<FieldName, OrderByExpressionInfo>,
    pub order_by_field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelGraphqlApiArgumentsConfig {
    pub field_name: ast::Name,
    pub type_name: ast::TypeName,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct LimitFieldGraphqlConfig {
    pub field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OffsetFieldGraphqlConfig {
    pub field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub struct ModelGraphQlApi {
    pub arguments_input_config: Option<ModelGraphqlApiArgumentsConfig>,
    pub select_uniques: Vec<SelectUniqueGraphQlDefinition>,
    pub select_many: Option<SelectManyGraphQlDefinition>,
    pub select_aggregate: Option<SelectAggregateGraphQlDefinition>,
    pub order_by_expression: Option<ModelOrderByExpression>,
    pub limit_field: Option<LimitFieldGraphqlConfig>,
    pub offset_field: Option<OffsetFieldGraphqlConfig>,
    pub filter_input_type_name: Option<ast::TypeName>,
}
