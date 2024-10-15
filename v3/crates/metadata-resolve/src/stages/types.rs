use std::collections::BTreeMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use open_dds::{
    aggregates::AggregateExpressionName, commands::CommandName, models::ModelName,
    types::CustomTypeName,
};

use crate::types::subgraph::Qualified;

use crate::stages::{
    aggregates, argument_presets, boolean_expressions, graphql_config, object_boolean_expressions,
    object_relationships, order_by_expressions, scalar_types,
};

use super::plugins::LifecyclePluginConfigs;

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub object_types:
        BTreeMap<Qualified<CustomTypeName>, object_relationships::ObjectTypeWithRelationships>,
    pub scalar_types: BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, argument_presets::ModelWithArgumentPresets>,
    pub commands: IndexMap<Qualified<CommandName>, argument_presets::CommandWithArgumentPresets>,
    pub object_boolean_expression_types: BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    pub boolean_expression_types: boolean_expressions::BooleanExpressionTypes,
    pub order_by_expressions: order_by_expressions::OrderByExpressions,
    pub aggregate_expressions:
        BTreeMap<Qualified<AggregateExpressionName>, aggregates::AggregateExpression>,
    pub graphql_config: graphql_config::GlobalGraphqlConfig,
    pub plugin_configs: LifecyclePluginConfigs,
    pub roles: Vec<Role>,
}
