use serde_with::serde_as;
use std::collections::{BTreeMap, BTreeSet};

use hasura_authn_core::Role;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use open_dds::{
    aggregates::AggregateExpressionName, commands::CommandName, models::ModelName,
    types::CustomTypeName,
};

use crate::flags::RuntimeFlags;
use crate::types::condition::Conditions;
use crate::types::subgraph::Qualified;

use crate::stages::{
    aggregates, boolean_expressions, command_permissions, graphql_config, model_permissions,
    object_relationships, order_by_expressions, scalar_type_representations, views,
};

use super::plugins::LifecyclePluginConfigs;

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    #[serde_as(as = "Vec<(_, _)>")]
    pub object_types:
        BTreeMap<Qualified<CustomTypeName>, object_relationships::ObjectTypeWithRelationships>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub scalar_types:
        BTreeMap<Qualified<CustomTypeName>, scalar_type_representations::ScalarTypeRepresentation>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub models: IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub commands: IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
    pub boolean_expression_types: boolean_expressions::BooleanExpressionTypes,
    pub order_by_expressions: order_by_expressions::OrderByExpressions,
    #[serde_as(as = "Vec<(_, _)>")]
    pub aggregate_expressions:
        BTreeMap<Qualified<AggregateExpressionName>, aggregates::AggregateExpression>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub views: IndexMap<Qualified<open_dds::views::ViewName>, views::ResolvedView>,
    pub graphql_config: graphql_config::GlobalGraphqlConfig,
    pub plugin_configs: LifecyclePluginConfigs,
    pub roles: BTreeSet<Role>,
    pub conditions: Conditions,
    pub runtime_flags: RuntimeFlags,
}
