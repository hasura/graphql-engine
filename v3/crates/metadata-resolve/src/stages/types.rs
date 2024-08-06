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
    aggregates, boolean_expressions, command_permissions, graphql_config, model_permissions,
    object_boolean_expressions, order_by_expressions, relationships, scalar_types,
};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub object_types:
        BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    pub scalar_types: BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
    pub commands: IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
    pub object_boolean_expression_types: BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    pub boolean_expression_types: boolean_expressions::BooleanExpressionTypes,
    pub order_by_expressions: order_by_expressions::OrderByExpressions,
    pub aggregate_expressions:
        BTreeMap<Qualified<AggregateExpressionName>, aggregates::AggregateExpression>,
    pub graphql_config: graphql_config::GlobalGraphqlConfig,
    pub roles: Vec<Role>,
}
