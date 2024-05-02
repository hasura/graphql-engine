use std::collections::HashMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::types::subgraph::Qualified;

use crate::stages::{
    boolean_expressions, command_permissions, graphql_config, model_permissions, relationships,
    scalar_types,
};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub object_types:
        HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    pub scalar_types: HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
    pub commands: IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
    pub boolean_expression_types:
        HashMap<Qualified<CustomTypeName>, boolean_expressions::ObjectBooleanExpressionType>,
    pub graphql_config: graphql_config::GlobalGraphqlConfig,
    pub roles: Vec<Role>,
}
