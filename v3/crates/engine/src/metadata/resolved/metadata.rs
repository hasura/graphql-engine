use std::collections::HashMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::stages::{
    boolean_expressions, command_permissions, graphql_config, model_permissions, relationships,
    roles, scalar_types,
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

/*******************
    Functions to validate and resolve OpenDD spec to internal metadata
*******************/
pub fn resolve_metadata(
    graphql_config: &graphql_config::GraphqlConfig,
    object_types: HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    models: IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
    commands: IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
) -> Result<Metadata, Error> {
    let roles = roles::resolve(&object_types, &models, &commands);

    Ok(Metadata {
        scalar_types: scalar_types.clone(),
        object_types,
        models,
        commands,
        boolean_expression_types: boolean_expression_types.clone(),
        graphql_config: graphql_config.global.clone(),
        roles,
    })
}
