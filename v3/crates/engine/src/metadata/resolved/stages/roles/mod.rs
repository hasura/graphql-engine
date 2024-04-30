use std::collections::HashMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::stages::{command_permissions, models, relationships};

/// Gather all roles from various permission objects.
pub fn resolve(
    object_types: &HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
) -> Vec<Role> {
    let mut roles = Vec::new();
    for object_type in object_types.values() {
        for role in object_type.type_output_permissions.keys() {
            roles.push(role.clone());
        }
        for role in object_type.type_input_permissions.keys() {
            roles.push(role.clone());
        }
    }
    for model in models.values() {
        if let Some(select_permissions) = &model.select_permissions {
            for role in select_permissions.keys() {
                roles.push(role.clone());
            }
        }
    }
    for command in commands.values() {
        for role in command.permissions.keys() {
            roles.push(role.clone());
        }
    }
    roles
}
