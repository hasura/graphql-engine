use std::collections::HashMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::stages::{commands, models, type_permissions};

/// Gather all roles from various permission objects.
pub fn resolve(
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
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
        if let Some(command_permissions) = &command.permissions {
            for role in command_permissions.keys() {
                roles.push(role.clone());
            }
        }
    }
    roles
}
