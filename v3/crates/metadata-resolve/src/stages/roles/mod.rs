use std::collections::BTreeMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::types::subgraph::Qualified;

use crate::stages::{command_permissions, model_permissions, relationships};

/// Gather all roles from various permission objects.
pub fn resolve(
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    models: &IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
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
        for role in model.select_permissions.keys() {
            roles.push(role.clone());
        }
    }
    for command in commands.values() {
        for role in command.permissions.keys() {
            roles.push(role.clone());
        }
    }
    roles
}
