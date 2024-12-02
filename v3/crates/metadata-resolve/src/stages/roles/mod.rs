use std::collections::{BTreeMap, BTreeSet};

use hasura_authn_core::Role;
use indexmap::IndexMap;

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::types::subgraph::Qualified;

use crate::stages::{command_permissions, model_permissions, object_relationships};

/// Gather all roles from various permission objects.
pub fn resolve(
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    models: &IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
    commands: &IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
) -> BTreeSet<Role> {
    let mut roles = BTreeSet::new();
    for object_type in object_types.values() {
        for role in object_type.type_output_permissions.keys() {
            roles.insert(role.clone());
        }
        for role in object_type.type_input_permissions.keys() {
            roles.insert(role.clone());
        }
    }
    for model in models.values() {
        for role in model.select_permissions.keys() {
            roles.insert(role.clone());
        }
    }
    for command in commands.values() {
        for role in command.permissions.keys() {
            roles.insert(role.clone());
        }
    }
    roles
}
