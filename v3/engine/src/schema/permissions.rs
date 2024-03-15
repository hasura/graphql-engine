use open_dds::types::FieldName;
use std::collections::HashMap;

use crate::metadata::resolved::model::FilterPermission;
use crate::metadata::resolved::{self, types::ObjectTypeRepresentation};
use crate::schema::types;
use crate::schema::Role;

/// Build namespace annotation for select permissions
pub(crate) fn get_select_permissions_namespace_annotations(
    model: &resolved::model::Model,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    model
        .select_permissions
        .as_ref()
        .map(|permissions| {
            permissions
                .iter()
                .map(|(role, select_permission)| {
                    (
                        role.clone(),
                        Some(types::NamespaceAnnotation::Filter(
                            select_permission.filter.clone(),
                        )),
                    )
                })
                .collect()
        })
        .unwrap_or_default()
}

/// Build namespace annotation for select one permissions.
/// This is different from generating permissions for select_many etc,
/// as we need to check the permissions of the arguments used in the selection.
pub(crate) fn get_select_one_namespace_annotations(
    model: &resolved::model::Model,
    object_type_representation: &ObjectTypeRepresentation,
    select_unique: &resolved::model::SelectUniqueGraphQlDefinition,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let select_permissions = get_select_permissions_namespace_annotations(model);

    select_permissions
        .into_iter()
        .filter(|(role, _)| {
            select_unique.unique_identifier.iter().all(|field| {
                get_allowed_roles_for_field(object_type_representation, field.0)
                    .any(|allowed_role| role == allowed_role)
            })
        })
        .collect()
}

/// Build namespace annotation for model relationship permissions.
/// We need to check the permissions of the source and target fields
/// in the relationship mappings.
pub(crate) fn get_model_relationship_namespace_annotations(
    target_model: &resolved::model::Model,
    source_object_type_representation: &ObjectTypeRepresentation,
    target_object_type_representation: &ObjectTypeRepresentation,
    mappings: &[resolved::relationship::RelationshipModelMapping],
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let select_permissions = get_select_permissions_namespace_annotations(target_model);
    select_permissions
        .into_iter()
        .filter(|(role, _)| {
            mappings.iter().all(|mapping| {
                let source_name = mapping.source_field.field_name.clone();
                let target_name = mapping.target_field.field_name.clone();

                get_allowed_roles_for_field(source_object_type_representation, &source_name)
                    .any(|allowed_role| role == allowed_role)
                    && get_allowed_roles_for_field(target_object_type_representation, &target_name)
                        .any(|allowed_role| role == allowed_role)
            })
        })
        .collect()
}

/// Build namespace annotation for commands
pub(crate) fn get_command_namespace_annotations(
    command: &resolved::command::Command,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, crate::schema::Error> {
    let mut permissions = HashMap::new();
    match &command.permissions {
        Some(command_permissions) => {
            for (role, permission) in command_permissions {
                if permission.allow_execution {
                    permissions.insert(
                        role.clone(),
                        Some(types::NamespaceAnnotation::ArgumentPresets(
                            permission.argument_presets.clone(),
                        )),
                    );
                }
            }
        }
        None => {}
    }
    Ok(permissions)
}

/// Build namespace annotation for command relationship permissions.
/// We need to check the permissions of the source fields
/// in the relationship mappings.
pub(crate) fn get_command_relationship_namespace_annotations(
    command: &resolved::command::Command,
    source_object_type_representation: &ObjectTypeRepresentation,
    mappings: &[resolved::relationship::RelationshipCommandMapping],
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, crate::schema::Error> {
    let select_permissions = get_command_namespace_annotations(command)?;

    Ok(select_permissions
        .into_iter()
        .filter(|(role, _)| {
            mappings.iter().all(|mapping| {
                get_allowed_roles_for_field(
                    source_object_type_representation,
                    &mapping.source_field.field_name,
                )
                .any(|allowed_role| role == allowed_role)
            })
        })
        .collect())
}

/// Build namespace annotations for the node interface..
/// The global ID field and the Node interface will only be exposed
/// for a role if the role has access (select permissions)
/// to all the Global ID fields.
pub(crate) fn get_node_interface_annotations(
    object_type_representation: &ObjectTypeRepresentation,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let mut permissions = HashMap::new();
    for (role, type_output_permission) in &object_type_representation.type_permissions {
        let is_permitted = object_type_representation
            .global_id_fields
            .iter()
            .all(|field_name| type_output_permission.allowed_fields.contains(field_name));
        if is_permitted {
            permissions.insert(role.clone(), None);
        }
    }
    permissions
}

/// Build namespace annotations for each field based on the type permissions
pub(crate) fn get_allowed_roles_for_field<'a>(
    object_type_representation: &'a ObjectTypeRepresentation,
    field_name: &'a FieldName,
) -> impl Iterator<Item = &'a Role> {
    object_type_representation
        .type_permissions
        .iter()
        .filter_map(|(role, type_output_permission)| {
            if type_output_permission.allowed_fields.contains(field_name) {
                Some(role)
            } else {
                None
            }
        })
}

/// Builds namespace annotations for the `node` field.
pub(crate) fn get_node_field_namespace_permissions(
    object_type_representation: &ObjectTypeRepresentation,
    model: &resolved::model::Model,
) -> HashMap<Role, FilterPermission> {
    let mut permissions = HashMap::new();

    match &model.select_permissions {
        // Model doesn't have any select permissions, so no `FilterPermission` can be obtained
        None => {}
        Some(select_permissions) => {
            for (role, type_output_permission) in &object_type_representation.type_permissions {
                let is_global_id_field_accessible = object_type_representation
                    .global_id_fields
                    .iter()
                    .all(|field_name| type_output_permission.allowed_fields.contains(field_name));

                if is_global_id_field_accessible {
                    let select_permission = select_permissions.get(role).map(|s| s.filter.clone());

                    match select_permission {
                        // Select permission doesn't exist for the role, so no `FilterPermission` can
                        // be obtained.
                        None => {}
                        Some(select_permission) => {
                            permissions.insert(role.clone(), select_permission);
                        }
                    }
                };
            }
        }
    }

    permissions
}
