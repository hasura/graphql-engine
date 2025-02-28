use indexmap::IndexMap;
use open_dds::types::FieldName;
use std::collections::HashMap;

use crate::types;
use crate::Role;
use metadata_resolve::{self};

/// Build namespace annotation for select permissions
pub(crate) fn get_select_permissions_namespace_annotations(
    model: &metadata_resolve::ModelWithPermissions,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let mut namespace_annotations = HashMap::new();

    for (role, select_permission) in &model.select_permissions {
        namespace_annotations.insert(
            role.clone(),
            Some(types::NamespaceAnnotation::Model {
                filter: select_permission.filter.clone(),
                argument_presets: select_permission.argument_presets.clone(),
                allow_subscriptions: select_permission.allow_subscriptions,
            }),
        );
    }

    namespace_annotations
}

/// Build namespace annotation for select one permissions.
/// This is different from generating permissions for select_many etc,
/// as we need to check the permissions of the arguments used in the selection.
pub(crate) fn get_select_one_namespace_annotations(
    model: &metadata_resolve::ModelWithPermissions,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    unique_identifier: &IndexMap<FieldName, metadata_resolve::UniqueIdentifierField>,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let select_permissions = get_select_permissions_namespace_annotations(model);

    let permissions = select_permissions
        .into_iter()
        .filter(|(role, _)| {
            unique_identifier.iter().all(|field| {
                get_allowed_roles_for_field(object_type_representation, field.0)
                    .any(|allowed_role| role == allowed_role)
            })
        })
        .collect();
    permissions
}

/// Build namespace annotation for model relationship permissions.
/// We need to check the permissions of the source and target fields
/// in the relationship mappings.
pub(crate) fn get_model_relationship_namespace_annotations(
    target_model: &metadata_resolve::ModelWithPermissions,
    source_object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    target_object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    mappings: &[metadata_resolve::RelationshipModelMapping],
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let select_permissions = get_select_permissions_namespace_annotations(target_model);
    let permissions = select_permissions
        .into_iter()
        .filter(|(role, _)| {
            mappings.iter().all(|mapping| {
                let source_name = &mapping.source_field.field_name;

                let has_access_to_source_field =
                    get_allowed_roles_for_field(source_object_type_representation, source_name)
                        .any(|allowed_role| role == allowed_role);

                let has_access_to_target = match &mapping.target {
                    metadata_resolve::RelationshipModelMappingTarget::ModelField(
                        metadata_resolve::RelationshipModelMappingFieldTarget {
                            target_field, ..
                        },
                    ) => get_allowed_roles_for_field(
                        target_object_type_representation,
                        &target_field.field_name,
                    )
                    .any(|allowed_role| role == allowed_role),
                    metadata_resolve::RelationshipModelMappingTarget::Argument(_) => true,
                };

                has_access_to_source_field && has_access_to_target
            })
        })
        .collect();
    permissions
}

/// Build namespace annotation for commands
pub(crate) fn get_command_namespace_annotations(
    command: &metadata_resolve::CommandWithPermissions,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let mut permissions = HashMap::new();

    // process command permissions, and annotate any command argument presets
    for (role, permission) in &command.permissions {
        if permission.allow_execution {
            permissions.insert(
                role.clone(),
                Some(types::NamespaceAnnotation::Command(
                    permission.argument_presets.clone(),
                )),
            );
        }
    }

    permissions
}

/// Build namespace annotation for command relationship permissions.
/// We need to check the permissions of the source fields
/// in the relationship mappings.
pub(crate) fn get_command_relationship_namespace_annotations(
    command: &metadata_resolve::CommandWithPermissions,
    source_object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    mappings: &[metadata_resolve::RelationshipCommandMapping],
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let select_permissions = get_command_namespace_annotations(command);

    select_permissions
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
        .collect()
}

/// Build namespace annotations for the node interface..
/// The global ID field and the Node interface will only be exposed
/// for a role if the role has access (select permissions)
/// to all the Global ID fields.
pub(crate) fn get_node_interface_annotations(
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let mut permissions = HashMap::new();
    for (role, type_output_permission) in &object_type_representation.type_output_permissions {
        let is_permitted = object_type_representation
            .object_type
            .global_id_fields
            .iter()
            .all(|field_name| type_output_permission.allowed_fields.contains(field_name));
        if is_permitted {
            permissions.insert(role.clone(), None);
        }
    }
    permissions
}

/// Build namespace annotations for the _Entity union.
/// The key fields and the _Entity union will only be exposed
/// for a role if the role has access (select permissions)
/// to all the key fields.
pub(crate) fn get_entity_union_permissions(
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    let mut permissions = HashMap::new();
    for (role, type_output_permission) in &object_type_representation.type_output_permissions {
        let is_permitted = object_type_representation
            .object_type
            .global_id_fields
            .iter()
            .all(|field_name| type_output_permission.allowed_fields.contains(field_name));
        if is_permitted {
            permissions.insert(role.clone(), None);
        }
    }
    permissions
}

/// Are we allowed to access a given type at all?
/// If we are allowed to access at least one field, yes
pub(crate) fn get_allowed_roles_for_type(
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
) -> impl Iterator<Item = &'_ Role> {
    object_type_representation
        .object_type
        .fields
        .keys()
        .flat_map(|field_name| get_allowed_roles_for_field(object_type_representation, field_name))
}

/// Build namespace annotations for each field based on the type permissions
pub(crate) fn get_allowed_roles_for_field<'a>(
    object_type_representation: &'a metadata_resolve::ObjectTypeWithRelationships,
    field_name: &'a FieldName,
) -> impl Iterator<Item = &'a Role> {
    object_type_representation
        .type_output_permissions
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
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    model: &metadata_resolve::ModelWithPermissions,
) -> HashMap<Role, metadata_resolve::FilterPermission> {
    let mut permissions = HashMap::new();

    for (role, type_output_permission) in &object_type_representation.type_output_permissions {
        let is_global_id_field_accessible = object_type_representation
            .object_type
            .global_id_fields
            .iter()
            .all(|field_name| type_output_permission.allowed_fields.contains(field_name));

        if is_global_id_field_accessible {
            let select_permission = model.select_permissions.get(role).map(|s| s.filter.clone());

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

    permissions
}

/// Builds namespace annotations for the `_entities` field.
pub(crate) fn get_entities_field_namespace_permissions(
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    model: &metadata_resolve::ModelWithPermissions,
) -> HashMap<Role, metadata_resolve::FilterPermission> {
    let mut permissions = HashMap::new();

    for (role, type_output_permission) in &object_type_representation.type_output_permissions {
        if let Some(apollo_federation_config) = &object_type_representation
            .object_type
            .apollo_federation_config
        {
            let is_all_keys_field_accessible =
                apollo_federation_config.keys.iter().all(|key_fields| {
                    key_fields.fields.iter().all(|field_name| {
                        type_output_permission.allowed_fields.contains(field_name)
                    })
                });

            if is_all_keys_field_accessible {
                let select_permission =
                    model.select_permissions.get(role).map(|s| s.filter.clone());

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

    permissions
}
