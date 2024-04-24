use open_dds::types::{CustomTypeName, FieldName};
use std::collections::{BTreeMap, HashMap};

use crate::metadata::resolved::model::FilterPermission;
use crate::metadata::resolved::permission::resolve_value_expression;
use crate::metadata::resolved::stages::type_permissions;
use crate::metadata::resolved::subgraph::{Qualified, QualifiedTypeReference};
use crate::metadata::resolved::types::{
    get_underlying_object_type, unwrap_custom_type_name, FieldMapping, TypeMapping,
};
use crate::metadata::resolved::{self};
use crate::schema::Role;
use crate::schema::{self, types};

use super::types::ArgumentNameAndPath;

/// Build namespace annotation for select permissions
pub(crate) fn get_select_permissions_namespace_annotations(
    model: &resolved::model::Model,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, schema::Error> {
    let mut permissions: HashMap<Role, Option<types::NamespaceAnnotation>> = model
        .select_permissions
        .as_ref()
        .map(|permissions| {
            permissions
                .iter()
                .map(|(role, select_permission)| {
                    (
                        role.clone(),
                        Some(types::NamespaceAnnotation::Model {
                            filter: select_permission.filter.clone(),
                            argument_presets: types::ArgumentPresets {
                                argument_presets: select_permission
                                    .argument_presets
                                    .iter()
                                    .map(|(arg_name, preset)| {
                                        (
                                            ArgumentNameAndPath {
                                                ndc_argument_name: model
                                                    .source
                                                    .as_ref()
                                                    .and_then(|model_source| {
                                                        model_source.argument_mappings.get(arg_name)
                                                    })
                                                    .cloned(),
                                                field_path: vec![],
                                            },
                                            preset.clone(),
                                        )
                                    })
                                    .collect(),
                            },
                        }),
                    )
                })
                .collect()
        })
        .unwrap_or_default();

    // if any of model argument's input type has field presets defined, add
    // them to model argument preset annotations as well. if there is no
    // source defined for the model, we don't generate these preset
    // annotations.
    if let Some(model_source) = model.source.as_ref() {
        let mut role_presets_map = HashMap::new();
        for (arg_name, arg_info) in &model.arguments {
            // get the NDC argument name of this command source
            let ndc_argument_name = model_source.argument_mappings.get(arg_name).cloned();

            let mut field_path = Vec::new();

            build_annotations_from_input_object_type_permissions(
                &mut field_path,
                &arg_info.argument_type,
                &ndc_argument_name,
                object_types,
                &model_source.type_mappings,
                &mut role_presets_map,
            )?;
        }

        // go through the role presets map and extend them into the permissions map
        for (role, preset_map) in role_presets_map {
            if let Some(Some(types::NamespaceAnnotation::Model {
                filter: _,
                argument_presets,
            })) = permissions.get_mut(&role)
            {
                *argument_presets = preset_map;
            }
        }
    }

    Ok(permissions)
}

/// Build namespace annotation for select one permissions.
/// This is different from generating permissions for select_many etc,
/// as we need to check the permissions of the arguments used in the selection.
pub(crate) fn get_select_one_namespace_annotations(
    model: &resolved::model::Model,
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    select_unique: &resolved::model::SelectUniqueGraphQlDefinition,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, schema::Error> {
    let select_permissions = get_select_permissions_namespace_annotations(model, object_types)?;

    let permissions = select_permissions
        .into_iter()
        .filter(|(role, _)| {
            select_unique.unique_identifier.iter().all(|field| {
                get_allowed_roles_for_field(object_type_representation, field.0)
                    .any(|allowed_role| role == allowed_role)
            })
        })
        .collect();
    Ok(permissions)
}

/// Build namespace annotation for model relationship permissions.
/// We need to check the permissions of the source and target fields
/// in the relationship mappings.
pub(crate) fn get_model_relationship_namespace_annotations(
    target_model: &resolved::model::Model,
    source_object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    target_object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    mappings: &[resolved::relationship::RelationshipModelMapping],
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, schema::Error> {
    let select_permissions =
        get_select_permissions_namespace_annotations(target_model, object_types)?;
    let permissions = select_permissions
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
        .collect();
    Ok(permissions)
}

/// Build namespace annotation for commands
pub(crate) fn get_command_namespace_annotations(
    command: &resolved::command::Command,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, crate::schema::Error> {
    let mut permissions = HashMap::new();

    // process command permissions, and annotate any command argument presets
    match &command.permissions {
        Some(command_permissions) => {
            for (role, permission) in command_permissions {
                if permission.allow_execution {
                    let argument_presets = permission
                        .argument_presets
                        .iter()
                        .map(|(argument_name, preset)| {
                            (
                                ArgumentNameAndPath {
                                    ndc_argument_name: command
                                        .source
                                        .as_ref()
                                        .and_then(|command_source| {
                                            command_source.argument_mappings.get(argument_name)
                                        })
                                        .cloned(),
                                    field_path: vec![],
                                },
                                preset.clone(),
                            )
                        })
                        .collect();
                    permissions.insert(
                        role.clone(),
                        Some(types::NamespaceAnnotation::Command(
                            types::ArgumentPresets { argument_presets },
                        )),
                    );
                }
            }
        }
        None => {}
    }

    // if any of command argument's input type has field presets defined, add
    // them to command argument preset annotations as well. if there is no
    // source defined for the command, we don't generate these preset
    // annotations.
    if let Some(command_source) = command.source.as_ref() {
        let mut role_presets_map = HashMap::new();
        for (arg_name, arg_info) in &command.arguments {
            // get the NDC argument name of this command source
            let ndc_argument_name = command_source.argument_mappings.get(arg_name).cloned();

            let mut field_path = Vec::new();
            build_annotations_from_input_object_type_permissions(
                &mut field_path,
                &arg_info.argument_type,
                &ndc_argument_name,
                object_types,
                &command_source.type_mappings,
                &mut role_presets_map,
            )?;
        }
        // go through the role presets map and extend them into the permissions map
        for (role, preset_map) in role_presets_map {
            if let Some(Some(types::NamespaceAnnotation::Command(argument_presets))) =
                permissions.get_mut(&role)
            {
                *argument_presets = preset_map;
            }
        }
    }

    Ok(permissions)
}

fn build_annotations_from_input_object_type_permissions(
    field_path: &mut [String],
    type_reference: &QualifiedTypeReference,
    ndc_argument_name: &Option<String>,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    role_presets_map: &mut HashMap<Role, types::ArgumentPresets>,
) -> Result<(), schema::Error> {
    if let Some(custom_typename) = unwrap_custom_type_name(type_reference) {
        if let Ok(object_type) = get_underlying_object_type(custom_typename, object_types) {
            if let Some(object_type_repr) = object_types.get(&object_type) {
                let field_mappings =
                    type_mappings
                        .get(&object_type)
                        .map(|type_mapping| match type_mapping {
                            TypeMapping::Object {
                                ndc_object_type_name: _,
                                field_mappings,
                            } => field_mappings,
                        });

                for (role, permission) in &object_type_repr.type_input_permissions {
                    let preset_map = build_preset_map_from_input_object_type_permission(
                        permission,
                        field_mappings,
                        type_reference,
                        field_path,
                        ndc_argument_name,
                        &object_type,
                    )?;

                    role_presets_map.insert(
                        role.clone(),
                        types::ArgumentPresets {
                            argument_presets: preset_map,
                        },
                    );
                }

                // recursively process all the fields of this input object type
                for (field_name, field_definition) in &object_type_repr.object_type.fields {
                    let mut field_path_ = field_path.to_owned();
                    field_path_.push(field_name.to_string());
                    build_annotations_from_input_object_type_permissions(
                        &mut field_path_,
                        &field_definition.field_type,
                        ndc_argument_name,
                        object_types,
                        type_mappings,
                        role_presets_map,
                    )?;
                }
            }
        }
    }
    Ok(())
}

/// Given one input permission and other necessary info, build the arguments presets map
///
/// Example of a preset annotation -
/// Command and types -
///    createPerson(person: Person) -> Result<PersonOutput>
///    Person { name: Name, address: Address }
///    Name { first_name: String, last_name: String }
///    Address { street: String, city: String, country: String, zip_code: String }
/// Field preset metadata -
///   on type Address -
///     ("user", fieldPreset: {country: {sessionVariable: "x-hasura-user-country"}})
/// Preset map we generate -
///   `Map<("person", ["address", "country"]), ValueExpression(SessionVariable("x-hasura-user-country"))>`
fn build_preset_map_from_input_object_type_permission(
    permission: &type_permissions::TypeInputPermission,
    field_mappings: Option<&BTreeMap<FieldName, FieldMapping>>,
    type_reference: &QualifiedTypeReference,
    field_path: &[String],
    ndc_argument_name: &Option<String>,
    object_type: &Qualified<CustomTypeName>,
) -> Result<
    BTreeMap<
        ArgumentNameAndPath,
        (
            QualifiedTypeReference,
            resolved::permission::ValueExpression,
        ),
    >,
    schema::Error,
> {
    let preset_map = permission
        .field_presets
        .iter()
        .map(|(field_name, preset)| {
            let ndc_field = field_mappings
                .and_then(|mappings| {
                    mappings
                        .get(field_name)
                        .map(|field_mapping| field_mapping.column.clone())
                })
                .ok_or_else(|| schema::Error::InternalMappingNotFound {
                    type_name: object_type.clone(),
                    field_name: field_name.clone(),
                })?;

            // extend the existing field path with a new field
            let mut new_field_path = field_path.to_owned();
            new_field_path.push(ndc_field);

            let key = ArgumentNameAndPath {
                ndc_argument_name: ndc_argument_name.clone(),
                field_path: new_field_path,
            };
            let value = (
                type_reference.clone(),
                resolve_value_expression(preset.clone()),
            );

            Ok((key, value))
        })
        .collect::<Result<BTreeMap<_, _>, schema::Error>>()?;

    Ok(preset_map)
}

/// Build namespace annotation for command relationship permissions.
/// We need to check the permissions of the source fields
/// in the relationship mappings.
pub(crate) fn get_command_relationship_namespace_annotations(
    command: &resolved::command::Command,
    source_object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    mappings: &[resolved::relationship::RelationshipCommandMapping],
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, crate::schema::Error> {
    let select_permissions = get_command_namespace_annotations(command, object_types)?;

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
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
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
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
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

/// Build namespace annotations for each field based on the type permissions
pub(crate) fn get_allowed_roles_for_field<'a>(
    object_type_representation: &'a type_permissions::ObjectTypeWithPermissions,
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
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    model: &resolved::model::Model,
) -> HashMap<Role, FilterPermission> {
    let mut permissions = HashMap::new();

    match &model.select_permissions {
        // Model doesn't have any select permissions, so no `FilterPermission` can be obtained
        None => {}
        Some(select_permissions) => {
            for (role, type_output_permission) in
                &object_type_representation.type_output_permissions
            {
                let is_global_id_field_accessible = object_type_representation
                    .object_type
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

/// Builds namespace annotations for the `_entities` field.
pub(crate) fn get_entities_field_namespace_permissions(
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    model: &resolved::model::Model,
) -> HashMap<Role, FilterPermission> {
    let mut permissions = HashMap::new();

    match &model.select_permissions {
        // Model doesn't have any select permissions, so no `FilterPermission` can be obtained
        None => {}
        Some(select_permissions) => {
            for (role, type_output_permission) in
                &object_type_representation.type_output_permissions
            {
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
                            select_permissions.get(role).map(|s| s.filter.clone());

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
    }

    permissions
}
