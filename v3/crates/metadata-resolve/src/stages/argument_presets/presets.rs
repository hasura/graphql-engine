use super::types::{ArgumentNameAndPath, ArgumentPresets};
use crate::helpers::types::{object_type_exists, unwrap_custom_type_name};
use crate::stages::{
    command_permissions, model_permissions, object_relationships, object_types, type_permissions,
};
use crate::types::{
    permission::ValueExpressionOrPredicate,
    subgraph::{Qualified, QualifiedTypeReference},
};
use hasura_authn_core::Role;
use open_dds::{
    data_connector::DataConnectorColumnName,
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use std::collections::BTreeMap;

#[derive(Debug, thiserror::Error)]
pub enum ArgumentPresetError {
    #[error(
        "Type mapping or field mapping not found for type {type_name:} and field {field_name:}"
    )]
    MappingNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
}

/// Build namespace annotation for select permissions
pub fn get_argument_presets_for_model(
    model: &model_permissions::ModelWithPermissions,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
) -> Result<BTreeMap<Role, ArgumentPresets>, ArgumentPresetError> {
    let mut argument_presets_by_role: BTreeMap<Role, ArgumentPresets> = model
        .select_permissions
        .iter()
        .map(|(role, select_permission)| {
            (
                role.clone(),
                ArgumentPresets {
                    argument_presets: select_permission
                        .argument_presets
                        .iter()
                        .map(|(arg_name, preset)| {
                            (
                                ArgumentNameAndPath {
                                    ndc_argument_name: model
                                        .model
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
            )
        })
        .collect();

    // if any of model argument's input type has field presets defined, add
    // them to model argument preset annotations as well. if there is no
    // source defined for the model, we don't generate these preset
    // annotations.
    if let Some(model_source) = model.model.source.as_ref() {
        let mut role_presets_map = BTreeMap::new();
        for (arg_name, arg_info) in &model.model.arguments {
            // get the NDC argument name of this command source
            let ndc_argument_name = model_source.argument_mappings.get(arg_name).cloned();

            // A list to keep track of the input types we have already processed
            let mut processed_input_types = Vec::new();
            let mut field_path = Vec::new();

            build_annotations_from_input_object_type_permissions(
                &mut field_path,
                &arg_info.argument_type,
                ndc_argument_name.as_ref(),
                object_types,
                &model_source.type_mappings,
                &mut role_presets_map,
                &mut processed_input_types,
            )?;
        }

        // go through the role presets map and extend them into the permissions map
        for (role, preset_map) in role_presets_map {
            if let Some(argument_presets) = argument_presets_by_role.get_mut(&role) {
                *argument_presets = preset_map;
            }
        }
    }

    Ok(argument_presets_by_role)
}

/// Build namespace annotation for commands
pub fn get_argument_presets_for_command(
    command: &command_permissions::CommandWithPermissions,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
) -> Result<BTreeMap<Role, ArgumentPresets>, ArgumentPresetError> {
    let mut argument_presets_by_role = BTreeMap::new();

    // process command permissions, and annotate any command argument presets
    for (role, permission) in &command.permissions {
        if permission.allow_execution {
            let argument_presets = permission
                .argument_presets
                .iter()
                .map(|(argument_name, preset)| {
                    (
                        ArgumentNameAndPath {
                            ndc_argument_name: command
                                .command
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

            argument_presets_by_role.insert(role.clone(), ArgumentPresets { argument_presets });
        }
    }

    // if any of command argument's input type has field presets defined, add
    // them to command argument preset annotations as well. if there is no
    // source defined for the command, we don't generate these preset
    // annotations.
    if let Some(command_source) = command.command.source.as_ref() {
        let mut role_presets_map = BTreeMap::new();
        for (arg_name, arg_info) in &command.command.arguments {
            // get the NDC argument name of this command source
            let ndc_argument_name = command_source.argument_mappings.get(arg_name).cloned();

            // A list to keep track of the input types we have already processed
            let mut processed_input_types = Vec::new();
            let mut field_path = Vec::new();
            build_annotations_from_input_object_type_permissions(
                &mut field_path,
                &arg_info.argument_type,
                ndc_argument_name.as_ref(),
                object_types,
                &command_source.type_mappings,
                &mut role_presets_map,
                &mut processed_input_types,
            )?;
        }
        // go through the role presets map and extend them into the permissions map
        for (role, preset_map) in role_presets_map {
            if let Some(argument_presets) = argument_presets_by_role.get_mut(&role) {
                *argument_presets = preset_map;
            }
        }
    }

    Ok(argument_presets_by_role)
}

fn build_annotations_from_input_object_type_permissions<'a>(
    field_path: &mut [DataConnectorColumnName],
    type_reference: &'a QualifiedTypeReference,
    ndc_argument_name: Option<&DataConnectorArgumentName>,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    role_presets_map: &mut BTreeMap<Role, ArgumentPresets>,
    processed_input_types: &mut Vec<&'a Qualified<CustomTypeName>>,
) -> Result<(), ArgumentPresetError> {
    if let Some(object_type) = unwrap_custom_type_name(type_reference) {
        // If the object type exists and it is not already processed (to avoid infinite recursion)
        if object_type_exists(object_type, object_types).is_ok()
            && !processed_input_types.contains(&object_type)
        {
            if let Some(object_type_repr) = object_types.get(object_type) {
                let field_mappings =
                    type_mappings
                        .get(object_type)
                        .map(|type_mapping| match type_mapping {
                            object_types::TypeMapping::Object {
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
                        object_type,
                    )?;

                    role_presets_map.insert(
                        role.clone(),
                        ArgumentPresets {
                            argument_presets: preset_map,
                        },
                    );
                }

                // Push the input type to the list of processed input types
                processed_input_types.push(object_type);

                // recursively process all the fields of this input object type
                // the processed_input_types_ list is passed to avoid infinite recursion
                for (field_name, field_definition) in &object_type_repr.object_type.fields {
                    let mut field_path_ = field_path.to_owned();
                    let ndc_field = field_mappings
                        .and_then(|mappings| {
                            mappings
                                .get(field_name)
                                .map(|field_mapping| field_mapping.column.clone())
                        })
                        .ok_or_else(|| ArgumentPresetError::MappingNotFound {
                            type_name: object_type.clone(),
                            field_name: field_name.clone(),
                        })?;

                    field_path_.push(ndc_field.clone());
                    build_annotations_from_input_object_type_permissions(
                        &mut field_path_,
                        &field_definition.field_type,
                        ndc_argument_name,
                        object_types,
                        type_mappings,
                        role_presets_map,
                        processed_input_types,
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
/// ```ignore
///    createPerson(person: Person) -> Result<PersonOutput>
///    Person { name: Name, address: Address }
///    Name { first_name: String, last_name: String }
///    Address { street: String, city: String, country: String, zip_code: String }
/// ```
/// Field preset metadata -
///   on type Address -
///     `("user", fieldPreset: {country: {sessionVariable: "x-hasura-user-country"}})`
/// Preset map we generate -
///   `Map<("person", ["address", "country"]), ValueExpression(SessionVariable("x-hasura-user-country"))>`
fn build_preset_map_from_input_object_type_permission(
    permission: &type_permissions::TypeInputPermission,
    field_mappings: Option<&BTreeMap<FieldName, object_types::FieldMapping>>,
    type_reference: &QualifiedTypeReference,
    field_path: &[DataConnectorColumnName],
    ndc_argument_name: Option<&DataConnectorArgumentName>,
    object_type: &Qualified<CustomTypeName>,
) -> Result<
    BTreeMap<ArgumentNameAndPath, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
    ArgumentPresetError,
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
                .ok_or_else(|| ArgumentPresetError::MappingNotFound {
                    type_name: object_type.clone(),
                    field_name: field_name.clone(),
                })?;

            // extend the existing field path with a new field
            let mut new_field_path = field_path.to_owned();
            new_field_path.push(ndc_field);

            let key = ArgumentNameAndPath {
                ndc_argument_name: ndc_argument_name.cloned(),
                field_path: new_field_path,
            };

            let value = (type_reference.clone(), preset.value.clone());

            Ok((key, value))
        })
        .collect::<Result<BTreeMap<_, _>, ArgumentPresetError>>()?;

    Ok(preset_map)
}
