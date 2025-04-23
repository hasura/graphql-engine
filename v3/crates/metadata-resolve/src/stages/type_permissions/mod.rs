use std::collections::BTreeMap;

mod error;
mod types;
pub use error::{
    TypeInputPermissionError, TypeOutputPermissionError, TypePermissionError, TypePermissionIssue,
};
use open_dds::identifier::SubgraphName;
use open_dds::permissions::{FieldPreset, Role, TypeOutputPermission, TypePermissionsV1};
use open_dds::types::CustomTypeName;
pub use types::{
    FieldPresetInfo, ObjectTypeWithPermissions, ObjectTypesWithPermissions, TypeInputPermission,
};

use crate::types::subgraph::Qualified;

use crate::ValueExpression;
use crate::helpers::typecheck;
use crate::stages::object_types;

/// resolve type permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: object_types::ObjectTypesWithTypeMappings,
) -> Result<(ObjectTypesWithPermissions, Vec<TypePermissionIssue>), Vec<TypePermissionError>> {
    let mut issues = Vec::new();
    let object_types_context = object_types
        .0
        .iter()
        .map(|(k, v)| (k, &v.object_type))
        .collect();

    // A temporary map to store the resolved permissions
    let mut type_permissions = BTreeMap::new();

    let mut results = vec![];

    // resolve type permissions
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: output_type_permission,
    } in &metadata_accessor.type_permissions
    {
        results.push(resolve_type_permission(
            output_type_permission,
            subgraph,
            &object_types,
            &object_types_context,
            &metadata_accessor.flags,
            &mut type_permissions,
            &mut issues,
        ));
    }

    // collect all errors or continue
    partition_eithers::collect_any_errors(results).map(|_| {
        // Stitch the permissions back onto the object types
        let object_types_with_permissions = object_types
            .0
            .into_iter()
            .map(|(qualified_type_name, object_type)| {
                let Permissions {
                    output: type_output_permissions,
                    input: type_input_permissions,
                } = type_permissions
                    .remove(&qualified_type_name)
                    .unwrap_or_else(|| Permissions {
                        input: BTreeMap::new(),
                        output: BTreeMap::new(),
                    }); // Assume no permissions if not found in the map
                (
                    qualified_type_name,
                    ObjectTypeWithPermissions {
                        object_type: object_type.object_type,
                        type_mappings: object_type.type_mappings,
                        type_output_permissions,
                        type_input_permissions,
                    },
                )
            })
            .collect();
        (
            ObjectTypesWithPermissions(object_types_with_permissions),
            issues,
        )
    })
}

struct Permissions {
    output: BTreeMap<Role, TypeOutputPermission>,
    input: BTreeMap<Role, TypeInputPermission>,
}

fn resolve_type_permission(
    output_type_permission: &TypePermissionsV1,
    subgraph: &SubgraphName,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    object_types_context: &BTreeMap<
        &Qualified<open_dds::types::CustomTypeName>,
        &object_types::ObjectTypeRepresentation,
    >,
    flags: &open_dds::flags::OpenDdFlags,
    type_permissions: &mut BTreeMap<Qualified<CustomTypeName>, Permissions>,
    issues: &mut Vec<TypePermissionIssue>,
) -> Result<(), TypePermissionError> {
    let qualified_type_name =
        Qualified::new(subgraph.clone(), output_type_permission.type_name.clone());

    match object_types.0.get(&qualified_type_name) {
        None => {
            return Err(TypePermissionError::from(
                TypeOutputPermissionError::UnknownTypeInOutputPermissionsDefinition {
                    type_name: qualified_type_name,
                },
            ));
        }
        Some(object_type) => {
            let type_output_permissions =
                resolve_output_type_permission(&object_type.object_type, output_type_permission)?;

            let type_input_permissions = resolve_input_type_permission(
                flags,
                object_types_context,
                &object_type.object_type,
                output_type_permission,
                issues,
            )?;

            type_permissions.insert(
                qualified_type_name,
                Permissions {
                    output: type_output_permissions,
                    input: type_input_permissions,
                },
            );
        }
    }
    Ok(())
}

pub fn resolve_output_type_permission(
    object_type_representation: &object_types::ObjectTypeRepresentation,
    type_permissions: &TypePermissionsV1,
) -> Result<BTreeMap<Role, TypeOutputPermission>, TypeOutputPermissionError> {
    let mut resolved_type_permissions = BTreeMap::new();

    // validate all the fields definied in output permissions actually
    // exist in this type definition
    for type_permission in &type_permissions.permissions {
        if let Some(output) = &type_permission.output {
            for field_name in &output.allowed_fields {
                if !object_type_representation.fields.contains_key(field_name) {
                    return Err(
                        TypeOutputPermissionError::UnknownFieldInOutputPermissionsDefinition {
                            field_name: field_name.clone(),
                            type_name: type_permissions.type_name.clone(),
                        },
                    );
                }
            }
            if resolved_type_permissions
                .insert(type_permission.role.clone(), output.clone())
                .is_some()
            {
                return Err(TypeOutputPermissionError::DuplicateOutputTypePermissions {
                    type_name: type_permissions.type_name.clone(),
                });
            }
        }
    }
    Ok(resolved_type_permissions)
}

pub(crate) fn resolve_input_type_permission(
    flags: &open_dds::flags::OpenDdFlags,
    object_types: &BTreeMap<
        &Qualified<open_dds::types::CustomTypeName>,
        &object_types::ObjectTypeRepresentation,
    >,
    object_type_representation: &object_types::ObjectTypeRepresentation,
    type_permissions: &TypePermissionsV1,
    issues: &mut Vec<TypePermissionIssue>,
) -> Result<BTreeMap<Role, TypeInputPermission>, TypeInputPermissionError> {
    let mut resolved_type_permissions = BTreeMap::new();

    for type_permission in &type_permissions.permissions {
        if let Some(input) = &type_permission.input {
            let mut resolved_field_presets = BTreeMap::new();
            for FieldPreset {
                field: field_name,
                value,
            } in &input.field_presets
            {
                // check if the field exists on this type
                let field_definition = match object_type_representation.fields.get(field_name) {
                    Some(field_definition) => {
                        // check if the value is provided typechecks
                        let new_issues = typecheck::typecheck_value_expression(
                            object_types,
                            &field_definition.field_type,
                            value,
                        )
                        .map_err(|type_error| {
                            TypeInputPermissionError::FieldPresetTypeError {
                                field_name: field_name.clone(),
                                type_name: type_permissions.type_name.clone(),
                                type_error,
                            }
                        })?;
                        // Convert typecheck issues into type permission issues and collect them
                        for issue in new_issues {
                            issues.push(TypePermissionIssue::FieldPresetTypecheckIssue {
                                field_name: field_name.clone(),
                                type_name: type_permissions.type_name.clone(),
                                typecheck_issue: issue,
                            });
                        }
                        field_definition
                    }
                    None => {
                        return Err(
                            TypeInputPermissionError::UnknownFieldInInputPermissionsDefinition {
                                field_name: field_name.clone(),
                                type_name: type_permissions.type_name.clone(),
                            },
                        );
                    }
                };
                let resolved_value = match &value {
                    open_dds::permissions::ValueExpression::Literal(literal) => {
                        ValueExpression::Literal(literal.clone())
                    }
                    open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
                        ValueExpression::SessionVariable(
                            hasura_authn_core::SessionVariableReference {
                                name: session_variable.clone(),
                                passed_as_json: flags
                                    .contains(open_dds::flags::Flag::JsonSessionVariables),
                                disallow_unknown_fields: flags.contains(
                                    open_dds::flags::Flag::DisallowUnknownValuesInArguments,
                                ),
                            },
                        )
                    }
                };
                resolved_field_presets.insert(
                    field_name.clone(),
                    FieldPresetInfo {
                        value: resolved_value,
                        deprecated: field_definition.deprecated.clone(),
                    },
                );
            }
            if resolved_type_permissions
                .insert(
                    type_permission.role.clone(),
                    TypeInputPermission {
                        field_presets: resolved_field_presets,
                    },
                )
                .is_some()
            {
                return Err(TypeInputPermissionError::DuplicateInputTypePermissions {
                    type_name: type_permissions.type_name.clone(),
                });
            }
        }
    }
    Ok(resolved_type_permissions)
}
