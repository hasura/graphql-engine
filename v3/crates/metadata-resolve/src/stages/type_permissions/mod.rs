use std::collections::BTreeMap;

mod error;
mod types;
pub use error::{TypeInputPermissionError, TypeOutputPermissionError, TypePermissionError};
use open_dds::permissions::{FieldPreset, Role, TypeOutputPermission, TypePermissionsV1};
pub use types::{
    FieldPresetInfo, ObjectTypeWithPermissions, ObjectTypesWithPermissions, TypeInputPermission,
};

use crate::types::subgraph::Qualified;

use crate::helpers::typecheck;
use crate::stages::object_types;
use crate::ValueExpressionOrPredicate;

/// resolve type permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: object_types::ObjectTypesWithTypeMappings,
) -> Result<ObjectTypesWithPermissions, TypePermissionError> {
    let mut object_types_with_permissions = BTreeMap::new();
    for (object_type_name, object_type) in object_types.0 {
        object_types_with_permissions.insert(
            object_type_name.clone(),
            ObjectTypeWithPermissions {
                object_type: object_type.object_type,
                type_mappings: object_type.type_mappings,
                type_input_permissions: BTreeMap::new(),
                type_output_permissions: BTreeMap::new(),
            },
        );
    }

    // resolve type permissions
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: output_type_permission,
    } in &metadata_accessor.type_permissions
    {
        let qualified_type_name =
            Qualified::new(subgraph.clone(), output_type_permission.type_name.clone());
        match object_types_with_permissions.get_mut(&qualified_type_name) {
            None => {
                return Err(TypePermissionError::from(
                    TypeOutputPermissionError::UnknownTypeInOutputPermissionsDefinition {
                        type_name: qualified_type_name,
                    },
                ))
            }
            Some(object_type) => {
                object_type.type_output_permissions = resolve_output_type_permission(
                    &object_type.object_type,
                    output_type_permission,
                )?;
                object_type.type_input_permissions = resolve_input_type_permission(
                    &metadata_accessor.flags,
                    &object_type.object_type,
                    output_type_permission,
                )?;
            }
        }
    }
    Ok(ObjectTypesWithPermissions(object_types_with_permissions))
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
    flags: &open_dds::flags::Flags,
    object_type_representation: &object_types::ObjectTypeRepresentation,
    type_permissions: &TypePermissionsV1,
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
                        typecheck::typecheck_value_expression(&field_definition.field_type, value)
                            .map_err(|type_error| {
                                TypeInputPermissionError::FieldPresetTypeError {
                                    field_name: field_name.clone(),
                                    type_name: type_permissions.type_name.clone(),
                                    type_error,
                                }
                            })?;
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
                        ValueExpressionOrPredicate::Literal(literal.clone())
                    }
                    open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
                        ValueExpressionOrPredicate::SessionVariable(
                            hasura_authn_core::SessionVariableReference {
                                name: session_variable.clone(),
                                passed_as_json: flags.json_session_variables,
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
