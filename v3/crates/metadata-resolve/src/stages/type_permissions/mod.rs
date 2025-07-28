use std::collections::{BTreeMap, BTreeSet};

mod error;
mod types;
use crate::types::condition::{BinaryOperation, Condition, Conditions};
use crate::types::subgraph::Qualified;
pub use error::{
    TypeInputPermissionError, TypeOutputPermissionError, TypePermissionError, TypePermissionIssue,
};
use hasura_authn_core::SESSION_VARIABLE_ROLE;
use indexmap::IndexSet;
use open_dds::identifier::SubgraphName;
use open_dds::permissions::{FieldPreset, Role, TypePermissionOperand, TypePermissionsV2};
use open_dds::session_variables::SessionVariableReference;
use open_dds::types::{CustomTypeName, FieldName};
pub use types::{
    FieldAuthorizationRule, FieldPresetInfo, ObjectTypeWithPermissions, ObjectTypesWithPermissions,
    TypeInputAuthorizationRule, TypeInputPermission, TypeInputPermissions, TypeOutputPermissions,
};

use crate::ValueExpression;
use crate::helpers::typecheck;
use crate::stages::object_types;

fn get_boolean_expression_type_names(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> BTreeSet<Qualified<CustomTypeName>> {
    let mut boolean_expression_types: BTreeSet<Qualified<CustomTypeName>> = BTreeSet::new();

    for qualified_object in &metadata_accessor.boolean_expression_types {
        boolean_expression_types.insert(Qualified::new(
            qualified_object.subgraph.clone(),
            qualified_object.object.name.clone(),
        ));
    }

    for qualified_object in &metadata_accessor.object_boolean_expression_types {
        boolean_expression_types.insert(Qualified::new(
            qualified_object.subgraph.clone(),
            qualified_object.object.name.clone(),
        ));
    }

    boolean_expression_types
}

/// resolve type permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: object_types::ObjectTypesWithTypeMappings,
    conditions: &mut Conditions,
) -> Result<(ObjectTypesWithPermissions, Vec<TypePermissionIssue>), Vec<TypePermissionError>> {
    let mut issues = Vec::new();
    let object_types_context = object_types
        .0
        .iter()
        .map(|(k, v)| (k, &v.object_type))
        .collect();

    // A temporary map to store the resolved permissions
    let mut type_permissions = BTreeMap::new();

    let boolean_expression_type_names = get_boolean_expression_type_names(metadata_accessor);

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
            &boolean_expression_type_names.iter().collect(),
            &object_types_context,
            &metadata_accessor.flags,
            &mut type_permissions,
            conditions,
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
                        input: TypeInputPermissions {
                            by_role: BTreeMap::new(),
                            authorization_rules: vec![],
                        },
                        output: TypeOutputPermissions {
                            authorization_rules: vec![],
                            by_role: BTreeMap::new(),
                        },
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
    output: TypeOutputPermissions,
    input: TypeInputPermissions,
}

fn resolve_type_permission(
    output_type_permission: &TypePermissionsV2,
    subgraph: &SubgraphName,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    boolean_expression_type_names: &BTreeSet<&Qualified<CustomTypeName>>,
    object_types_context: &BTreeMap<
        &Qualified<open_dds::types::CustomTypeName>,
        &object_types::ObjectTypeRepresentation,
    >,
    flags: &open_dds::flags::OpenDdFlags,
    type_permissions: &mut BTreeMap<Qualified<CustomTypeName>, Permissions>,
    conditions: &mut Conditions,
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
            let type_output_permissions = resolve_output_type_permission(
                &object_type.object_type,
                output_type_permission,
                flags,
                conditions,
            )?;

            let type_input_permissions = resolve_input_type_permission(
                flags,
                object_types_context,
                boolean_expression_type_names,
                &object_type.object_type,
                output_type_permission,
                conditions,
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
    type_permissions: &TypePermissionsV2,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> Result<TypeOutputPermissions, TypeOutputPermissionError> {
    let mut authorization_rules = Vec::new();
    let mut by_role = BTreeMap::new();

    match &type_permissions.permissions {
        TypePermissionOperand::RoleBased(role_based_type_permissions) => {
            // validate all the fields definied in output permissions actually
            // exist in this type definition
            for role_based_type_permission in role_based_type_permissions {
                if let Some(output) = &role_based_type_permission.output {
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

                    let authorization_rule = authorization_rule_for_role(
                        &role_based_type_permission.role,
                        &output.allowed_fields,
                        flags,
                        conditions,
                    );

                    if by_role
                        .insert(role_based_type_permission.role.clone(), output.clone())
                        .is_some()
                    {
                        return Err(TypeOutputPermissionError::DuplicateOutputTypePermissions {
                            type_name: type_permissions.type_name.clone(),
                        });
                    }

                    authorization_rules.push(authorization_rule);
                }
            }
            Ok(TypeOutputPermissions {
                authorization_rules,
                by_role,
            })
        }
    }
}

// given a role and some fields, return a FieldAuthorizationRule
// that allows those exact fields given `x-hasura-role` session variable matches the role
fn authorization_rule_for_role(
    role: &Role,
    allowed_fields: &IndexSet<FieldName>,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> FieldAuthorizationRule {
    let condition = Condition::BinaryOperation {
        op: BinaryOperation::Equals,
        left: ValueExpression::SessionVariable(SessionVariableReference {
            name: SESSION_VARIABLE_ROLE,
            passed_as_json: flags.contains(open_dds::flags::Flag::JsonSessionVariables),
            disallow_unknown_fields: flags
                .contains(open_dds::flags::Flag::DisallowUnknownValuesInArguments),
        }),
        right: ValueExpression::Literal(serde_json::Value::String(role.0.clone())),
    };

    let hash = conditions.add(condition);

    FieldAuthorizationRule::AllowFields {
        fields: allowed_fields.iter().cloned().collect(),
        condition: hash,
    }
}

pub(crate) fn resolve_input_type_permission(
    flags: &open_dds::flags::OpenDdFlags,
    object_types: &BTreeMap<
        &Qualified<open_dds::types::CustomTypeName>,
        &object_types::ObjectTypeRepresentation,
    >,
    boolean_expression_type_names: &BTreeSet<&Qualified<CustomTypeName>>,
    object_type_representation: &object_types::ObjectTypeRepresentation,
    type_permissions: &TypePermissionsV2,
    conditions: &mut Conditions,
    issues: &mut Vec<TypePermissionIssue>,
) -> Result<TypeInputPermissions, TypeInputPermissionError> {
    match &type_permissions.permissions {
        TypePermissionOperand::RoleBased(role_based_type_permissions) => {
            let mut by_role = BTreeMap::new();
            let mut authorization_rules = Vec::new();

            for role_based_type_permission in role_based_type_permissions {
                if let Some(input) = &role_based_type_permission.input {
                    let mut resolved_field_presets = BTreeMap::new();
                    for FieldPreset {
                        field: field_name,
                        value,
                    } in &input.field_presets
                    {
                        // check if the field exists on this type
                        let field_definition = match object_type_representation
                            .fields
                            .get(field_name)
                        {
                            Some(field_definition) => {
                                // check if the value is provided typechecks
                                let new_issues = typecheck::typecheck_value_expression(
                                    object_types,
                                    boolean_expression_type_names,
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
                            open_dds::permissions::ValueExpression::SessionVariable(
                                session_variable,
                            ) => ValueExpression::SessionVariable(
                                hasura_authn_core::SessionVariableReference {
                                    name: session_variable.clone(),
                                    passed_as_json: flags
                                        .contains(open_dds::flags::Flag::JsonSessionVariables),
                                    disallow_unknown_fields: flags.contains(
                                        open_dds::flags::Flag::DisallowUnknownValuesInArguments,
                                    ),
                                },
                            ),
                        };

                        authorization_rules.push(authorization_rule_for_field_preset(
                            &role_based_type_permission.role,
                            field_name,
                            &resolved_value,
                            flags,
                            conditions,
                        ));

                        resolved_field_presets.insert(
                            field_name.clone(),
                            FieldPresetInfo {
                                value: resolved_value,
                                deprecated: field_definition.deprecated.clone(),
                            },
                        );
                    }
                    if by_role
                        .insert(
                            role_based_type_permission.role.clone(),
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
            Ok(TypeInputPermissions {
                authorization_rules,
                by_role,
            })
        }
    }
}

// given a role and a field preset return an authorization rule
// that includes this preset field given `x-hasura-role` session variable matches the role
fn authorization_rule_for_field_preset(
    role: &Role,
    field_name: &FieldName,
    value: &ValueExpression,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> TypeInputAuthorizationRule {
    let condition = Condition::BinaryOperation {
        op: BinaryOperation::Equals,
        left: ValueExpression::SessionVariable(SessionVariableReference {
            name: SESSION_VARIABLE_ROLE,
            passed_as_json: flags.contains(open_dds::flags::Flag::JsonSessionVariables),
            disallow_unknown_fields: flags
                .contains(open_dds::flags::Flag::DisallowUnknownValuesInArguments),
        }),
        right: ValueExpression::Literal(serde_json::Value::String(role.0.clone())),
    };

    let hash = conditions.add(condition);

    TypeInputAuthorizationRule::FieldPresetValue {
        field_name: field_name.clone(),
        value: value.clone(),
        condition: Some(hash),
    }
}
