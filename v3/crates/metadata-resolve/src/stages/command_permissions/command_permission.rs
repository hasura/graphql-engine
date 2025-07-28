use hasura_authn_core::{Role, SESSION_VARIABLE_ROLE, SessionVariableReference};
use indexmap::IndexMap;

use open_dds::query::ArgumentName;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};

use crate::stages::{
    boolean_expressions, commands, data_connector_scalar_types, models_graphql,
    object_relationships, scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::{
    BinaryOperation, Condition, Conditions, QualifiedTypeReference, ValueExpression,
    ValueExpressionOrPredicate,
};

use open_dds::permissions::{CommandPermissionOperand, CommandPermissionsV2};

use super::types::{Command, CommandPermission, CommandPermissionIssue, CommandPermissions};
use super::{AllowOrDeny, CommandAuthorizationRule};
use std::collections::BTreeMap;

pub fn resolve_command_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    command: &Command,
    permissions: &CommandPermissionsV2,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    conditions: &mut Conditions,
    issues: &mut Vec<CommandPermissionIssue>,
) -> Result<CommandPermissions, Error> {
    match &permissions.permissions {
        CommandPermissionOperand::RoleBased(role_based_command_permissions) => {
            let mut command_permissions_by_role = BTreeMap::new();
            let mut authorization_rules = vec![];

            for role_based_command_permission in role_based_command_permissions {
                let mut argument_presets = BTreeMap::new();

                authorization_rules.push(authorization_rule_for_access(
                    &role_based_command_permission.role,
                    role_based_command_permission.allow_execution,
                    flags,
                    conditions,
                ));

                for argument_preset in &role_based_command_permission.argument_presets {
                    if argument_presets.contains_key(&argument_preset.argument.value) {
                        return Err(Error::DuplicateCommandArgumentPreset {
                            command_name: command.name.clone(),
                            argument_name: argument_preset.argument.value.clone(),
                        });
                    }

                    let command_source = command.source.as_ref().ok_or_else(|| {
                        commands::CommandsError::CommandSourceRequiredForPredicate {
                            command_name: command.name.clone(),
                        }
                    })?;

                    match command.arguments.get(&argument_preset.argument.value) {
                        Some(argument) => {
                            let error_mapper = |type_error| Error::CommandArgumentPresetTypeError {
                                role: role_based_command_permission.role.clone(),
                                command_name: command.name.clone(),
                                argument_name: argument_preset.argument.value.clone(),
                                type_error,
                            };
                            let (value_expression_or_predicate, new_issues) =
                                resolve_value_expression_for_argument(
                                    &role_based_command_permission.role,
                                    flags,
                                    &argument_preset.argument,
                                    &argument_preset.value,
                                    &argument.argument_type,
                                    &command_source.data_connector,
                                    object_types,
                                    scalar_types,
                                    boolean_expression_types,
                                    models,
                                    &command_source.type_mappings,
                                    data_connector_scalars,
                                    error_mapper,
                                )?;

                            // Convert typecheck issues into command permission issues and collect them
                            for issue in new_issues {
                                issues.push(
                                    CommandPermissionIssue::CommandArgumentPresetTypecheckIssue {
                                        role: role_based_command_permission.role.clone(),
                                        command_name: command.name.clone(),
                                        argument_name: argument_preset.argument.value.clone(),
                                        typecheck_issue: issue,
                                    },
                                );
                            }

                            // store authorization rule for argument preset
                            authorization_rules.push(authorization_rule_for_argument_preset(
                                &role_based_command_permission.role,
                                &argument_preset.argument.value,
                                &argument.argument_type,
                                &value_expression_or_predicate,
                                flags,
                                conditions,
                            ));

                            argument_presets.insert(
                                argument_preset.argument.value.clone(),
                                (
                                    argument.argument_type.clone(),
                                    value_expression_or_predicate,
                                ),
                            );
                        }
                        None => {
                            return Err(Error::from(
                                commands::CommandsError::CommandArgumentPresetMismatch {
                                    command_name: command.name.clone(),
                                    argument_name: argument_preset.argument.value.clone(),
                                },
                            ));
                        }
                    }
                }

                let resolved_permission = CommandPermission {
                    allow_execution: role_based_command_permission.allow_execution,
                    argument_presets,
                };
                command_permissions_by_role.insert(
                    role_based_command_permission.role.clone(),
                    resolved_permission,
                );
            }
            Ok(CommandPermissions {
                by_role: command_permissions_by_role,
                authorization_rules,
            })
        }
    }
}

// given a role and allow_execution for a `Command`, return a CommandAuthorizationRule
// that allows or denies access for a role
fn authorization_rule_for_access(
    role: &Role,
    allow_execution: bool,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> CommandAuthorizationRule {
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

    CommandAuthorizationRule::Access {
        allow_or_deny: if allow_execution {
            AllowOrDeny::Allow
        } else {
            AllowOrDeny::Deny
        },
        condition: Some(hash),
    }
}

// given a role and a preset value, return a CommandAuthorizationRule
// that includes these presets when `x-hasura-role` matches our role
fn authorization_rule_for_argument_preset(
    role: &Role,
    argument_name: &ArgumentName,
    argument_type: &QualifiedTypeReference,
    value_expression_or_predicate: &ValueExpressionOrPredicate,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> CommandAuthorizationRule {
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

    match value_expression_or_predicate.clone().split_predicate() {
        Err(model_predicate) => CommandAuthorizationRule::ArgumentAuthPredicate {
            argument_name: argument_name.clone(),
            predicate: model_predicate,
            condition: Some(hash),
        },
        Ok(value_expression) => CommandAuthorizationRule::ArgumentPresetValue {
            argument_name: argument_name.clone(),
            argument_type: argument_type.clone(),
            value: value_expression,
            condition: Some(hash),
        },
    }
}
