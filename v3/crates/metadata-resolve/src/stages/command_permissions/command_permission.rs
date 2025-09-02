use hasura_authn_core::{Role, SESSION_VARIABLE_ROLE, SessionVariableReference};
use indexmap::IndexMap;

use open_dds::authorization::{Allow, Deny, PresetArgument};
use open_dds::query::ArgumentName;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};

use crate::stages::type_permissions::{
    ObjectTypeToCheck, resolve_condition, types_that_use_fancy_auth,
};
use crate::stages::{
    boolean_expressions, commands, data_connector_scalar_types, models_graphql,
    object_relationships, scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::{
    BinaryOperation, CommandSource, Condition, Conditions, QualifiedTypeReference, ValueExpression,
    ValueExpressionOrPredicate, unwrap_custom_type_name,
};

use open_dds::permissions::{CommandPermissionOperand, CommandPermissionsV2};

use super::types::{Command, CommandPermission, CommandPermissionIssue, CommandPermissions};
use super::{AllowOrDeny, CommandAuthorizationRule};
use std::collections::{BTreeMap, BTreeSet};

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
    warn_on_fancy_type_permissions(command, permissions, object_types, issues);

    match &permissions.permissions {
        CommandPermissionOperand::RoleBased(role_based_command_permissions) => {
            resolve_role_based_command_permissions(
                flags,
                command,
                role_based_command_permissions,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
                data_connector_scalars,
                conditions,
                issues,
            )
        }
        CommandPermissionOperand::RulesBased(command_authorization_rules) => {
            resolve_rules_based_command_permissions(
                flags,
                command,
                command_authorization_rules,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
                data_connector_scalars,
                conditions,
                issues,
            )
        }
    }
}

fn resolve_rules_based_command_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    command: &Command,
    command_authorization_rules: &Vec<open_dds::authorization::CommandAuthorizationRule>,
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
    let mut authorization_rules = vec![];

    for command_authorization_rule in command_authorization_rules {
        let authorization_rule = match command_authorization_rule {
            open_dds::authorization::CommandAuthorizationRule::Allow(Allow { condition }) => {
                let hash = condition
                    .as_ref()
                    .map(|condition| conditions.add(resolve_condition(condition, flags)));

                CommandAuthorizationRule::Access {
                    allow_or_deny: AllowOrDeny::Allow,
                    condition: hash,
                }
            }
            open_dds::authorization::CommandAuthorizationRule::Deny(Deny { condition }) => {
                let hash = conditions.add(resolve_condition(condition, flags));

                CommandAuthorizationRule::Access {
                    allow_or_deny: AllowOrDeny::Deny,
                    condition: Some(hash),
                }
            }
            open_dds::authorization::CommandAuthorizationRule::PresetArgument(PresetArgument {
                argument_name,
                condition,
                value,
            }) => {
                let command_source = command.source.as_ref().ok_or_else(|| {
                    commands::CommandsError::CommandSourceRequiredForArgumentPreset {
                        command_name: command.name.clone(),
                    }
                })?;

                let hash = condition
                    .as_ref()
                    .map(|condition| conditions.add(resolve_condition(condition, flags)));

                let (argument_type, value_expression_or_predicate) = resolve_argument_preset(
                    flags,
                    command,
                    command_source,
                    argument_name,
                    value,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
                    models,
                    data_connector_scalars,
                    None,
                    issues,
                )?;

                match value_expression_or_predicate.split_predicate() {
                    Ok(value_expression) => CommandAuthorizationRule::ArgumentPresetValue {
                        condition: hash,
                        argument_type,
                        argument_name: argument_name.value.clone(),
                        value: value_expression,
                    },
                    Err(boolean_expression) => CommandAuthorizationRule::ArgumentAuthPredicate {
                        condition: hash,
                        argument_name: argument_name.value.clone(),
                        predicate: Box::new(boolean_expression),
                    },
                }
            }
        };

        authorization_rules.push(authorization_rule);
    }

    Ok(CommandPermissions {
        by_role: BTreeMap::new(),
        authorization_rules,
    })
}
fn resolve_role_based_command_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    command: &Command,
    role_based_command_permissions: &Vec<open_dds::permissions::CommandPermission>,
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
                commands::CommandsError::CommandSourceRequiredForArgumentPreset {
                    command_name: command.name.clone(),
                }
            })?;

            match command.arguments.get(&argument_preset.argument.value) {
                Some(argument) => {
                    let (argument_type, value_expression_or_predicate) = resolve_argument_preset(
                        flags,
                        command,
                        command_source,
                        &argument_preset.argument,
                        &argument_preset.value,
                        object_types,
                        scalar_types,
                        boolean_expression_types,
                        models,
                        data_connector_scalars,
                        Some(&role_based_command_permission.role),
                        issues,
                    )?;

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
                        (argument_type.clone(), value_expression_or_predicate),
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

fn resolve_argument_preset(
    flags: &open_dds::flags::OpenDdFlags,
    command: &Command,
    command_source: &CommandSource,
    argument_name: &ArgumentName,
    value_expression_or_predicate: &open_dds::permissions::ValueExpressionOrPredicate,
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
    role: Option<&Role>,
    issues: &mut Vec<CommandPermissionIssue>,
) -> Result<(QualifiedTypeReference, ValueExpressionOrPredicate), Error> {
    let type_error_mapper = |type_error| Error::CommandArgumentPresetTypeError {
        role: role.cloned(),
        command_name: command.name.clone(),
        argument_name: argument_name.clone(),
        type_error,
    };

    match command.arguments.get(argument_name) {
        Some(argument) => {
            let (value_expression_or_predicate, new_issues) =
                resolve_value_expression_for_argument(
                    role,
                    flags,
                    argument_name,
                    value_expression_or_predicate,
                    &argument.argument_type,
                    &command_source.data_connector,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
                    models,
                    &command_source.type_mappings,
                    data_connector_scalars,
                    type_error_mapper,
                )?;

            // Convert typecheck issues into command permission issues and collect them
            for issue in new_issues {
                issues.push(
                    CommandPermissionIssue::CommandArgumentPresetTypecheckIssue {
                        role: role.cloned(),
                        command_name: command.name.clone(),
                        argument_name: argument_name.clone(),
                        typecheck_issue: issue,
                    },
                );
            }

            Ok((
                argument.argument_type.clone(),
                value_expression_or_predicate,
            ))
        }
        None => Err(Error::from(
            commands::CommandsError::CommandArgumentPresetMismatch {
                command_name: command.name.clone(),
                argument_name: argument_name.clone(),
            },
        )),
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
            predicate: Box::new(model_predicate),
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

// raise a warning if this command's return type uses fancy auth and has a graphql api
// (as it won't appear in the schema)
fn warn_on_fancy_type_permissions(
    command: &Command,
    command_permissions: &CommandPermissionsV2,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    issues: &mut Vec<CommandPermissionIssue>,
) {
    let has_graphql_api = command.graphql_api.is_some();

    if !has_graphql_api {
        return;
    }

    // raise issue if command itself uses rules-based auth
    if matches!(
        command_permissions.permissions,
        CommandPermissionOperand::RulesBased(_)
    ) {
        issues.push(CommandPermissionIssue::CommandUsesRulesBasedAuthorization {
            command_name: command.name.clone(),
        });
        // don't need the more granular warnings if command is not exposed in the schema
        return;
    }

    if has_graphql_api {
        let mut argument_types_with_fancy_auth = BTreeSet::new();
        for argument in command.arguments.values() {
            if let Some(custom_type_name) = unwrap_custom_type_name(&argument.argument_type) {
                argument_types_with_fancy_auth.extend(types_that_use_fancy_auth(
                    object_types,
                    custom_type_name,
                    ObjectTypeToCheck::Input,
                ));
            }
        }

        // raise issue for every input type that uses rules-based auth
        for argument_type in argument_types_with_fancy_auth {
            issues.push(
                CommandPermissionIssue::CommandArgumentTypeUsesRulesBasedAuthorization {
                    command_name: command.name.clone(),
                    argument_type,
                },
            );
        }

        // raise issue for every output type that uses rules-based auth
        if let Some(custom_return_type_name) = unwrap_custom_type_name(&command.output_type) {
            for custom_type_name in types_that_use_fancy_auth(
                object_types,
                custom_return_type_name,
                ObjectTypeToCheck::Output,
            ) {
                issues.push(
                    CommandPermissionIssue::CommandReturnTypeUsesRulesBasedAuthorization {
                        command_name: command.name.clone(),
                        data_type: custom_type_name.clone(),
                    },
                );
            }
        }
    }
}
