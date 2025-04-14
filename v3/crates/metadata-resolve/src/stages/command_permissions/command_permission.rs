use hasura_authn_core::Role;
use indexmap::IndexMap;

use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};

use crate::stages::{
    boolean_expressions, commands, data_connector_scalar_types, models_graphql,
    object_relationships, scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use crate::helpers::argument::resolve_value_expression_for_argument;

use open_dds::permissions::CommandPermissionsV1;

use super::types::{CommandPermission, CommandPermissionIssue};
use std::collections::BTreeMap;

pub fn resolve_command_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    command: &commands::Command,
    permissions: &CommandPermissionsV1,
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
    issues: &mut Vec<CommandPermissionIssue>,
) -> Result<BTreeMap<Role, CommandPermission>, Error> {
    let mut validated_permissions = BTreeMap::new();
    for command_permission in &permissions.permissions {
        let mut argument_presets = BTreeMap::new();

        for argument_preset in &command_permission.argument_presets {
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
                        role: command_permission.role.clone(),
                        command_name: command.name.clone(),
                        argument_name: argument_preset.argument.value.clone(),
                        type_error,
                    };
                    let (value_expression, new_issues) = resolve_value_expression_for_argument(
                        &command_permission.role,
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
                                role: command_permission.role.clone(),
                                command_name: command.name.clone(),
                                argument_name: argument_preset.argument.value.clone(),
                                typecheck_issue: issue,
                            },
                        );
                    }

                    argument_presets.insert(
                        argument_preset.argument.value.clone(),
                        (argument.argument_type.clone(), value_expression),
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
            allow_execution: command_permission.allow_execution,
            argument_presets,
        };
        validated_permissions.insert(command_permission.role.clone(), resolved_permission);
    }
    Ok(validated_permissions)
}
