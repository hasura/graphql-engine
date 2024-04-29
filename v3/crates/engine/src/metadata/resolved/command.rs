use super::stages::{
    boolean_expressions, commands, data_connector_scalar_types, data_connector_type_mappings,
    relationships,
};
use crate::metadata::resolved::argument::resolve_value_expression_for_argument;
use crate::metadata::resolved::error::Error;

use crate::metadata::resolved::subgraph::Qualified;

use open_dds::permissions::{CommandPermissionsV1, Role};
use open_dds::types::CustomTypeName;

use std::collections::{BTreeMap, HashMap};

use super::typecheck;

pub fn resolve_command_permissions(
    command: &commands::Command,
    permissions: &CommandPermissionsV1,
    object_types: &HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    subgraph: &str,
) -> Result<HashMap<Role, commands::CommandPermission>, Error> {
    let mut validated_permissions = HashMap::new();
    for command_permission in &permissions.permissions {
        let mut argument_presets = BTreeMap::new();

        for argument_preset in &command_permission.argument_presets {
            if argument_presets.contains_key(&argument_preset.argument) {
                return Err(Error::DuplicateCommandArgumentPreset {
                    command_name: command.name.clone(),
                    argument_name: argument_preset.argument.clone(),
                });
            }

            match command.arguments.get(&argument_preset.argument) {
                Some(argument) => {
                    let value_expression = resolve_value_expression_for_argument(
                        &argument_preset.argument,
                        &argument_preset.value,
                        &argument.argument_type,
                        subgraph,
                        object_types,
                        boolean_expression_types,
                        data_connectors,
                        data_connector_type_mappings,
                    )?;

                    // additionally typecheck literals
                    // we do this outside the argument resolve so that we can emit a command-specific error
                    // on typechecking failure
                    typecheck::typecheck_value_expression(
                        &argument.argument_type,
                        &argument_preset.value,
                    )
                    .map_err(|type_error| {
                        Error::CommandArgumentPresetTypeError {
                            command_name: command.name.clone(),
                            argument_name: argument_preset.argument.clone(),
                            type_error,
                        }
                    })?;

                    argument_presets.insert(
                        argument_preset.argument.clone(),
                        (argument.argument_type.clone(), value_expression),
                    );
                }
                None => {
                    return Err(Error::CommandArgumentPresetMismatch {
                        command_name: command.name.clone(),
                        argument_name: argument_preset.argument.clone(),
                    });
                }
            }
        }

        let resolved_permission = commands::CommandPermission {
            allow_execution: command_permission.allow_execution,
            argument_presets,
        };
        validated_permissions.insert(command_permission.role.clone(), resolved_permission);
    }
    Ok(validated_permissions)
}
