use hasura_authn_core::Role;
use indexmap::IndexMap;

use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};

use crate::stages::{
    boolean_expressions, commands, data_connector_scalar_types, data_connectors, models_graphql,
    object_boolean_expressions, relationships, scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use open_dds::arguments::ArgumentName;

use crate::helpers::argument::resolve_value_expression_for_argument;

use open_dds::permissions::CommandPermissionsV1;

use super::types::CommandPermission;
use crate::helpers::typecheck;
use std::collections::BTreeMap;

// get the ndc_models::Type for an argument if it is available
fn get_command_source_argument<'a>(
    argument_name: &'a ArgumentName,
    command: &'a commands::Command,
) -> Option<&'a ndc_models::Type> {
    command
        .source
        .as_ref()
        .and_then(|source| {
            source
                .argument_mappings
                .get(argument_name)
                .map(|connector_argument_name| source.source_arguments.get(connector_argument_name))
        })
        .flatten()
}

pub fn resolve_command_permissions(
    command: &commands::Command,
    permissions: &CommandPermissionsV1,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    subgraph: &str,
) -> Result<BTreeMap<Role, CommandPermission>, Error> {
    let mut validated_permissions = BTreeMap::new();
    for command_permission in &permissions.permissions {
        let mut argument_presets = BTreeMap::new();

        for argument_preset in &command_permission.argument_presets {
            if argument_presets.contains_key(&argument_preset.argument) {
                return Err(Error::DuplicateCommandArgumentPreset {
                    command_name: command.name.clone(),
                    argument_name: argument_preset.argument.clone(),
                });
            }

            let source_argument_type =
                get_command_source_argument(&argument_preset.argument, command);

            let data_connector_name = command
                .source
                .as_ref()
                .map(|source| &source.data_connector.name)
                .ok_or(Error::CommandSourceRequiredForPredicate {
                    command_name: command.name.clone(),
                })?;

            let data_connector_context =
                data_connectors.0.get(data_connector_name).ok_or_else(|| {
                    Error::UnknownCommandDataConnector {
                        command_name: command.name.clone(),
                        data_connector: data_connector_name.clone(),
                    }
                })?;

            let data_connector_link = data_connectors::DataConnectorLink::new(
                data_connector_name.clone(),
                data_connector_context,
            )?;

            match command.arguments.get(&argument_preset.argument) {
                Some(argument) => {
                    let value_expression = resolve_value_expression_for_argument(
                        &argument_preset.argument,
                        &argument_preset.value,
                        &argument.argument_type,
                        source_argument_type,
                        &data_connector_link,
                        subgraph,
                        object_types,
                        scalar_types,
                        object_boolean_expression_types,
                        boolean_expression_types,
                        models,
                        data_connector_scalars,
                    )?;

                    // additionally typecheck literals
                    // we do this outside the argument resolve so that we can emit a command-specific error
                    // on typechecking failure
                    typecheck::typecheck_value_expression_or_predicate(
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

        let resolved_permission = CommandPermission {
            allow_execution: command_permission.allow_execution,
            argument_presets,
        };
        validated_permissions.insert(command_permission.role.clone(), resolved_permission);
    }
    Ok(validated_permissions)
}
