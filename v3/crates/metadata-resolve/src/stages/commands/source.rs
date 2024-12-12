use std::sync::Arc;

use crate::helpers::argument::{get_argument_mappings, ArgumentMappingResults};
use crate::helpers::ndc_validation::{self};
use crate::helpers::types::{object_type_exists, unwrap_custom_type_name};
use crate::stages::{
    boolean_expressions, data_connectors, object_boolean_expressions, scalar_types,
    type_permissions,
};
use crate::types::subgraph::Qualified;

use super::types::CommandsIssue;
pub use super::types::{Command, CommandSource};
use open_dds::commands::{self, DataConnectorCommand};

use open_dds::identifier::SubgraphName;
use open_dds::types::{CustomTypeName, DataConnectorArgumentName};

use std::collections::BTreeMap;

use super::error::CommandsError;
use crate::helpers::type_mappings::{self, SpecialCaseTypeMapping};

struct CommandSourceResponse {
    result_type: ndc_models::Type,
    arguments: BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
}

pub fn resolve_command_source(
    command_source: &commands::CommandSource,
    command: &Command,
    subgraph: &SubgraphName,
    data_connectors: &data_connectors::DataConnectors,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> Result<(CommandSource, Vec<CommandsIssue>), CommandsError> {
    if command.source.is_some() {
        return Err(CommandsError::DuplicateCommandSourceDefinition {
            command_name: command.name.clone(),
        });
    }

    // check if data_connector for the command exists
    let qualified_data_connector_name =
        Qualified::new(subgraph.clone(), command_source.data_connector_name.clone());

    let data_connector_context = data_connectors
        .0
        .get(&qualified_data_connector_name)
        .ok_or_else(|| CommandsError::UnknownCommandDataConnector {
            command_name: command.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
        })?;

    // Get the result type and arguments of the function or procedure used as the ndc source for commands
    // object type
    let command_source_response = match &command_source.data_connector_command {
        DataConnectorCommand::Procedure(procedure) => {
            let source_procedure = data_connector_context
                .schema
                .procedures
                .get(procedure)
                .ok_or_else(|| CommandsError::UnknownCommandProcedure {
                    command_name: command.name.clone(),
                    data_connector: qualified_data_connector_name.clone(),
                    procedure: procedure.clone(),
                })?;

            CommandSourceResponse {
                result_type: source_procedure.result_type.clone(),
                arguments: source_procedure
                    .arguments
                    .iter()
                    .map(|(k, v)| {
                        (
                            DataConnectorArgumentName::from(k.as_str()),
                            v.argument_type.clone(),
                        )
                    })
                    .collect(),
            }
        }
        DataConnectorCommand::Function(function) => {
            let source_function = data_connector_context
                .schema
                .functions
                .get(function)
                .ok_or_else(|| CommandsError::UnknownCommandFunction {
                    command_name: command.name.clone(),
                    data_connector: qualified_data_connector_name.clone(),
                    function: function.clone(),
                })?;

            CommandSourceResponse {
                result_type: source_function.result_type.clone(),
                arguments: source_function
                    .arguments
                    .iter()
                    .map(|(k, v)| {
                        (
                            DataConnectorArgumentName::from(k.as_str()),
                            v.argument_type.clone(),
                        )
                    })
                    .collect(),
            }
        }
    };

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let ArgumentMappingResults {
        argument_mappings,
        data_connector_link_argument_presets,
        argument_type_mappings_to_resolve,
        issues,
    } = get_argument_mappings(
        &command.arguments,
        &command_source.argument_mapping,
        &command_source_response.arguments,
        data_connector_context,
        object_types,
        scalar_types,
        object_boolean_expression_types,
        boolean_expression_types,
    )
    .map_err(|err| match &command_source.data_connector_command {
        DataConnectorCommand::Function(function_name) => {
            CommandsError::CommandFunctionArgumentMappingError {
                data_connector_name: qualified_data_connector_name.clone(),
                command_name: command.name.clone(),
                function_name: function_name.clone(),
                error: err,
            }
        }
        DataConnectorCommand::Procedure(procedure_name) => {
            CommandsError::CommandProcedureArgumentMappingError {
                data_connector_name: qualified_data_connector_name.clone(),
                command_name: command.name.clone(),
                procedure_name: procedure_name.clone(),
                error: err,
            }
        }
    })?;

    let issues = issues
        .into_iter()
        .map(|issue| match &command_source.data_connector_command {
            DataConnectorCommand::Function(function_name) => {
                CommandsIssue::FunctionArgumentMappingIssue {
                    data_connector_name: qualified_data_connector_name.clone(),
                    command_name: command.name.clone(),
                    function_name: function_name.clone(),
                    issue,
                }
            }
            DataConnectorCommand::Procedure(procedure_name) => {
                CommandsIssue::ProcedureArgumentMappingIssue {
                    data_connector_name: qualified_data_connector_name.clone(),
                    command_name: command.name.clone(),
                    procedure_name: procedure_name.clone(),
                    issue,
                }
            }
        })
        .collect();

    // get object type name if it exists for the output type, and refers to a valid object
    let command_result_base_object_type_name = unwrap_custom_type_name(&command.output_type)
        .and_then(|custom_type_name| object_type_exists(custom_type_name, object_types).ok());

    let mut type_mappings = BTreeMap::new();

    // Get the type mapping to resolve for the result type
    let source_result_type_mapping_to_resolve = command_result_base_object_type_name
        .as_ref()
        .map(|custom_type_name| {
            // Get the corresponding object_type (data_connector.object_type) associated with the result_type for the source
            let source_result_type_name =
                ndc_validation::get_underlying_named_type(&command_source_response.result_type);

            let source_result_type_mapping_to_resolve = type_mappings::TypeMappingToCollect {
                type_name: custom_type_name,
                ndc_object_type_name: source_result_type_name,
            };

            Ok::<_, CommandsError>(source_result_type_mapping_to_resolve)
        })
        .transpose()?;

    // Get the ndc object type from the source result type name
    let ndc_object_type = source_result_type_mapping_to_resolve
        .as_ref()
        .map(|type_mapping_to_resolve| {
            let ndc_type_name = type_mapping_to_resolve.ndc_object_type_name;
            data_connector_context
                .schema
                .object_types
                .get(ndc_type_name)
                .ok_or_else(|| CommandsError::CommandTypeMappingCollectionError {
                    command_name: command.name.clone(),
                    error: type_mappings::TypeMappingCollectionError::NDCValidationError(
                        crate::NDCValidationError::NoSuchType(ndc_type_name.as_str().to_owned()),
                    ),
                })
        })
        .transpose()?;

    let special_case = data_connector_context
        .response_headers
        .as_ref()
        .zip(ndc_object_type)
        .map(
            |(response_config, ndc_object_type)| SpecialCaseTypeMapping {
                response_config,
                ndc_object_type,
            },
        );

    for type_mapping_to_collect in source_result_type_mapping_to_resolve
        .iter()
        .chain(argument_type_mappings_to_resolve.iter())
    {
        type_mappings::collect_type_mapping_for_source(
            type_mapping_to_collect,
            &qualified_data_connector_name,
            object_types,
            scalar_types,
            &mut type_mappings,
            special_case.as_ref(),
        )
        .map_err(|error| CommandsError::CommandTypeMappingCollectionError {
            command_name: command.name.clone(),
            error,
        })?;
    }

    let mut command_source = CommandSource {
        data_connector: data_connectors::DataConnectorLink::new(
            qualified_data_connector_name,
            data_connector_context,
        )
        .map(Arc::new)?,
        source: command_source.data_connector_command.clone(),
        ndc_type_opendd_type_same: true,
        type_mappings,
        argument_mappings,
        data_connector_link_argument_presets,
        source_arguments: command_source_response.arguments,
    };

    let commands_response_config = special_case.map(|x| x.response_config);
    let source_type_opendd_type_same = ndc_validation::validate_ndc_command(
        &command.name,
        &command_source,
        &command.output_type,
        &data_connector_context.schema,
        commands_response_config,
    )?;

    command_source.ndc_type_opendd_type_same = source_type_opendd_type_same;

    Ok((command_source, issues))
}
