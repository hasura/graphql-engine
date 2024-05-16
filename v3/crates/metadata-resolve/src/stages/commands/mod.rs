mod types;

use crate::helpers::argument::get_argument_mappings;
use crate::helpers::ndc_validation;
use crate::helpers::types::{
    get_type_representation, mk_name, object_type_exists, unwrap_custom_type_name,
};
use crate::stages::{
    data_connector_scalar_types, data_connectors, models, object_boolean_expressions, scalar_types,
    type_permissions,
};
use crate::types::error::Error;
use crate::types::subgraph::{mk_qualified_type_reference, ArgumentInfo, Qualified};
use indexmap::IndexMap;
use ref_cast::RefCast;

use open_dds::commands::{self, CommandName, CommandV1, DataConnectorCommand};
use open_dds::data_connector::DataConnectorObjectType;
pub use types::{Command, CommandGraphQlApi, CommandSource};

use open_dds::types::{BaseType, CustomTypeName, TypeName, TypeReference};

use std::collections::BTreeMap;

use crate::helpers::type_mappings;

/// resolve commands
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<IndexMap<Qualified<CommandName>, Command>, Error> {
    let mut commands: IndexMap<Qualified<CommandName>, Command> = IndexMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        let mut resolved_command = resolve_command(
            command,
            subgraph,
            object_types,
            scalar_types,
            object_boolean_expression_types,
        )?;
        if let Some(command_source) = &command.source {
            let command_source = resolve_command_source(
                command_source,
                &resolved_command,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                object_boolean_expression_types,
            )?;
            resolved_command.source = Some(command_source);
        }
        let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
        if commands
            .insert(qualified_command_name.clone(), resolved_command)
            .is_some()
        {
            return Err(Error::DuplicateCommandDefinition {
                name: qualified_command_name,
            });
        }
    }
    Ok(commands)
}

fn type_exists(
    type_obj: &TypeReference,
    subgraph: &str,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> bool {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => type_exists(
            type_obj,
            subgraph,
            object_types,
            scalar_types,
            object_boolean_expression_types,
        ),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => true,
            TypeName::Custom(type_name) => {
                let qualified_type_name =
                    Qualified::new(subgraph.to_string(), type_name.to_owned());

                get_type_representation(
                    &qualified_type_name,
                    object_types,
                    scalar_types,
                    object_boolean_expression_types,
                )
                .is_ok()
            }
        },
    }
}

pub fn resolve_command(
    command: &CommandV1,
    subgraph: &str,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<Command, Error> {
    let mut arguments = IndexMap::new();
    let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
    let command_description = command.description.clone();
    // duplicate command arguments should not be allowed
    for argument in &command.arguments {
        if type_exists(
            &argument.argument_type,
            subgraph,
            object_types,
            scalar_types,
            object_boolean_expression_types,
        ) {
            if arguments
                .insert(
                    argument.name.clone(),
                    ArgumentInfo {
                        argument_type: mk_qualified_type_reference(
                            &argument.argument_type,
                            subgraph,
                        ),
                        description: argument.description.clone(),
                    },
                )
                .is_some()
            {
                return Err(Error::DuplicateCommandArgumentDefinition {
                    command_name: qualified_command_name,
                    argument_name: argument.name.clone(),
                });
            }
        } else {
            return Err(Error::UnknownCommandArgumentType {
                command_name: qualified_command_name,
                argument_name: argument.name.clone(),
                argument_type: argument.argument_type.clone(),
            });
        }
    }

    let graphql_api = match &command.graphql {
        None => Ok(None),
        Some(graphql_definition) => {
            mk_name(graphql_definition.root_field_name.0.as_ref()).map(|f| {
                Some(CommandGraphQlApi {
                    root_field_kind: graphql_definition.root_field_kind.clone(),
                    root_field_name: f,
                    deprecated: graphql_definition.deprecated.clone(),
                })
            })
        }
    }?;

    Ok(Command {
        name: qualified_command_name,
        output_type: mk_qualified_type_reference(&command.output_type, subgraph),
        arguments,
        graphql_api,
        source: None,
        description: command_description,
    })
}

struct CommandSourceResponse {
    result_type: ndc_models::Type,
    arguments: BTreeMap<models::ConnectorArgumentName, ndc_models::Type>,
}

pub fn resolve_command_source(
    command_source: &commands::CommandSource,
    command: &Command,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<CommandSource, Error> {
    if command.source.is_some() {
        return Err(Error::DuplicateCommandSourceDefinition {
            command_name: command.name.clone(),
        });
    }

    // check if data_connector for the command exists
    let qualified_data_connector_name = Qualified::new(
        subgraph.to_string(),
        command_source.data_connector_name.clone(),
    );

    let data_connector_context = data_connectors
        .data_connectors_with_scalars
        .get(&qualified_data_connector_name)
        .ok_or_else(|| Error::UnknownCommandDataConnector {
            command_name: command.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
        })?;

    // Get the result type and arguments of the function or procedure used as the ndc source for commands
    // object type
    let command_source_response = match &command_source.data_connector_command {
        DataConnectorCommand::Procedure(procedure) => {
            let source_procedure = data_connector_context
                .inner
                .schema
                .procedures
                .get(procedure)
                .ok_or_else(|| Error::UnknownCommandProcedure {
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
                            models::ConnectorArgumentName(k.clone()),
                            v.argument_type.clone(),
                        )
                    })
                    .collect(),
            }
        }
        DataConnectorCommand::Function(function) => {
            let source_function = data_connector_context
                .inner
                .schema
                .functions
                .get(function)
                .ok_or_else(|| Error::UnknownCommandFunction {
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
                            models::ConnectorArgumentName(k.clone()),
                            v.argument_type.clone(),
                        )
                    })
                    .collect(),
            }
        }
    };

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_resolve) = get_argument_mappings(
        &command.arguments,
        &command_source.argument_mapping,
        &command_source_response.arguments,
        object_types,
        scalar_types,
        object_boolean_expression_types,
    )
    .map_err(|err| match &command_source.data_connector_command {
        DataConnectorCommand::Function(function_name) => {
            Error::CommandFunctionArgumentMappingError {
                data_connector_name: qualified_data_connector_name.clone(),
                command_name: command.name.clone(),
                function_name: function_name.clone(),
                error: err,
            }
        }
        DataConnectorCommand::Procedure(procedure_name) => {
            Error::CommandProcedureArgumentMappingError {
                data_connector_name: qualified_data_connector_name.clone(),
                command_name: command.name.clone(),
                procedure_name: procedure_name.clone(),
                error: err,
            }
        }
    })?;

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
                ndc_validation::get_underlying_named_type(&command_source_response.result_type)
                    .map_err(|e| Error::CommandTypeMappingCollectionError {
                        command_name: command.name.clone(),
                        error: type_mappings::TypeMappingCollectionError::NDCValidationError(e),
                    })?;

            let source_result_type_mapping_to_resolve = type_mappings::TypeMappingToCollect {
                type_name: custom_type_name,
                ndc_object_type_name: DataConnectorObjectType::ref_cast(source_result_type_name),
            };

            Ok::<_, Error>(source_result_type_mapping_to_resolve)
        })
        .transpose()?;

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
        )
        .map_err(|error| Error::CommandTypeMappingCollectionError {
            command_name: command.name.clone(),
            error,
        })?;
    }

    let command_source = CommandSource {
        data_connector: data_connectors::DataConnectorLink::new(
            qualified_data_connector_name,
            data_connector_context.inner.url.clone(),
            data_connector_context.inner.headers,
        )?,
        source: command_source.data_connector_command.clone(),
        type_mappings,
        argument_mappings,
        source_arguments: command_source_response.arguments,
    };

    ndc_validation::validate_ndc_command(
        &command.name,
        command,
        &data_connector_context.inner.schema,
    )?;

    Ok(command_source)
}
