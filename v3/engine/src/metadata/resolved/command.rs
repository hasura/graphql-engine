use crate::metadata::resolved::argument::get_argument_mappings;
use crate::metadata::resolved::data_connector::{DataConnector, DataConnectorContext};
use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::ndc_validation;
use crate::metadata::resolved::subgraph::{
    deserialize_qualified_btreemap, mk_qualified_type_reference, serialize_qualified_btreemap,
    ArgumentInfo, Qualified, QualifiedTypeReference,
};
use crate::metadata::resolved::types::{
    get_underlying_object_type, resolve_type_mappings, TypeMappingToResolve, TypeRepresentation,
};
use crate::metadata::resolved::types::{mk_name, TypeMapping};
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use open_dds::commands::{
    self, CommandName, CommandV1, DataConnectorCommand, GraphQlRootFieldKind,
};
use open_dds::data_connector::DataConnectorName;
use open_dds::permissions::{CommandPermissionsV1, Role};
use open_dds::types::{BaseType, CustomTypeName, TypeName, TypeReference};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandGraphQlApi {
    pub root_field_kind: GraphQlRootFieldKind,
    pub root_field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandSource {
    pub data_connector: DataConnector,
    pub source: DataConnectorCommand,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings: BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    pub argument_mappings: HashMap<ArgumentName, String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Command {
    pub name: Qualified<CommandName>,
    pub output_type: QualifiedTypeReference,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: Option<CommandGraphQlApi>,
    pub source: Option<CommandSource>,
    pub permissions: Option<HashMap<Role, CommandPermission>>,
    /// The underlying object type name, if exists for the output_type
    pub underlying_object_typename: Option<Qualified<CustomTypeName>>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandPermission {
    pub allow_execution: bool,
}

fn is_valid_type(
    type_obj: &TypeReference,
    subgraph: &str,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> bool {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => is_valid_type(type_obj, subgraph, types),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => true,
            TypeName::Custom(type_name) => types
                .get(&Qualified::new(subgraph.to_string(), type_name.to_owned()))
                .is_some(),
        },
    }
}

pub fn resolve_command(
    command: &CommandV1,
    subgraph: &str,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<Command, Error> {
    let mut arguments = IndexMap::new();
    let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
    let command_description = command.description.clone();
    // duplicate command arguments should not be allowed
    for argument in &command.arguments {
        if is_valid_type(&argument.argument_type, subgraph, types) {
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
        permissions: None,
        underlying_object_typename: None,
        description: command_description,
    })
}

pub fn resolve_command_source(
    command_source: &commands::CommandSource,
    command: &mut Command,
    subgraph: &str,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<(), Error> {
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
        .get(&qualified_data_connector_name)
        .ok_or_else(|| Error::UnknownCommandDataConnector {
            command_name: command.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
        })?;

    // Get the result type and arguments of the function or procedure used as the ndc source for commands
    // object type
    let (source_result_type, ndc_arguments) = match &command_source.data_connector_command {
        DataConnectorCommand::Procedure(procedure) => {
            let source_procedure = data_connector_context
                .schema
                .procedures
                .iter()
                .find(|proc| proc.name == *procedure.0)
                .ok_or_else(|| Error::UnknownCommandProcedure {
                    command_name: command.name.clone(),
                    data_connector: qualified_data_connector_name.clone(),
                    procedure: procedure.clone(),
                })?;

            (&source_procedure.result_type, &source_procedure.arguments)
        }
        DataConnectorCommand::Function(function) => {
            let source_function = data_connector_context
                .schema
                .functions
                .iter()
                .find(|func| func.name == *function.0)
                .ok_or_else(|| Error::UnknownCommandFunction {
                    command_name: command.name.clone(),
                    data_connector: qualified_data_connector_name.clone(),
                    function: function.clone(),
                })?;

            (&source_function.result_type, &source_function.arguments)
        }
    };

    command.underlying_object_typename = get_underlying_object_type(&command.output_type, types)?;

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_resolve) = get_argument_mappings(
        &command.arguments,
        &command_source.argument_mapping,
        ndc_arguments,
        &data_connector_context.schema.object_types,
        types,
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

    // Get the type mapping to resolve for the result type
    let source_result_type_mapping_to_resolve = command
        .underlying_object_typename
        .as_ref()
        .map(|custom_type_name| {
            // Get the corresponding object_type (data_connector.object_type) associated with the result_type for the source
            let source_result_type_name =
                ndc_validation::get_underlying_named_type(source_result_type);
            let source_result_object_type = data_connector_context
                .schema
                .object_types
                .get(source_result_type_name)
                .ok_or_else(|| {
                    ndc_validation::NDCValidationError::NoSuchType(
                        source_result_type_name.to_string(),
                    )
                })?;

            let source_result_type_mapping_to_resolve = TypeMappingToResolve {
                type_name: custom_type_name,
                ndc_object_type_name: source_result_type_name,
                ndc_object_type: source_result_object_type,
            };

            Ok::<_, Error>(source_result_type_mapping_to_resolve)
        })
        .transpose()?;

    // Resolve all the type mappings (types from the arguments and the result type)
    let namespaced_type_mappings = command_source
        .type_mapping
        .iter()
        .map(|(type_name, mapping)| {
            (
                Qualified::new(subgraph.to_string(), type_name.clone()),
                mapping,
            )
        })
        .collect();
    let mappings_to_resolve = source_result_type_mapping_to_resolve
        .iter()
        .chain(argument_type_mappings_to_resolve.iter());
    let type_mappings = resolve_type_mappings(
        mappings_to_resolve,
        &namespaced_type_mappings,
        types,
        &data_connector_context.schema.object_types,
    )
    .map_err(
        |type_validation_error| Error::CommandTypeMappingValidationError {
            command_name: command.name.clone(),
            error: type_validation_error,
        },
    )?;

    let data_connector_name = Qualified::new(
        subgraph.to_string(),
        command_source.data_connector_name.clone(),
    );
    command.source = Some(CommandSource {
        data_connector: DataConnector::new(
            data_connector_name,
            data_connector_context.url.clone(),
            data_connector_context.headers,
        )?,
        source: command_source.data_connector_command.clone(),
        type_mappings,
        argument_mappings,
    });

    ndc_validation::validate_ndc_command(&command.name, command, data_connector_context.schema)?;

    Ok(())
}

pub fn resolve_command_permissions(
    permissions: &CommandPermissionsV1,
) -> Result<HashMap<Role, CommandPermission>, Error> {
    let mut validated_permissions = HashMap::new();
    for command_permission in &permissions.permissions {
        // TODO: Use the permission predicates/presets
        let resolved_permission = CommandPermission {
            allow_execution: command_permission.allow_execution,
        };
        validated_permissions.insert(command_permission.role.clone(), resolved_permission);
    }
    Ok(validated_permissions)
}
