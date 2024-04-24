use super::stages::{data_connector_scalar_types, scalar_types, type_permissions};
use crate::metadata::resolved::argument::get_argument_mappings;
use crate::metadata::resolved::data_connector::DataConnectorLink;
use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::ndc_validation;
use crate::metadata::resolved::subgraph::{
    deserialize_qualified_btreemap, mk_qualified_type_reference, serialize_qualified_btreemap,
    ArgumentInfo, Qualified, QualifiedTypeReference,
};
use crate::metadata::resolved::types::{
    get_type_representation, get_underlying_object_type, unwrap_custom_type_name,
};
use crate::metadata::resolved::types::{mk_name, TypeMapping};
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use open_dds::commands::{
    self, CommandName, CommandV1, DataConnectorCommand, GraphQlRootFieldKind,
};

use open_dds::permissions::{CommandPermissionsV1, Role};
use open_dds::types::{BaseType, CustomTypeName, Deprecated, TypeName, TypeReference};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

use super::permission::{resolve_value_expression, ValueExpression};
use super::stages::data_connector_type_mappings;
use super::typecheck;
use super::types::{
    collect_type_mapping_for_source, TypeMappingCollectionError, TypeMappingToCollect,
};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandGraphQlApi {
    pub root_field_kind: GraphQlRootFieldKind,
    pub root_field_name: ast::Name,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandSource {
    pub data_connector: DataConnectorLink,
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
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandPermission {
    pub allow_execution: bool,
    pub argument_presets: BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpression)>,
}

fn is_valid_type(
    type_obj: &TypeReference,
    subgraph: &str,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> bool {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => is_valid_type(type_obj, subgraph, object_types, scalar_types),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => true,
            TypeName::Custom(type_name) => {
                let qualified_type_name =
                    Qualified::new(subgraph.to_string(), type_name.to_owned());

                get_type_representation(&qualified_type_name, object_types, scalar_types).is_ok()
            }
        },
    }
}

pub fn resolve_command(
    command: &CommandV1,
    subgraph: &str,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> Result<Command, Error> {
    let mut arguments = IndexMap::new();
    let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
    let command_description = command.description.clone();
    // duplicate command arguments should not be allowed
    for argument in &command.arguments {
        if is_valid_type(
            &argument.argument_type,
            subgraph,
            object_types,
            scalar_types,
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
        permissions: None,
        description: command_description,
    })
}

pub fn resolve_command_source(
    command_source: &commands::CommandSource,
    command: &mut Command,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
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
        .data_connectors_with_scalars
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
                .inner
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
                .inner
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

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_resolve) = get_argument_mappings(
        &command.arguments,
        &command_source.argument_mapping,
        ndc_arguments,
        object_types,
        scalar_types,
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
        .and_then(|custom_type_name| {
            get_underlying_object_type(custom_type_name, object_types).ok()
        });

    let mut type_mappings = BTreeMap::new();

    // Get the type mapping to resolve for the result type
    let source_result_type_mapping_to_resolve = command_result_base_object_type_name
        .as_ref()
        .map(|custom_type_name| {
            // Get the corresponding object_type (data_connector.object_type) associated with the result_type for the source
            let source_result_type_name =
                ndc_validation::get_underlying_named_type(source_result_type).map_err(|e| {
                    Error::CommandTypeMappingCollectionError {
                        command_name: command.name.clone(),
                        error: TypeMappingCollectionError::NDCValidationError(e),
                    }
                })?;

            let source_result_type_mapping_to_resolve = TypeMappingToCollect {
                type_name: custom_type_name,
                ndc_object_type_name: source_result_type_name,
            };

            Ok::<_, Error>(source_result_type_mapping_to_resolve)
        })
        .transpose()?;

    for type_mapping_to_collect in source_result_type_mapping_to_resolve
        .iter()
        .chain(argument_type_mappings_to_resolve.iter())
    {
        collect_type_mapping_for_source(
            type_mapping_to_collect,
            data_connector_type_mappings,
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

    command.source = Some(CommandSource {
        data_connector: DataConnectorLink::new(
            qualified_data_connector_name,
            data_connector_context.inner.url.clone(),
            data_connector_context.inner.headers,
        )?,
        source: command_source.data_connector_command.clone(),
        type_mappings,
        argument_mappings,
    });

    ndc_validation::validate_ndc_command(
        &command.name,
        command,
        data_connector_context.inner.schema,
    )?;

    Ok(())
}

pub fn resolve_command_permissions(
    command: &Command,
    permissions: &CommandPermissionsV1,
) -> Result<HashMap<Role, CommandPermission>, Error> {
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
                    // if our value is a literal, typecheck it against expected type
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
                        (
                            argument.argument_type.clone(),
                            resolve_value_expression(argument_preset.value.clone()),
                        ),
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
