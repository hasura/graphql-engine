use crate::helpers::{
    argument::ArgumentMappingError, ndc_validation::NDCValidationError,
    type_mappings::TypeMappingCollectionError,
};
use crate::stages::{data_connectors, graphql_config};
use crate::types::error::ContextualError;
use crate::types::subgraph::Qualified;
use error_context::{Context, Step};
use open_dds::{
    arguments::ArgumentName,
    commands::{CommandName, FunctionName, ProcedureName},
    data_connector::DataConnectorName,
    spanned::Spanned,
    types::{CustomTypeName, TypeReference},
};
impl ContextualError for CommandsError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        match self {
            Self::DuplicateCommandArgumentDefinition {
                command_name,
                argument_name,
            } => Some(Context::from_step(Step {
                subgraph: Some(command_name.subgraph.clone()),
                path: argument_name.path.clone(),
                message: format!(
                    "Command '{}' has a duplicate argument definition for '{}'",
                    command_name.name, argument_name.value
                ),
            })),

            Self::UnknownCommandArgumentType {
                command_name,
                argument_name,
                argument_type,
            } => Some(
                Context::from_step(Step {
                    message: format!(
                        "Command '{}' has an argument '{}'",
                        command_name.name, argument_name.value
                    ),
                    path: argument_name.path.clone(),
                    subgraph: Some(command_name.subgraph.clone()),
                })
                .append(Step {
                    subgraph: Some(command_name.subgraph.clone()),
                    path: argument_name
                        .path
                        .clone()
                        .parent()
                        .append_key("type".into()),
                    message: format!("The argument type '{argument_type}' has not been defined",),
                }),
            ),

            _other => None,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CommandsError {
    #[error("the following command is defined more than once: {name:}")]
    DuplicateCommandDefinition { name: Qualified<CommandName> },
    #[error(
        "the argument '{argument_name}' in command '{command_name}' has an unknown type: {argument_type}"
    )]
    UnknownCommandArgumentType {
        command_name: Qualified<CommandName>,
        argument_name: Spanned<ArgumentName>,
        argument_type: TypeReference,
    },
    #[error(
        "the following argument in command {command_name:} is defined more than once: {argument_name:}"
    )]
    DuplicateCommandArgumentDefinition {
        command_name: Qualified<CommandName>,
        argument_name: Spanned<ArgumentName>,
    },
    #[error("source for the following command is defined more than once: {command_name:}")]
    DuplicateCommandSourceDefinition {
        command_name: Qualified<CommandName>,
    },
    #[error(
        "the source data connector {data_connector:} for command {command_name:} has not been defined"
    )]
    UnknownCommandDataConnector {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error(
        "the mapping for type {type_name:} in command {command_name:} is defined more than once"
    )]
    DuplicateTypeMappingDefinitionInCommandSource {
        command_name: Qualified<CommandName>,
        type_name: CustomTypeName,
    },
    #[error("command source is required for command '{command_name:}' to resolve argument preset")]
    CommandSourceRequiredForArgumentPreset {
        command_name: Qualified<CommandName>,
    },
    #[error(
        "a preset argument {argument_name:} has been set for the command {command_name:} but no such argument exists for this command"
    )]
    CommandArgumentPresetMismatch {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error(
        "the procedure {procedure:} in the data connector {data_connector:} for command {command_name:} has not been defined"
    )]
    UnknownCommandProcedure {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
        procedure: ProcedureName,
    },
    #[error(
        "the function {function:} in the data connector {data_connector:} for command {command_name:} has not been defined"
    )]
    UnknownCommandFunction {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
        function: FunctionName,
    },
    #[error(
        "An error occurred while mapping arguments in the command {command_name:} to the function {function_name:} in the data connector {data_connector_name:}: {error:}"
    )]
    CommandFunctionArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        function_name: FunctionName,
        error: ArgumentMappingError,
    },
    #[error(
        "An error occurred while mapping arguments in the command {command_name:} to the procedure {procedure_name:} in the data connector {data_connector_name:}: {error:}"
    )]
    CommandProcedureArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        procedure_name: ProcedureName,
        error: ArgumentMappingError,
    },
    #[error("{error:} in command {command_name:}")]
    CommandTypeMappingCollectionError {
        command_name: Qualified<CommandName>,
        error: TypeMappingCollectionError,
    },

    #[error("{0}")]
    DataConnectorError(#[from] data_connectors::NamedDataConnectorError),
    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),
    #[error("NDC validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
}
