use crate::helpers::{
    argument::ArgumentMappingError, ndc_validation::NDCValidationError,
    type_mappings::TypeMappingCollectionError,
};
use crate::stages::{data_connectors, graphql_config};
use crate::types::subgraph::Qualified;
use open_dds::{
    arguments::ArgumentName,
    commands::{CommandName, FunctionName, ProcedureName},
    data_connector::DataConnectorName,
    types::{CustomTypeName, TypeReference},
};

#[derive(Debug, thiserror::Error)]
pub enum CommandsError {
    #[error("the following command is defined more than once: {name:}")]
    DuplicateCommandDefinition { name: Qualified<CommandName> },
    #[error("the following argument {argument_name:} with argument type {argument_type:} in command {command_name:} ) has not been defined")]
    UnknownCommandArgumentType {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
        argument_type: TypeReference,
    },
    #[error(
        "the following argument in command {command_name:} is defined more than once: {argument_name:}"
    )]
    DuplicateCommandArgumentDefinition {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("source for the following command is defined more than once: {command_name:}")]
    DuplicateCommandSourceDefinition {
        command_name: Qualified<CommandName>,
    },
    #[error("the source data connector {data_connector:} for command {command_name:} has not been defined")]
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
    #[error(
        "unknown argument {argument_name:} referenced in argument mappings for command {command_name:}"
    )]
    UnknownCommandSourceArgument {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("command source is required for command '{command_name:}' to resolve predicate")]
    CommandSourceRequiredForPredicate {
        command_name: Qualified<CommandName>,
    },
    #[error(
        "the mapping for argument {argument_name:} of command {command_name:} has been defined more than once"
    )]
    DuplicateCommandArgumentMapping {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("a preset argument {argument_name:} has been set for the command {command_name:} but no such argument exists for this command")]
    CommandArgumentPresetMismatch {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("the procedure {procedure:} in the data connector {data_connector:} for command {command_name:} has not been defined")]
    UnknownCommandProcedure {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
        procedure: ProcedureName,
    },
    #[error("the function {function:} in the data connector {data_connector:} for command {command_name:} has not been defined")]
    UnknownCommandFunction {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
        function: FunctionName,
    },
    #[error("An error occurred while mapping arguments in the command {command_name:} to the function {function_name:} in the data connector {data_connector_name:}: {error:}")]
    CommandFunctionArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        function_name: FunctionName,
        error: ArgumentMappingError,
    },
    #[error("An error occurred while mapping arguments in the command {command_name:} to the procedure {procedure_name:} in the data connector {data_connector_name:}: {error:}")]
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
