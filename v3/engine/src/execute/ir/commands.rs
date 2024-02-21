//! IR and execution logic for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.

use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::TypeContainer;
use lang_graphql::ast::common::TypeName;
use lang_graphql::normalized_ast;
use open_dds::commands;
use open_dds::commands::FunctionName;
use open_dds::commands::ProcedureName;
use serde::Serialize;
use serde_json as json;
use std::collections::BTreeMap;

use super::arguments;
use super::selection_set;
use crate::execute::error;
use crate::execute::model_tracking::{count_command, UsagesCounts};
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph;
use crate::schema::types::CommandSourceDetail;
use crate::schema::GDS;

/// IR for the 'command' operations
#[derive(Serialize, Debug)]
pub struct CommandInfo<'s> {
    /// The name of the command
    pub command_name: subgraph::Qualified<commands::CommandName>,

    /// The name of the field as published in the schema
    pub field_name: ast::Name,

    /// The data connector backing this model.
    pub data_connector: &'s resolved::data_connector::DataConnectorLink,

    /// Arguments for the NDC table
    pub(crate) arguments: BTreeMap<String, json::Value>,

    /// IR for the command result selection set
    pub(crate) selection: selection_set::ResultSelectionSet<'s>,

    /// The Graphql base type for the output_type of command. Helps in deciding how
    /// the response from the NDC needs to be processed.
    pub type_container: TypeContainer<TypeName>,

    // All the models/commands used in the 'command' operation.
    pub(crate) usage_counts: UsagesCounts,
}

/// IR for the 'function based command' operations
#[derive(Serialize, Debug)]
pub struct FunctionBasedCommand<'s> {
    /// Info to generate command IR
    pub command_info: CommandInfo<'s>,

    /// Source function in the data connector for this model
    pub function_name: &'s FunctionName,

    /// Variable arguments to be used for remote joins
    pub variable_arguments: BTreeMap<String, String>,
}

/// IR for the 'procedure based command' operations
#[derive(Serialize, Debug)]
pub struct ProcedureBasedCommand<'s> {
    /// Info to generate command IR
    pub command_info: CommandInfo<'s>,

    /// Source procedure in the data connector for this model
    pub procedure_name: &'s ProcedureName,
}

/// Generates the IR for a 'command' operation
#[allow(irrefutable_let_patterns)]
pub(crate) fn generate_command_info<'n, 's>(
    command_name: &subgraph::Qualified<commands::CommandName>,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    underlying_object_typename: &Option<subgraph::Qualified<open_dds::types::CustomTypeName>>,
    command_source: &'s CommandSourceDetail,
    session_variables: &SessionVariables,
) -> Result<CommandInfo<'s>, error::Error> {
    let empty_field_mappings = BTreeMap::new();
    // No field mappings should exists if the resolved output type of command is
    // not a custom object type
    let field_mappings = match underlying_object_typename {
        None => &empty_field_mappings,
        Some(typename) => command_source
            .type_mappings
            .get(typename)
            .and_then(|type_mapping| {
                if let resolved::types::TypeMapping::Object { field_mappings } = type_mapping {
                    Some(field_mappings)
                } else {
                    None
                }
            })
            .ok_or_else(|| error::InternalEngineError::InternalGeneric {
                description: format!(
                    "type '{}' not found in command source type_mappings",
                    typename
                ),
            })?,
    };

    let mut command_arguments = BTreeMap::new();
    for argument in field_call.arguments.values() {
        command_arguments.extend(
            arguments::build_ndc_command_arguments_as_value(
                &field_call.name,
                argument,
                &command_source.type_mappings,
            )?
            .into_iter(),
        );
    }

    // Add the name of the root command
    let mut usage_counts = UsagesCounts::new();
    count_command(command_name.clone(), &mut usage_counts);

    let selection = selection_set::generate_selection_set_ir(
        &field.selection_set,
        &command_source.data_connector,
        &command_source.type_mappings,
        field_mappings,
        session_variables,
        &mut usage_counts,
    )?;

    Ok(CommandInfo {
        command_name: command_name.clone(),
        field_name: field_call.name.clone(),
        data_connector: &command_source.data_connector,
        arguments: command_arguments,
        selection,
        type_container: field.type_container.clone(),
        usage_counts,
    })
}

/// Generates the IR for a 'function based command' operation
pub(crate) fn generate_function_based_command<'n, 's>(
    command_name: &subgraph::Qualified<commands::CommandName>,
    function_name: &'s open_dds::commands::FunctionName,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    underlying_object_typename: &Option<subgraph::Qualified<open_dds::types::CustomTypeName>>,
    command_source: &'s CommandSourceDetail,
    session_variables: &SessionVariables,
) -> Result<FunctionBasedCommand<'s>, error::Error> {
    let command_info = generate_command_info(
        command_name,
        field,
        field_call,
        underlying_object_typename,
        command_source,
        session_variables,
    )?;

    Ok(FunctionBasedCommand {
        command_info,
        function_name,
        variable_arguments: BTreeMap::new(),
    })
}

/// Generates the IR for a 'procedure based command' operation
pub(crate) fn generate_procedure_based_command<'n, 's>(
    command_name: &subgraph::Qualified<commands::CommandName>,
    procedure_name: &'s open_dds::commands::ProcedureName,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    underlying_object_typename: &Option<subgraph::Qualified<open_dds::types::CustomTypeName>>,
    command_source: &'s CommandSourceDetail,
    session_variables: &SessionVariables,
) -> Result<ProcedureBasedCommand<'s>, error::Error> {
    let command_info = generate_command_info(
        command_name,
        field,
        field_call,
        underlying_object_typename,
        command_source,
        session_variables,
    )?;

    Ok(ProcedureBasedCommand {
        command_info,
        procedure_name,
    })
}
