//! IR and execution logic for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::TypeContainer;
use lang_graphql::ast::common::TypeName;
use lang_graphql::normalized_ast;

use open_dds::commands;
use open_dds::commands::FunctionName;
use open_dds::commands::ProcedureName;
use open_dds::types::DataConnectorArgumentName;
use serde::Serialize;
use std::collections::BTreeMap;
use std::sync::Arc;

use super::arguments;
use super::selection_set;
use super::selection_set::FieldSelection;
use super::selection_set::NestedSelection;
use super::selection_set::ResultSelectionSet;
use crate::error;
use crate::model_tracking::count_command;
use crate::permissions;
use graphql_schema::CommandSourceDetail;
use graphql_schema::TypeKind;
use graphql_schema::GDS;
use metadata_resolve::{Qualified, QualifiedTypeReference};
use plan_types::NdcFieldAlias;
use plan_types::UsagesCounts;

/// IR for the 'command' operations
#[derive(Serialize, Debug)]
pub struct CommandInfo<'s> {
    /// The name of the command
    pub command_name: Arc<Qualified<commands::CommandName>>,

    /// The name of the field as published in the schema
    pub field_name: ast::Name,

    /// The data connector backing this model.
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,

    /// Arguments for the NDC table
    pub arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,

    /// IR for the command result selection set
    pub selection: Option<selection_set::NestedSelection<'s>>,

    /// The Graphql base type for the output_type of command. Helps in deciding how
    /// the response from the NDC needs to be processed.
    pub type_container: TypeContainer<TypeName>,

    // All the models/commands used in the 'command' operation.
    pub usage_counts: UsagesCounts,
}

/// IR for the 'function based command' operations
#[derive(Serialize, Debug)]
pub struct FunctionBasedCommand<'s> {
    /// Info to generate command IR
    pub command_info: CommandInfo<'s>,

    /// Source function in the data connector for this model
    pub function_name: &'s FunctionName,

    /// Variable arguments to be used for remote joins
    pub variable_arguments: BTreeMap<DataConnectorArgumentName, String>,
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
pub fn generate_command_info<'n, 's>(
    command_name: &Qualified<commands::CommandName>,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    result_type: &QualifiedTypeReference,
    result_base_type_kind: TypeKind,
    command_source: &'s CommandSourceDetail,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<CommandInfo<'s>, error::Error> {
    let mut command_arguments = BTreeMap::new();

    for argument in field_call.arguments.values() {
        let (ndc_arg_name, ndc_val) = arguments::build_ndc_argument_as_value(
            &field_call.name,
            argument,
            &command_source.type_mappings,
            &command_source.data_connector,
            session_variables,
            usage_counts,
        )?;

        command_arguments.insert(ndc_arg_name, ndc_val);
    }

    let command_argument_presets =
        permissions::get_argument_presets(field_call.info.namespaced.as_ref())?;

    // preset arguments from permissions presets (both command permission argument
    // presets and input field presets)
    command_arguments = arguments::process_argument_presets(
        &command_source.data_connector,
        &command_source.type_mappings,
        command_argument_presets,
        &command_source.data_connector_link_argument_presets,
        session_variables,
        request_headers,
        command_arguments,
        usage_counts,
    )?;

    // Add the name of the root command
    let mut usage_counts = UsagesCounts::new();
    count_command(command_name, &mut usage_counts);

    let selection = selection_set::generate_nested_selection(
        result_type,
        result_base_type_kind,
        metadata_resolve::FieldNestedness::NotNested,
        selection_set::NestedSelectionType::CommandRootSelection,
        field,
        &command_source.data_connector,
        &command_source.type_mappings,
        session_variables,
        request_headers,
        &mut usage_counts,
    )?;
    let selection = wrap_selection_in_response_config(command_source, selection);

    Ok(CommandInfo {
        command_name: Arc::new(command_name.clone()),
        field_name: field_call.name.clone(),
        data_connector: command_source.data_connector.clone(),
        arguments: command_arguments,
        selection,
        type_container: field.type_container.clone(),
        usage_counts,
    })
}

/// Wrap a selection set in a `{"headers": ..., "response": ...}` selection
/// shape for command selections, where `CommandsResponseConfig` is configured.
///
/// When the output type of a NDC function/procedure is an object type,
/// containing headers and response fields; and the response field is also an
/// object type -
/// 1. Engine needs to generate fields selection IR such that it contains
///    `{"headers": ..., "response": ...}` shape, and the actual selection from the
///    user-facing query goes inside the `response` field
fn wrap_selection_in_response_config<'a>(
    command_source: &CommandSourceDetail,
    original_selection: Option<NestedSelection<'a>>,
) -> Option<NestedSelection<'a>> {
    match &command_source.data_connector.response_config {
        None => original_selection,
        Some(response_config) => {
            if command_source.ndc_type_opendd_type_same {
                original_selection
            } else {
                let headers_field_name =
                    NdcFieldAlias::from(response_config.headers_field.as_str());
                let headers_field = FieldSelection::Column {
                    column: response_config.headers_field.clone(),
                    nested_selection: None,
                    arguments: BTreeMap::new(),
                };
                let result_field_name = NdcFieldAlias::from(response_config.result_field.as_str());
                let result_field = FieldSelection::Column {
                    column: response_config.result_field.clone(),
                    nested_selection: original_selection,
                    arguments: BTreeMap::new(),
                };
                let fields = IndexMap::from_iter([
                    (headers_field_name, headers_field),
                    (result_field_name, result_field),
                ]);
                Some(NestedSelection::Object(ResultSelectionSet { fields }))
            }
        }
    }
}

/// Generates the IR for a 'function based command' operation
pub fn generate_function_based_command<'n, 's>(
    command_name: &Qualified<commands::CommandName>,
    function_name: &'s open_dds::commands::FunctionName,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    result_type: &QualifiedTypeReference,
    result_base_type_kind: TypeKind,
    command_source: &'s CommandSourceDetail,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FunctionBasedCommand<'s>, error::Error> {
    let command_info = generate_command_info(
        command_name,
        field,
        field_call,
        result_type,
        result_base_type_kind,
        command_source,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    Ok(FunctionBasedCommand {
        command_info,
        function_name,
        variable_arguments: BTreeMap::new(),
    })
}

/// Generates the IR for a 'procedure based command' operation
pub fn generate_procedure_based_command<'n, 's>(
    command_name: &Qualified<commands::CommandName>,
    procedure_name: &'s open_dds::commands::ProcedureName,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    result_type: &QualifiedTypeReference,
    result_base_type_kind: TypeKind,
    command_source: &'s CommandSourceDetail,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<ProcedureBasedCommand<'s>, error::Error> {
    let mut usage_counts = UsagesCounts::new();

    let command_info = generate_command_info(
        command_name,
        field,
        field_call,
        result_type,
        result_base_type_kind,
        command_source,
        session_variables,
        request_headers,
        &mut usage_counts,
    )?;

    Ok(ProcedureBasedCommand {
        command_info,
        procedure_name,
    })
}
