//! IR and execution logic for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.

use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::TypeContainer;
use lang_graphql::ast::common::TypeName;
use lang_graphql::normalized_ast;
use ndc_client as ndc;
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
use crate::execute::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};
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
    pub data_connector: &'s resolved::data_connector::DataConnector,

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

pub fn ir_to_ndc_query<'s>(
    ir: &CommandInfo<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::models::Query, JoinLocations<RemoteJoin<'s>>), error::Error> {
    let (ndc_fields, jl) = selection_set::process_selection_set_ir(&ir.selection, join_id_counter)?;
    let query = ndc::models::Query {
        aggregates: None,
        fields: Some(ndc_fields),
        limit: None,
        offset: None,
        order_by: None,
        predicate: None,
    };
    Ok((query, jl))
}

pub fn ir_to_ndc_query_ir<'s>(
    ir: &FunctionBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::models::QueryRequest, JoinLocations<RemoteJoin<'s>>), error::Error> {
    let mut arguments: BTreeMap<String, ndc_client::models::Argument> = ir
        .command_info
        .arguments
        .iter()
        .map(|(argument_name, argument_value)| {
            (
                argument_name.clone(),
                ndc_client::models::Argument::Literal {
                    value: argument_value.clone(),
                },
            )
        })
        .collect();

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in ir.variable_arguments.iter() {
        arguments.insert(
            variable_name.clone(),
            ndc_client::models::Argument::Variable {
                name: variable_argument.clone(),
            },
        );
    }

    let (query, jl) = ir_to_ndc_query(&ir.command_info, join_id_counter)?;
    let mut collection_relationships = BTreeMap::new();
    selection_set::collect_relationships(
        &ir.command_info.selection,
        &mut collection_relationships,
    )?;
    let query_request = ndc::models::QueryRequest {
        query,
        collection: ir.function_name.to_string(),
        arguments,
        collection_relationships,
        variables: None,
    };
    Ok((query_request, jl))
}

pub fn ir_to_ndc_mutation_ir<'s>(
    procedure_name: &ProcedureName,
    ir: &ProcedureBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::models::MutationRequest, JoinLocations<RemoteJoin<'s>>), error::Error> {
    let (ndc_fields, jl) =
        selection_set::process_selection_set_ir(&ir.command_info.selection, join_id_counter)?;
    let mutation_operation = ndc::models::MutationOperation::Procedure {
        name: procedure_name.to_string(),
        arguments: ir.command_info.arguments.clone(),
        fields: Some(ndc_fields),
    };
    let mut collection_relationships = BTreeMap::new();
    selection_set::collect_relationships(
        &ir.command_info.selection,
        &mut collection_relationships,
    )?;
    let mutation_request = ndc::models::MutationRequest {
        operations: vec![mutation_operation],
        collection_relationships,
    };
    Ok((mutation_request, jl))
}
