//! IR and execution logic for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::TypeContainer;
use lang_graphql::ast::common::TypeName;
use lang_graphql::normalized_ast;

use metadata_resolve::CommandSource;
use open_dds::commands;
use open_dds::commands::FunctionName;
use open_dds::commands::ProcedureName;
use open_dds::types::{CustomTypeName, DataConnectorArgumentName};
use plan_types::VariableName;
use serde::Serialize;
use std::collections::BTreeMap;
use std::sync::Arc;

use super::arguments;
use super::selection_set;
use crate::error;
use crate::flags::GraphqlIrFlags;
use graphql_schema::GDS;
use graphql_schema::TypeKind;
use metadata_resolve::{ObjectTypeWithRelationships, Qualified, QualifiedTypeReference};
use plan::count_command;
use plan_types::UsagesCounts;

/// IR for the 'command' operations
#[derive(Serialize, Debug)]
pub struct CommandInfo {
    /// The name of the command
    pub command_name: Arc<Qualified<commands::CommandName>>,

    /// The name of the field as published in the schema
    pub field_name: ast::Name,

    /// The data connector backing this model.
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,

    /// IR for the command result selection set
    pub selection: open_dds::query::CommandSelection,

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
    pub command_info: CommandInfo,

    /// Source function in the data connector for this model
    pub function_name: &'s FunctionName,

    /// Variable arguments to be used for remote joins
    pub variable_arguments: BTreeMap<DataConnectorArgumentName, VariableName>,
}

/// IR for the 'procedure based command' operations
#[derive(Serialize, Debug)]
pub struct ProcedureBasedCommand<'s> {
    /// Info to generate command IR
    pub command_info: CommandInfo,

    /// Source procedure in the data connector for this model
    pub procedure_name: &'s ProcedureName,
}

/// Generates the OpenDD IR for a 'command' operation
#[allow(irrefutable_let_patterns)]
pub fn generate_command_info_open_dd<'n, 's>(
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    command_name: &Qualified<commands::CommandName>,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    result_type: &QualifiedTypeReference,
    result_base_type_kind: TypeKind,
    command_source: &'s CommandSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<CommandInfo, error::Error> {
    let mut command_arguments = IndexMap::new();
    for argument in field_call.arguments.values() {
        let (argument_name, argument_value) = arguments::build_argument_as_value(
            argument,
            &command_source.type_mappings,
            flags,
            usage_counts,
        )?;

        command_arguments.insert(argument_name, argument_value);
    }

    // Add the name of the root command
    let mut usage_counts = UsagesCounts::new();
    count_command(command_name, &mut usage_counts);

    let target = open_dds::query::CommandTarget {
        arguments: command_arguments,
        command_name: command_name.name.clone(),
        subgraph: command_name.subgraph.clone(),
    };

    let nested_selection = selection_set::generate_nested_selection_open_dd_ir(
        result_type,
        result_base_type_kind,
        metadata_resolve::FieldNestedness::NotNested,
        models,
        &command_source.type_mappings,
        object_types,
        selection_set::NestedSelectionType::CommandRootSelection,
        field,
        session_variables,
        request_headers,
        flags,
        &mut usage_counts,
    )?;

    let selection = open_dds::query::CommandSelection {
        selection: nested_selection,
        target,
    };

    Ok(CommandInfo {
        command_name: Arc::new(command_name.clone()),
        field_name: field_call.name.clone(),
        data_connector: command_source.data_connector.clone(),
        selection,
        type_container: field.type_container.clone(),
        usage_counts,
    })
}

/// Generates the OpenDD IR for a 'function based command' operation
pub fn generate_function_based_command_open_dd<'n, 's>(
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    command_name: &Qualified<commands::CommandName>,
    function_name: &'s open_dds::commands::FunctionName,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    result_type: &QualifiedTypeReference,
    result_base_type_kind: TypeKind,
    command_source: &'s CommandSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<FunctionBasedCommand<'s>, error::Error> {
    let command_info = generate_command_info_open_dd(
        models,
        object_types,
        command_name,
        field,
        field_call,
        result_type,
        result_base_type_kind,
        command_source,
        session_variables,
        request_headers,
        flags,
        usage_counts,
    )?;

    Ok(FunctionBasedCommand {
        command_info,
        function_name,
        variable_arguments: BTreeMap::new(),
    })
}

/// Generates the OpenDD IR for a 'procedure based command' operation
pub fn generate_procedure_based_command_open_dd<'n, 's>(
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    command_name: &Qualified<commands::CommandName>,
    procedure_name: &'s open_dds::commands::ProcedureName,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    result_type: &QualifiedTypeReference,
    result_base_type_kind: TypeKind,
    command_source: &'s CommandSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
) -> Result<ProcedureBasedCommand<'s>, error::Error> {
    let mut usage_counts = UsagesCounts::new();

    let command_info = generate_command_info_open_dd(
        models,
        object_types,
        command_name,
        field,
        field_call,
        result_type,
        result_base_type_kind,
        command_source,
        session_variables,
        request_headers,
        flags,
        &mut usage_counts,
    )?;

    Ok(ProcedureBasedCommand {
        command_info,
        procedure_name,
    })
}
