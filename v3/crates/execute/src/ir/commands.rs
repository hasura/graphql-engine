//! IR and execution logic for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::TypeContainer;
use lang_graphql::ast::common::TypeName;
use lang_graphql::normalized_ast;

use nonempty::NonEmpty;
use open_dds::commands;
use open_dds::commands::FunctionName;
use open_dds::commands::ProcedureName;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::types::DataConnectorArgumentName;
use serde::Serialize;
use std::collections::BTreeMap;

use super::arguments;
use super::error::InternalDeveloperError;
use super::selection_set;
use super::selection_set::FieldSelection;
use super::selection_set::NestedSelection;
use super::selection_set::ResultSelectionSet;
use crate::ir::error;
use crate::ir::permissions;
use crate::model_tracking::{count_command, UsagesCounts};
use metadata_resolve;
use metadata_resolve::http::SerializableHeaderMap;
use metadata_resolve::{Qualified, QualifiedTypeReference};
use schema::ArgumentNameAndPath;
use schema::ArgumentPresets;
use schema::CommandSourceDetail;
use schema::TypeKind;
use schema::GDS;

/// IR for the 'command' operations
#[derive(Serialize, Debug)]
pub struct CommandInfo<'s> {
    /// The name of the command
    pub command_name: Qualified<commands::CommandName>,

    /// The name of the field as published in the schema
    pub field_name: ast::Name,

    /// The data connector backing this model.
    pub data_connector: &'s metadata_resolve::DataConnectorLink,

    /// Arguments for the NDC table
    pub(crate) arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument>,

    /// IR for the command result selection set
    pub(crate) selection: Option<selection_set::NestedSelection<'s>>,

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
pub(crate) fn generate_command_info<'n, 's>(
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
        let (ndc_arg_name, ndc_val) = arguments::build_ndc_command_arguments_as_value(
            &field_call.name,
            argument,
            &command_source.type_mappings,
        )?;
        command_arguments.insert(
            ndc_arg_name,
            arguments::Argument::Literal { value: ndc_val },
        );
    }

    // preset arguments from permissions presets (both command permission argument
    // presets and input field presets)
    if let Some(ArgumentPresets { argument_presets }) =
        permissions::get_argument_presets(field_call.info.namespaced)?
    {
        // add any preset arguments from command permissions or input type permissions
        for (argument_name_and_path, (field_type, argument_value)) in argument_presets {
            let ArgumentNameAndPath {
                field_path,
                ndc_argument_name,
            } = argument_name_and_path;

            let argument_name = ndc_argument_name.as_ref().ok_or_else(|| {
                // this can only happen when no argument mapping was not found
                // during annotation generation
                error::InternalEngineError::ArgumentPresetExecution {
                    description: "unexpected; ndc argument name not preset".to_string(),
                }
            })?;

            let actual_value = permissions::make_argument_from_value_expression_or_predicate(
                argument_value,
                field_type,
                session_variables,
                usage_counts,
            )?;

            match NonEmpty::from_slice(field_path) {
                // if field path is empty, then the entire argument has to preset
                None => {
                    command_arguments.insert(argument_name.clone(), actual_value);
                }
                // if there is some field path, preset the argument partially based on the field path
                Some(field_path) => {
                    if let Some(current_arg) = command_arguments.get_mut(argument_name) {
                        let current_arg = match current_arg {
                            arguments::Argument::Literal { value } => Ok(value),
                            arguments::Argument::BooleanExpression { predicate: _ } => {
                                Err(error::InternalEngineError::ArgumentPresetExecution {
                                    description: "unexpected; can't merge an argument preset into an argument that has a boolean expression value"
                                        .to_owned(),
                                })
                            }
                        }?;
                        let preset_value = match actual_value {
                            arguments::Argument::Literal { value } => Ok(value),
                            arguments::Argument::BooleanExpression { predicate: _ } => {
                                // See schema::Error::BooleanExpressionInTypePresetArgument
                                Err(error::InternalEngineError::ArgumentPresetExecution {
                                    description: "unexpected; type input presets cannot contain a boolean expression preset value"
                                        .to_owned(),
                                })
                            }
                        }?;
                        if let Some(current_arg_object) = current_arg.as_object_mut() {
                            arguments::follow_field_path_and_insert_value(
                                &field_path,
                                current_arg_object,
                                preset_value,
                            )?;
                        }
                    }
                }
            }
        }
    }

    // preset arguments from `DataConnectorLink` argument presets
    for dc_argument_preset in &command_source.data_connector.argument_presets {
        let mut headers_argument = reqwest::header::HeaderMap::new();

        // add headers from the request to be forwarded
        for header_name in &dc_argument_preset.value.http_headers.forward {
            if let Some(header_value) = request_headers.get(&header_name.0) {
                headers_argument.insert(header_name.0.clone(), header_value.clone());
            }
        }

        // add additional headers from `ValueExpression`
        for (header_name, value_expression) in &dc_argument_preset.value.http_headers.additional {
            // TODO: have helper functions to create types
            let string_type = QualifiedTypeReference {
                nullable: false,
                underlying_type: metadata_resolve::QualifiedBaseType::Named(
                    metadata_resolve::QualifiedTypeName::Inbuilt(
                        open_dds::types::InbuiltType::String,
                    ),
                ),
            };
            let value = permissions::make_argument_from_value_expression(
                value_expression,
                &string_type,
                session_variables,
            )?;
            let header_value =
                reqwest::header::HeaderValue::from_str(serde_json::to_string(&value)?.as_str())
                    .map_err(|_e| {
                        InternalDeveloperError::UnableToConvertValueExpressionToHeaderValue
                    })?;
            headers_argument.insert(header_name.0.clone(), header_value);
        }

        command_arguments.insert(
            DataConnectorArgumentName::from(dc_argument_preset.name.as_str()),
            arguments::Argument::Literal {
                value: serde_json::to_value(SerializableHeaderMap(headers_argument))?,
            },
        );
    }

    // Add the name of the root command
    let mut usage_counts = UsagesCounts::new();
    count_command(command_name, &mut usage_counts);

    let selection = selection_set::generate_nested_selection(
        result_type,
        result_base_type_kind,
        field,
        &command_source.data_connector,
        &command_source.type_mappings,
        session_variables,
        request_headers,
        &mut usage_counts,
    )?;
    let selection = wrap_selection_in_response_config(command_source, selection);

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

/// Wrap a selection set in a `{"headers": ..., "response": ...}` selection
/// shape for command selections, where `CommandsResponseConfig` is configured.
///
/// When the output type of a NDC function/procedure is an object type,
/// containing headers and response fields; and the response field is also an
/// object type -
/// 1. Engine needs to generate fields selection IR such that it contains
/// `{"headers": ..., "response": ...}` shape, and the actual selection from the
/// user-facing query goes inside the `response` field
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
                let headers_field_name = response_config.headers_field.as_str().to_owned();
                let headers_field = FieldSelection::Column {
                    column: DataConnectorColumnName::from(headers_field_name.as_str()),
                    nested_selection: None,
                    arguments: BTreeMap::new(),
                };
                let result_field_name = response_config.result_field.as_str().to_owned();
                let result_field = FieldSelection::Column {
                    column: DataConnectorColumnName::from(result_field_name.as_str()),
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
pub(crate) fn generate_function_based_command<'n, 's>(
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
pub(crate) fn generate_procedure_based_command<'n, 's>(
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
