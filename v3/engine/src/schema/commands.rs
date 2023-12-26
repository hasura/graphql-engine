//! Schema for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.

use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use lang_graphql::schema::InputField;
use lang_graphql::schema::Namespaced;
use ndc_client as gdc;
use open_dds::arguments::ArgumentName;
use open_dds::commands::DataConnectorCommand;
use std::collections::HashMap;

use crate::metadata::resolved;
use crate::schema::permissions;
use crate::schema::types::{self, output_type::get_output_type, Annotation};
use crate::schema::GDS;

pub enum Response {
    QueryResponse {
        response: gdc::models::QueryResponse,
    },
    MutationResponse {
        response: gdc::models::MutationResponse,
    },
}

pub(crate) fn generate_command_argument(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    argument_name: &ArgumentName,
    argument_type: &crate::schema::commands::resolved::subgraph::QualifiedTypeReference,
) -> Result<(ast::Name, Namespaced<GDS, InputField<GDS>>), crate::schema::Error> {
    let field_name = ast::Name::new(argument_name.0.as_str())?;
    let input_type = types::input_type::get_input_type(gds, builder, argument_type)?;
    Ok((
        field_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::InputField::new(
                field_name,
                None,
                Annotation::Input(types::InputAnnotation::CommandArgument {
                    argument_type: argument_type.clone(),
                    ndc_func_proc_argument: command
                        .source
                        .as_ref()
                        .and_then(|command_source| {
                            command_source.argument_mappings.get(argument_name)
                        })
                        .cloned(),
                }),
                input_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            None,
        ),
    ))
}

pub(crate) fn command_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    command_field_name: ast::Name,
    command_annotation: Annotation,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::schema::Error,
> {
    let output_typename = get_output_type(gds, builder, &command.output_type)?;

    let mut arguments = HashMap::new();
    for (argument_name, argument_type) in &command.arguments {
        let (field_name, input_field) =
            generate_command_argument(gds, builder, command, argument_name, argument_type)?;
        arguments.insert(field_name, input_field);
    }

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            command_field_name.clone(),
            None,
            command_annotation,
            output_typename,
            arguments,
            gql_schema::DeprecationStatus::NotDeprecated,
        ),
        permissions::get_command_namespace_annotations(command),
    );
    Ok((command_field_name, field))
}

pub(crate) fn function_command_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    command_field_name: ast::Name,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::schema::Error,
> {
    let (command_source_detail, function_name) = match &command.source {
        Some(command_source) => {
            let command_source_detail = types::CommandSourceDetail {
                data_connector: command_source.data_connector.clone(),
                type_mappings: command_source.type_mappings.clone(),
                argument_mappings: command_source.argument_mappings.clone(),
            };
            let function_name = match &command_source.source {
                DataConnectorCommand::Function(function_name) => function_name.clone(),
                _ => {
                    return Err(crate::schema::Error::IncorrectCommandBacking {
                        command_name: command.name.clone(),
                    })
                }
            };
            (Some(command_source_detail), Some(function_name))
        }
        None => (None, None),
    };

    let command_annotation = Annotation::Output(types::OutputAnnotation::RootField(
        types::RootFieldAnnotation::FunctionCommand {
            name: command.name.clone(),
            underlying_object_typename: command.underlying_object_typename.clone(),
            source: command_source_detail,
            function_name,
        },
    ));

    command_field(
        gds,
        builder,
        command,
        command_field_name,
        command_annotation,
    )
}

pub(crate) fn procedure_command_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    command_field_name: ast::Name,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::schema::Error,
> {
    let (command_source_detail, procedure_name) = match &command.source {
        Some(command_source) => {
            let command_source_detail = types::CommandSourceDetail {
                data_connector: command_source.data_connector.clone(),
                type_mappings: command_source.type_mappings.clone(),
                argument_mappings: command_source.argument_mappings.clone(),
            };
            let procedure_name = match &command_source.source {
                DataConnectorCommand::Procedure(procedure_name) => procedure_name.clone(),
                _ => {
                    return Err(crate::schema::Error::IncorrectCommandBacking {
                        command_name: command.name.clone(),
                    })
                }
            };
            (Some(command_source_detail), Some(procedure_name))
        }
        None => (None, None),
    };

    let command_annotation = Annotation::Output(types::OutputAnnotation::RootField(
        types::RootFieldAnnotation::ProcedureCommand {
            name: command.name.clone(),
            underlying_object_typename: command.underlying_object_typename.clone(),
            source: command_source_detail,
            procedure_name,
        },
    ));

    command_field(
        gds,
        builder,
        command,
        command_field_name,
        command_annotation,
    )
}
