//! Schema for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.

use crate::metadata::resolved;
use crate::schema::permissions;
use crate::schema::types::{self, output_type::get_output_type, Annotation};
use crate::schema::GDS;
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use lang_graphql::schema::InputField;
use lang_graphql::schema::Namespaced;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::commands::DataConnectorCommand;

use std::collections::{BTreeMap, HashMap};

use super::types::output_type::get_type_kind;

pub enum Response {
    QueryResponse {
        response: ndc_models::QueryResponse,
    },
    MutationResponse {
        response: ndc_models::MutationResponse,
    },
}

// look at the permissions and remove arguments with presets for this role
pub(crate) fn generate_command_argument(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    argument_name: &ArgumentName,
    argument_type: &crate::schema::commands::resolved::subgraph::ArgumentInfo,
) -> Result<(ast::Name, Namespaced<GDS, InputField<GDS>>), crate::schema::Error> {
    let field_name = ast::Name::new(argument_name.0.as_str())?;
    let input_type = types::input_type::get_input_type(gds, builder, &argument_type.argument_type)?;

    let input_field = gql_schema::InputField::new(
        field_name.clone(),
        argument_type.description.clone(),
        Annotation::Input(types::InputAnnotation::CommandArgument {
            argument_type: argument_type.argument_type.clone(),
            ndc_func_proc_argument: command
                .source
                .as_ref()
                .and_then(|command_source| command_source.argument_mappings.get(argument_name))
                .cloned(),
        }),
        input_type,
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    );

    // a role is "allowed" to use this argument if it DOESN'T have a preset argument defined
    match &command.permissions {
        // if this command has any permissions, we must assume it has them setup for every role
        // that is interested
        Some(permissions_by_namespace) => {
            let mut namespaced_annotations = HashMap::new();

            for (namespace, permission) in permissions_by_namespace {
                // if there is a preset for this argument, remove it from the schema
                // so the user cannot provide one
                if !permission.argument_presets.contains_key(argument_name) {
                    namespaced_annotations.insert(namespace.clone(), None);
                }
            }

            Ok((
                field_name,
                builder.conditional_namespaced(input_field, namespaced_annotations),
            ))
        }
        // if there are no permissions for this command, there are no presets so we assume all
        // arguments are OK to use
        None => Ok((field_name, builder.allow_all_namespaced(input_field, None))),
    }
}

pub(crate) fn command_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    command_field_name: ast::Name,
    command_annotation: Annotation,
    deprecation_status: gql_schema::DeprecationStatus,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::schema::Error,
> {
    let output_typename = get_output_type(gds, builder, &command.output_type)?;

    let mut arguments = BTreeMap::new();

    for (argument_name, argument_type) in &command.arguments {
        let (field_name, input_field) =
            generate_command_argument(gds, builder, command, argument_name, argument_type)?;
        arguments.insert(field_name, input_field);
    }
    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            command_field_name.clone(),
            command.description.clone(),
            command_annotation,
            output_typename,
            arguments,
            deprecation_status,
        ),
        permissions::get_command_namespace_annotations(command, &gds.metadata.object_types)?,
    );
    Ok((command_field_name, field))
}

pub(crate) fn function_command_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    command_field_name: ast::Name,
    deprecation_status: gql_schema::DeprecationStatus,
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
            result_type: command.output_type.clone(),
            result_base_type_kind: get_type_kind(gds, &command.output_type)?,
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
        deprecation_status,
    )
}

pub(crate) fn procedure_command_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    command: &resolved::command::Command,
    command_field_name: ast::Name,
    deprecation_status: gql_schema::DeprecationStatus,
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
            result_type: command.output_type.clone(),
            result_base_type_kind: get_type_kind(gds, &command.output_type)?,
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
        deprecation_status,
    )
}
