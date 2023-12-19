//! Schema for commands
//!
//! A 'command' executes a function/procedure and returns back the result of the execution.

use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use lang_graphql::schema::InputField;
use lang_graphql::schema::Namespaced;
use ndc_client as gdc;
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

pub(crate) fn command_field(
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
    let output_typename = get_output_type(gds, builder, &command.output_type)?;

    let mut arguments = HashMap::new();
    for (argument_name, argument_type) in &command.arguments {
        let field_name = ast::Name::new(argument_name.0.as_str())?;
        let input_type = types::input_type::get_input_type(gds, builder, argument_type)?;
        let input_field: Namespaced<GDS, InputField<GDS>> = builder.allow_all_namespaced(
            gql_schema::InputField::new(
                field_name.clone(),
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
        );
        arguments.insert(field_name, input_field);
    }

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            command_field_name.clone(),
            None,
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::Command {
                    name: command.name.clone(),
                    source: command.source.clone(),
                    underlying_object_typename: command.underlying_object_typename.clone(),
                },
            )),
            output_typename,
            arguments,
            gql_schema::DeprecationStatus::NotDeprecated,
        ),
        permissions::get_command_namespace_annotations(command),
    );
    Ok((command_field_name, field))
}
