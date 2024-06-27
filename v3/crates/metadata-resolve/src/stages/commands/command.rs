use crate::helpers::types::{get_type_representation, mk_name};
use crate::stages::{
    boolean_expressions, object_boolean_expressions, scalar_types, type_permissions,
};
use crate::types::error::Error;
use crate::types::subgraph::{mk_qualified_type_reference, ArgumentInfo, Qualified};
use indexmap::IndexMap;

use super::types::{Command, CommandGraphQlApi};
use open_dds::commands::CommandV1;

use open_dds::types::{BaseType, CustomTypeName, TypeName, TypeReference};

use std::collections::BTreeMap;

pub fn resolve_command(
    command: &CommandV1,
    subgraph: &str,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> Result<Command, Error> {
    let mut arguments = IndexMap::new();
    let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
    let command_description = command.description.clone();
    // duplicate command arguments should not be allowed
    for argument in &command.arguments {
        if type_exists(
            &argument.argument_type,
            subgraph,
            object_types,
            scalar_types,
            object_boolean_expression_types,
            boolean_expression_types,
        ) {
            if arguments
                .insert(
                    argument.name.clone(),
                    ArgumentInfo {
                        argument_type: mk_qualified_type_reference(
                            &argument.argument_type,
                            subgraph,
                        ),
                        description: argument.description.clone(),
                    },
                )
                .is_some()
            {
                return Err(Error::DuplicateCommandArgumentDefinition {
                    command_name: qualified_command_name,
                    argument_name: argument.name.clone(),
                });
            }
        } else {
            return Err(Error::UnknownCommandArgumentType {
                command_name: qualified_command_name,
                argument_name: argument.name.clone(),
                argument_type: argument.argument_type.clone(),
            });
        }
    }

    let graphql_api = match &command.graphql {
        None => Ok(None),
        Some(graphql_definition) => {
            mk_name(graphql_definition.root_field_name.0.as_ref()).map(|f| {
                Some(CommandGraphQlApi {
                    root_field_kind: graphql_definition.root_field_kind.clone(),
                    root_field_name: f,
                    deprecated: graphql_definition.deprecated.clone(),
                })
            })
        }
    }?;

    Ok(Command {
        name: qualified_command_name,
        output_type: mk_qualified_type_reference(&command.output_type, subgraph),
        arguments,
        graphql_api,
        source: None,
        description: command_description,
    })
}

fn type_exists(
    type_obj: &TypeReference,
    subgraph: &str,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> bool {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => type_exists(
            type_obj,
            subgraph,
            object_types,
            scalar_types,
            object_boolean_expression_types,
            boolean_expression_types,
        ),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => true,
            TypeName::Custom(type_name) => {
                let qualified_type_name =
                    Qualified::new(subgraph.to_string(), type_name.to_owned());

                get_type_representation(
                    &qualified_type_name,
                    object_types,
                    scalar_types,
                    object_boolean_expression_types,
                    boolean_expression_types,
                )
                .is_ok()
            }
        },
    }
}
