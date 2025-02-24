use crate::helpers::argument::get_argument_kind;
use crate::helpers::types::{get_type_representation, mk_name, TrackGraphQLRootFields};
use crate::stages::{boolean_expressions, scalar_types, type_permissions};
use crate::types::subgraph::{mk_qualified_type_reference, ArgumentInfo, Qualified};
use indexmap::IndexMap;
use open_dds::identifier::SubgraphName;

use super::types::{Command, CommandGraphQlApi, CommandsIssue};
use open_dds::commands::{CommandV1, GraphQlRootFieldKind};

use open_dds::types::{BaseType, CustomTypeName, TypeName, TypeReference};

use super::error::CommandsError;
use std::collections::BTreeMap;

pub fn resolve_command(
    command: &CommandV1,
    subgraph: &SubgraphName,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    track_root_fields: &mut TrackGraphQLRootFields,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    issues: &mut Vec<CommandsIssue>,
) -> Result<Command, CommandsError> {
    let mut arguments = IndexMap::new();

    let qualified_command_name = Qualified::new(subgraph.clone(), command.name.clone());

    // Check command output type and raise an issue if invalid
    if !type_exists(
        &command.output_type,
        subgraph,
        object_types,
        scalar_types,
        // boolean expression types are also not valid output types for commands
        // so we pass in an empty boolean expression types
        &boolean_expressions::BooleanExpressionTypes::default(),
    ) {
        issues.push(CommandsIssue::InvalidCommandOutputType {
            command_name: qualified_command_name.clone(),
            output_type: command.output_type.clone(),
        });
    }

    let command_description = command.description.clone();
    // duplicate command arguments should not be allowed
    for argument in &command.arguments {
        if type_exists(
            &argument.argument_type,
            subgraph,
            object_types,
            scalar_types,
            boolean_expression_types,
        ) {
            // is this an expression or not?
            let argument_kind =
                get_argument_kind(&argument.argument_type, subgraph, boolean_expression_types);
            if arguments
                .insert(
                    argument.name.clone(),
                    ArgumentInfo {
                        argument_type: mk_qualified_type_reference(
                            &argument.argument_type,
                            subgraph,
                        ),
                        argument_kind,
                        description: argument.description.clone(),
                    },
                )
                .is_some()
            {
                return Err(CommandsError::DuplicateCommandArgumentDefinition {
                    command_name: qualified_command_name,
                    argument_name: argument.name.clone(),
                });
            }
        } else {
            return Err(CommandsError::UnknownCommandArgumentType {
                command_name: qualified_command_name,
                argument_name: argument.name.clone(),
                argument_type: argument.argument_type.clone(),
            });
        }
    }

    let graphql_api = match &command.graphql {
        Some(graphql_definition) => {
            // previously we were missing validation on whether the root field name for this
            // command has already been used. Therefore that means adding it is a breaking change,
            // therefore if the name is already in use we
            // a) dont include a GraphQL field for this command, essentially dropping the field
            // b) raise a warning, that we can raise to an error using CompatibilityConfig
            // making it an error for new projects but not old ones
            let root_field_name = mk_name(graphql_definition.root_field_name.as_ref())?;
            let root_field_tracked = match graphql_definition.root_field_kind {
                GraphQlRootFieldKind::Query => {
                    track_root_fields.track_query_root_field(&root_field_name)
                }
                GraphQlRootFieldKind::Mutation => {
                    track_root_fields.track_mutation_root_field(&root_field_name)
                }
            };
            match root_field_tracked {
                Ok(()) => Ok(Some(CommandGraphQlApi {
                    root_field_kind: graphql_definition.root_field_kind.clone(),
                    root_field_name,
                    deprecated: graphql_definition.deprecated.clone(),
                })),
                Err(error) => {
                    // raise a warning
                    issues.push(CommandsIssue::GraphQlRootFieldAlreadyInUse {
                        command_name: qualified_command_name.clone(),
                        error,
                    });
                    // don't include the field in GraphQL schema
                    Ok(None)
                }
            }
        }
        None => Ok::<Option<CommandGraphQlApi>, CommandsError>(None),
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
    subgraph: &SubgraphName,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> bool {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => type_exists(
            type_obj,
            subgraph,
            object_types,
            scalar_types,
            boolean_expression_types,
        ),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => true,
            TypeName::Custom(type_name) => {
                let qualified_type_name = Qualified::new(subgraph.clone(), type_name.to_owned());

                get_type_representation(
                    &qualified_type_name,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
                )
                .is_ok()
            }
        },
    }
}
