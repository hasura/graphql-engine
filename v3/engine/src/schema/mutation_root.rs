//! Schema for the mutation root type

use lang_graphql::ast::common::{self as ast, TypeName};
use lang_graphql::schema as gql_schema;
use std::collections::HashMap;

use crate::schema::{commands, GDS};

/// Generates schema for the query root type
pub fn mutation_root_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    mutation_root_type_name: &TypeName,
) -> Result<gql_schema::Object<GDS>, crate::schema::Error> {
    let mut fields = HashMap::new();

    // Add node field for only the commands which have a mutation root field
    // defined, that is, they are based on procedures.
    for command in gds.metadata.commands.values() {
        if let Some(command_graphql_api) = &command.graphql_api {
            if matches!(
                command_graphql_api.root_field_kind,
                open_dds::commands::GraphQlRootFieldKind::Mutation
            ) {
                let command_field_name: ast::Name = command_graphql_api.root_field_name.clone();
                let (field_name, field) =
                    commands::procedure_command_field(gds, builder, command, command_field_name)?;
                fields.insert(field_name, field);
            }
        }
    }

    Ok(gql_schema::Object::new(
        builder,
        mutation_root_type_name.clone(),
        None,
        fields,
        HashMap::new(),
    ))
}
