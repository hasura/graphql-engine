//! Schema of the query root type

use lang_graphql::ast::common::TypeName;
use lang_graphql::schema as gql_schema;
use open_dds::commands::GraphQlRootFieldKind;
use std::collections::HashMap;

use crate::schema::commands;
use crate::schema::query_root::node_field::relay_node_field;
use crate::schema::GDS;

use self::node_field::RelayNodeFieldOutput;

pub mod node_field;
pub mod select_many;
pub mod select_one;

/// Generates schema for the query root type
pub fn query_root_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    query_root_type_name: &TypeName,
) -> Result<gql_schema::Object<GDS>, crate::schema::Error> {
    let mut fields = HashMap::new();
    for model in gds.metadata.models.values() {
        for select_unique in model.graphql_api.select_uniques.iter() {
            let (field_name, field) = select_one::select_one_field(
                gds,
                builder,
                model,
                select_unique,
                query_root_type_name,
            )?;
            fields.insert(field_name, field);
        }
        for select_many in model.graphql_api.select_many.iter() {
            let (field_name, field) = select_many::select_many_field(
                gds,
                builder,
                model,
                select_many,
                query_root_type_name,
            )?;
            fields.insert(field_name, field);
        }
    }

    // Add node field for only the commands which have a query root field
    // defined, that is, they are based on functions.
    for command in gds.metadata.commands.values() {
        if let Some(command_graphql_api) = &command.graphql_api {
            if matches!(
                command_graphql_api.root_field_kind,
                GraphQlRootFieldKind::Query
            ) {
                let command_field_name = command_graphql_api.root_field_name.clone();
                let (field_name, field) =
                    commands::function_command_field(gds, builder, command, command_field_name)?;

                fields.insert(field_name, field);
            }
        }
    }

    let RelayNodeFieldOutput {
        relay_node_gql_field,
        relay_node_field_permissions,
    } = relay_node_field(gds, builder)?;
    if fields
        .insert(
            relay_node_gql_field.name.clone(),
            builder
                .conditional_namespaced(relay_node_gql_field.clone(), relay_node_field_permissions),
        )
        .is_some()
    {
        return Err(crate::schema::Error::DuplicateFieldInQueryRoot {
            field_name: relay_node_gql_field.name,
        });
    };

    Ok(gql_schema::Object::new(
        builder,
        query_root_type_name.clone(),
        None,
        fields,
        HashMap::new(),
    ))
}
