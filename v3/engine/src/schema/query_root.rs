//! Schema of the query root type

use lang_graphql::schema as gql_schema;
use open_dds::{commands::GraphQlRootFieldKind, types::CustomTypeName};
use std::collections::HashMap;

use crate::metadata::resolved::subgraph;
use crate::schema::commands;
use crate::schema::{mk_typename, GDS};

pub mod node_field;
pub mod select_many;
pub mod select_one;

/// Generates schema for the query root type
pub fn query_root_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
) -> Result<gql_schema::Object<GDS>, crate::schema::Error> {
    let type_name = mk_typename("Query")?;
    let mut fields = HashMap::new();
    for model in gds.metadata.models.values() {
        for select_unique in model.graphql_api.select_uniques.iter() {
            let (field_name, field) =
                select_one::select_one_field(gds, builder, model, select_unique, &type_name)?;
            fields.insert(field_name, field);
        }
        for select_many in model.graphql_api.select_many.iter() {
            let (field_name, field) =
                select_many::select_many_field(gds, builder, model, select_many, &type_name)?;
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
                    commands::command_field(gds, builder, command, command_field_name)?;
                fields.insert(field_name, field);
            }
        }
    }

    let node_field::RelayNodeFieldOutput {
        relay_node_gql_field: node_field,
        relay_node_permissions: roles_implementing_node_interface,
    } = node_field::relay_node_field(gds, builder)?;
    if fields
        .insert(
            node_field.name.clone(),
            // Instead of allowing all, here we should conditionally
            // allow roles whose atleast one object implement the
            // global ID.
            builder.conditional_namespaced(node_field.clone(), roles_implementing_node_interface),
        )
        .is_some()
    {
        return Err(
            crate::schema::Error::DuplicateFieldNameGeneratedInObjectType {
                field_name: node_field.name,
                type_name: subgraph::Qualified::new(
                    "-".to_string(),
                    CustomTypeName("Query".to_string()),
                ),
            },
        );
    };
    Ok(gql_schema::Object::new(
        builder,
        type_name,
        None,
        fields,
        HashMap::new(),
    ))
}
