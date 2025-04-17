//! Schema of the query root type

use lang_graphql::ast::common::TypeName;
use lang_graphql::schema as gql_schema;
use open_dds::commands::GraphQlRootFieldKind;
use std::collections::BTreeMap;

use crate::GDS;
use crate::commands;
use crate::query_root::node_field::relay_node_field;

use self::node_field::RelayNodeFieldOutput;

pub mod apollo_federation;
pub mod node_field;
pub mod select_aggregate;
pub mod select_many;
pub mod select_one;

/// Generates schema for the query root type
pub fn query_root_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    query_root_type_name: &TypeName,
) -> Result<gql_schema::Object<GDS>, crate::Error> {
    let mut fields = BTreeMap::new();
    for model in gds.metadata.models.values() {
        for select_unique in &model.graphql_api.select_uniques {
            let (field_name, field) = select_one::select_one_field(
                gds,
                builder,
                model,
                select_unique,
                query_root_type_name,
            )?;
            fields.insert(field_name, field);
        }
        if let Some(select_many) = &model.graphql_api.select_many {
            let (field_name, field) = select_many::select_many_field(
                gds,
                builder,
                model,
                select_many,
                query_root_type_name,
            )?;
            fields.insert(field_name, field);
        }
        if let Some(select_aggregate) = &model.graphql_api.select_aggregate {
            let (field_name, field) = select_aggregate::select_aggregate_field(
                gds,
                builder,
                model,
                select_aggregate,
                query_root_type_name,
            )?;
            fields.insert(field_name, field);
        }
    }

    // Add node field for only the commands which have a query root field
    // defined, that is, they are based on functions.
    for command in gds.metadata.commands.values() {
        if let Some(command_graphql_api) = &command.command.graphql_api {
            if matches!(
                command_graphql_api.root_field_kind,
                GraphQlRootFieldKind::Query
            ) {
                let command_field_name = command_graphql_api.root_field_name.clone();
                let deprecation_status =
                    super::mk_deprecation_status(command_graphql_api.deprecated.as_ref());
                let (field_name, field) = commands::function_command_field(
                    gds,
                    builder,
                    command,
                    command_field_name,
                    deprecation_status,
                )?;

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
        return Err(crate::Error::DuplicateFieldInQueryRoot {
            field_name: relay_node_gql_field.name,
        });
    };

    // apollo federation field
    if gds.metadata.graphql_config.enable_apollo_federation_fields {
        let apollo_federation::ApolloFederationFieldOutput {
            apollo_federation_entities_field,
            apollo_federation_entities_field_permissions,
            apollo_federation_service_field,
        } = apollo_federation::apollo_federation_field(gds, builder)?;

        if fields
            .insert(
                apollo_federation_entities_field.name.clone(),
                builder.conditional_namespaced(
                    apollo_federation_entities_field.clone(),
                    apollo_federation_entities_field_permissions,
                ),
            )
            .is_some()
        {
            return Err(crate::Error::DuplicateFieldInQueryRoot {
                field_name: apollo_federation_entities_field.name,
            });
        };

        if fields
            .insert(
                apollo_federation_service_field.name.clone(),
                builder.allow_all_namespaced(apollo_federation_service_field.clone()),
            )
            .is_some()
        {
            return Err(crate::Error::DuplicateFieldInQueryRoot {
                field_name: apollo_federation_service_field.name,
            });
        };
    }

    Ok(gql_schema::Object::new(
        builder,
        query_root_type_name.clone(),
        None,
        fields,
        BTreeMap::new(),
        Vec::new(),
    ))
}
