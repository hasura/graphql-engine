use std::collections::HashMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::command;
use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::model::resolve_model_select_permissions;
use crate::metadata::resolved::relationship::resolve_relationship;
use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::stages::{
    boolean_expressions, commands, data_connector_scalar_types, data_connector_type_mappings,
    graphql_config, models, roles, scalar_types, type_permissions,
};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub object_types:
        HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    pub scalar_types: HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, models::Model>,
    pub commands: IndexMap<Qualified<CommandName>, commands::Command>,
    pub boolean_expression_types:
        HashMap<Qualified<CustomTypeName>, boolean_expressions::ObjectBooleanExpressionType>,
    pub graphql_config: graphql_config::GlobalGraphqlConfig,
    pub roles: Vec<Role>,
}

/*******************
    Functions to validate and resolve OpenDD spec to internal metadata
*******************/
pub fn resolve_metadata(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    graphql_config: &graphql_config::GraphqlConfig,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    object_types: HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    mut models: IndexMap<Qualified<ModelName>, models::Model>,
    mut commands: IndexMap<Qualified<CommandName>, commands::Command>,
) -> Result<Metadata, Error> {
    // resolve relationships
    let object_types = resolve_relationships(
        metadata_accessor,
        data_connectors,
        object_types,
        &models,
        &commands,
    )?;

    // resolve command permissions
    // TODO: make this return values rather than blindly mutating it's inputs
    resolve_command_permissions(
        metadata_accessor,
        &mut commands,
        &object_types,
        boolean_expression_types,
        data_connectors,
        data_connector_type_mappings,
    )?;

    // resolve model permissions
    // Note: Model permissions's predicate can include the relationship field,
    // hence Model permissions should be resolved after the relationships of a
    // model is resolved.
    // TODO: make this return values rather than blindly mutating it's inputs
    resolve_model_permissions(
        metadata_accessor,
        data_connectors,
        &object_types,
        &mut models,
        boolean_expression_types,
        data_connector_type_mappings,
    )?;

    let roles = roles::resolve(&object_types, &models, &commands);

    Ok(Metadata {
        scalar_types: scalar_types.clone(),
        object_types,
        models,
        commands,
        boolean_expression_types: boolean_expression_types.clone(),
        graphql_config: graphql_config.global.clone(),
        roles,
    })
}

/// resolve relationships
/// returns updated `types` value
fn resolve_relationships(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    mut object_types: HashMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
) -> Result<HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>, Error>
{
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: relationship,
    } in &metadata_accessor.relationships
    {
        let qualified_relationship_source_type_name =
            Qualified::new(subgraph.to_string(), relationship.source.to_owned());
        let object_representation = object_types
            .get_mut(&qualified_relationship_source_type_name)
            .ok_or_else(|| Error::RelationshipDefinedOnUnknownType {
                relationship_name: relationship.name.clone(),
                type_name: qualified_relationship_source_type_name.clone(),
            })?;

        let resolved_relationship = resolve_relationship(
            relationship,
            subgraph,
            models,
            commands,
            data_connectors,
            object_representation,
        )?;

        if object_representation
            .object_type
            .relationships
            .insert(
                resolved_relationship.field_name.clone(),
                resolved_relationship,
            )
            .is_some()
        {
            return Err(Error::DuplicateRelationshipInSourceType {
                type_name: qualified_relationship_source_type_name,
                relationship_name: relationship.name.clone(),
            });
        }
    }

    Ok(object_types)
}

/// resolve command permissions
/// this currently works by mutating `commands`, let's change it to
/// return new values instead where possible
fn resolve_command_permissions(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    commands: &mut IndexMap<Qualified<CommandName>, commands::Command>,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
) -> Result<(), Error> {
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: command_permissions,
    } in &metadata_accessor.command_permissions
    {
        let command_name = &command_permissions.command_name;
        let qualified_command_name = Qualified::new(subgraph.to_string(), command_name.to_owned());
        let command = commands.get_mut(&qualified_command_name).ok_or_else(|| {
            Error::UnknownCommandInCommandPermissions {
                command_name: qualified_command_name.clone(),
            }
        })?;
        if command.permissions.is_none() {
            command.permissions = Some(command::resolve_command_permissions(
                command,
                command_permissions,
                object_types,
                boolean_expression_types,
                data_connectors,
                data_connector_type_mappings,
                subgraph,
            )?);
        } else {
            return Err(Error::DuplicateCommandPermission {
                command_name: qualified_command_name.clone(),
            });
        }
    }
    Ok(())
}

/// resolve model permissions
/// this currently works by mutating `models`, let's change it to
/// return new values instead where possible
fn resolve_model_permissions(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    models: &mut IndexMap<Qualified<ModelName>, models::Model>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
) -> Result<(), Error> {
    // Note: Model permissions's predicate can include the relationship field,
    // hence Model permissions should be resolved after the relationships of a
    // model is resolved.
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: permissions,
    } in &metadata_accessor.model_permissions
    {
        let model_name = Qualified::new(subgraph.to_string(), permissions.model_name.clone());
        let model =
            models
                .get(&model_name)
                .ok_or_else(|| Error::UnknownModelInModelSelectPermissions {
                    model_name: model_name.clone(),
                })?;

        if model.select_permissions.is_none() {
            let select_permissions = Some(resolve_model_select_permissions(
                model,
                subgraph,
                permissions,
                data_connectors,
                object_types,
                models, // This is required to get the model for the relationship target
                boolean_expression_types,
                data_connector_type_mappings,
            )?);

            let model = models.get_mut(&model_name).ok_or_else(|| {
                Error::UnknownModelInModelSelectPermissions {
                    model_name: model_name.clone(),
                }
            })?;
            model.select_permissions = select_permissions;
        } else {
            return Err(Error::DuplicateModelSelectPermission {
                model_name: model_name.clone(),
            });
        }
    }
    Ok(())
}
