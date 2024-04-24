use std::collections::{HashMap, HashSet};

use hasura_authn_core::Role;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::command;

use crate::metadata::resolved::error::{BooleanExpressionError, Error};
use crate::metadata::resolved::model::{
    resolve_model, resolve_model_graphql_api, resolve_model_select_permissions,
    resolve_model_source, Model,
};
use crate::metadata::resolved::relationship::resolve_relationship;
use crate::metadata::resolved::subgraph::Qualified;
use crate::metadata::resolved::types::{
    resolve_object_boolean_expression_type, ObjectBooleanExpressionType,
};

use crate::metadata::resolved::stages::{
    data_connector_scalar_types, data_connector_type_mappings, graphql_config, scalar_types,
    type_permissions,
};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub object_types:
        HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    pub scalar_types: HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub commands: IndexMap<Qualified<CommandName>, command::Command>,
    pub boolean_expression_types: HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    pub graphql_config: graphql_config::GlobalGraphqlConfig,
    pub roles: Vec<Role>,
}

/*******************
    Functions to validate and resolve OpenDD spec to internal metadata
*******************/
pub fn resolve_metadata(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    graphql_config: &graphql_config::GraphqlConfig,
    mut existing_graphql_types: HashSet<ast::TypeName>,
    mut global_id_enabled_types: HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    mut apollo_federation_entity_enabled_types: HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    object_types: HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
) -> Result<Metadata, Error> {
    // resolve object boolean expression types
    let boolean_expression_types = resolve_boolean_expression_types(
        metadata_accessor,
        data_connectors,
        data_connector_type_mappings,
        &object_types,
        scalar_types,
        &mut existing_graphql_types,
        graphql_config,
    )?;

    // resolve models
    // TODO: validate types
    let mut models = resolve_models(
        metadata_accessor,
        data_connectors,
        data_connector_type_mappings,
        &object_types,
        scalar_types,
        &mut existing_graphql_types,
        &mut global_id_enabled_types,
        &mut apollo_federation_entity_enabled_types,
        &boolean_expression_types,
        graphql_config,
    )?;

    // To check if global_id_fields are defined in object type but no model has global_id_source set to true:
    //   - Throw an error if no model with globalIdSource:true is found for the object type.
    for (object_type, model_name_list) in global_id_enabled_types {
        if model_name_list.is_empty() {
            return Err(Error::GlobalIdSourceNotDefined { object_type });
        }
    }

    // To check if apollo federation entity keys are defined in object type but no model has
    // apollo_federation_entity_source set to true:
    //   - Throw an error if no model with apolloFederation.entitySource:true is found for the object type.
    for (object_type, model_name_list) in apollo_federation_entity_enabled_types {
        if model_name_list.is_none() {
            return Err(Error::ApolloFederationEntitySourceNotDefined { object_type });
        }
    }

    // resolve commands
    let mut commands = resolve_commands(
        metadata_accessor,
        data_connectors,
        data_connector_type_mappings,
        &object_types,
        scalar_types,
    )?;

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
    resolve_command_permissions(metadata_accessor, &mut commands)?;

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
    )?;

    let roles = collect_all_roles(&object_types, &models, &commands);

    Ok(Metadata {
        scalar_types: scalar_types.clone(),
        object_types,
        models,
        commands,
        boolean_expression_types,
        graphql_config: graphql_config.global.clone(),
        roles,
    })
}

/// Gather all roles from various permission objects.
fn collect_all_roles(
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    models: &IndexMap<Qualified<ModelName>, Model>,
    commands: &IndexMap<Qualified<CommandName>, command::Command>,
) -> Vec<Role> {
    let mut roles = Vec::new();
    for object_type in object_types.values() {
        for role in object_type.type_output_permissions.keys() {
            roles.push(role.clone());
        }
        for role in object_type.type_input_permissions.keys() {
            roles.push(role.clone());
        }
    }
    for model in models.values() {
        if let Some(select_permissions) = &model.select_permissions {
            for role in select_permissions.keys() {
                roles.push(role.clone());
            }
        }
    }
    for command in commands.values() {
        if let Some(command_permissions) = &command.permissions {
            for role in command_permissions.keys() {
                roles.push(role.clone());
            }
        }
    }
    roles
}

/// resolve commands
fn resolve_commands(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> Result<IndexMap<Qualified<CommandName>, command::Command>, Error> {
    let mut commands: IndexMap<Qualified<CommandName>, command::Command> = IndexMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        let mut resolved_command =
            command::resolve_command(command, subgraph, object_types, scalar_types)?;
        if let Some(command_source) = &command.source {
            command::resolve_command_source(
                command_source,
                &mut resolved_command,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                data_connector_type_mappings,
            )?;
        }
        let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
        if commands
            .insert(qualified_command_name.clone(), resolved_command)
            .is_some()
        {
            return Err(Error::DuplicateCommandDefinition {
                name: qualified_command_name,
            });
        }
    }
    Ok(commands)
}

/// resolve object boolean expression types
fn resolve_boolean_expression_types(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>, Error> {
    let mut boolean_expression_types = HashMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: boolean_expression_type,
    } in &metadata_accessor.object_boolean_expression_types
    {
        let resolved_boolean_expression = resolve_object_boolean_expression_type(
            boolean_expression_type,
            subgraph,
            data_connectors,
            data_connector_type_mappings,
            object_types,
            scalar_types,
            existing_graphql_types,
            graphql_config,
        )?;
        if let Some(existing) = boolean_expression_types.insert(
            resolved_boolean_expression.name.clone(),
            resolved_boolean_expression,
        ) {
            return Err(Error::from(
                BooleanExpressionError::DuplicateObjectBooleanExpressionTypeDefinition {
                    name: existing.name,
                },
            ));
        }
    }
    Ok(boolean_expression_types)
}

/// resolve models
fn resolve_models(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    global_id_enabled_types: &mut HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    boolean_expression_types: &HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<IndexMap<Qualified<ModelName>, Model>, Error> {
    // resolve models
    // TODO: validate types
    let mut models = IndexMap::new();
    let mut global_id_models = HashMap::new();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: model,
    } in &metadata_accessor.models
    {
        let mut resolved_model = resolve_model(
            subgraph,
            model,
            object_types,
            global_id_enabled_types,
            apollo_federation_entity_enabled_types,
            boolean_expression_types,
        )?;
        if resolved_model.global_id_source.is_some() {
            match global_id_models.insert(
                resolved_model.data_type.clone(),
                resolved_model.name.clone(),
            ) {
                None => {}
                Some(duplicate_model_name) => {
                    return Err(Error::DuplicateModelGlobalIdSource {
                        model_1: resolved_model.name,
                        model_2: duplicate_model_name,
                        object_type: resolved_model.data_type,
                    })
                }
            }
        }

        if let Some(model_source) = &model.source {
            resolve_model_source(
                model_source,
                &mut resolved_model,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                data_connector_type_mappings,
            )?;
        }
        if let Some(model_graphql_definition) = &model.graphql {
            resolve_model_graphql_api(
                model_graphql_definition,
                &mut resolved_model,
                existing_graphql_types,
                data_connectors,
                &model.description,
                graphql_config,
            )?;
        }
        let qualified_model_name = Qualified::new(subgraph.to_string(), model.name.clone());
        if models
            .insert(qualified_model_name.clone(), resolved_model)
            .is_some()
        {
            return Err(Error::DuplicateModelDefinition {
                name: qualified_model_name,
            });
        }
    }
    Ok(models)
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
    models: &IndexMap<Qualified<ModelName>, Model>,
    commands: &IndexMap<Qualified<CommandName>, command::Command>,
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
    commands: &mut IndexMap<Qualified<CommandName>, command::Command>,
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
    models: &mut IndexMap<Qualified<ModelName>, Model>,
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
