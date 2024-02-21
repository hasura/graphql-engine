use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};

use open_dds::{
    commands::CommandName,
    models::ModelName,
    types::{CustomTypeName, TypeName},
};

use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::command::{
    resolve_command, resolve_command_permissions, resolve_command_source, Command,
};
use crate::metadata::resolved::data_connector::DataConnectorContext;
use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::model::{
    resolve_model, resolve_model_graphql_api, resolve_model_select_permissions,
    resolve_model_source, Model,
};
use crate::metadata::resolved::relationship::resolve_relationship;
use crate::metadata::resolved::types::{
    check_conflicting_graphql_types, mk_name, resolve_object_type, resolve_output_type_permission,
    TypeRepresentation,
};

use super::types::ScalarTypeRepresentation;
use crate::metadata::resolved::graphql_config::{GlobalGraphqlConfig, GraphqlConfig};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub types: HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub commands: IndexMap<Qualified<CommandName>, Command>,
    pub graphql_config: GlobalGraphqlConfig,
}

/*******************
    Functions to validate and convert Open DDS spec to internal metadata
*******************/
pub fn resolve_metadata(metadata: open_dds::Metadata) -> Result<Metadata, Error> {
    let metadata_accessor: open_dds::accessor::MetadataAccessor =
        open_dds::accessor::MetadataAccessor::new(metadata);

    let mut data_connectors = HashMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: data_connector,
    } in &metadata_accessor.data_connectors
    {
        let qualified_data_connector_name =
            Qualified::new(subgraph.to_string(), data_connector.name.clone());
        if data_connectors
            .insert(
                qualified_data_connector_name.clone(),
                DataConnectorContext::new(data_connector)?,
            )
            .is_some()
        {
            return Err(Error::DuplicateDataConnectorDefinition {
                name: qualified_data_connector_name,
            });
        }
    }

    let mut types: HashMap<Qualified<CustomTypeName>, TypeRepresentation> = HashMap::new();
    let mut existing_graphql_types: HashSet<ast::TypeName> = HashSet::new();
    // we collect all the types with global id fields, and models with global id source for that field. this is used
    // later for validation, such that a type with global id field must have atleast one model with global id source
    let mut global_id_enabled_types: HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>> =
        HashMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: object_type_definition,
    } in &metadata_accessor.object_types
    {
        let qualified_object_type_name =
            Qualified::new(subgraph.to_string(), object_type_definition.name.clone());
        if types
            .insert(
                qualified_object_type_name.clone(),
                resolve_object_type(
                    object_type_definition,
                    &mut existing_graphql_types,
                    &qualified_object_type_name,
                    subgraph,
                    &mut global_id_enabled_types,
                )?,
            )
            .is_some()
        {
            return Err(Error::DuplicateTypeDefinition {
                name: qualified_object_type_name,
            });
        }
    }

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: scalar_type,
    } in &metadata_accessor.scalar_types
    {
        let graphql_type_name = match scalar_type.graphql.as_ref() {
            None => Ok(None),
            Some(type_name) => mk_name(type_name.type_name.0.as_ref())
                .map(ast::TypeName)
                .map(Some),
        }?;
        let qualified_scalar_type_name =
            Qualified::new(subgraph.to_string(), scalar_type.name.clone());
        if types
            .insert(
                qualified_scalar_type_name.clone(),
                TypeRepresentation::ScalarType(ScalarTypeRepresentation {
                    graphql_type_name: graphql_type_name.clone(),
                    description: scalar_type.description.clone(),
                }),
            )
            .is_some()
        {
            return Err(Error::DuplicateTypeDefinition {
                name: qualified_scalar_type_name,
            });
        }
        check_conflicting_graphql_types(&mut existing_graphql_types, graphql_type_name.as_ref())?;
    }

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: output_type_permission,
    } in &metadata_accessor.type_permissions
    {
        let qualified_type_name = Qualified::new(
            subgraph.to_string(),
            output_type_permission.type_name.to_owned(),
        );
        match types.get_mut(&qualified_type_name) {
            None => {
                return Err(Error::UnknownTypeInOutputPermissionsDefinition {
                    type_name: qualified_type_name,
                })
            }
            Some(object_type) => {
                resolve_output_type_permission(object_type, output_type_permission)?;
            }
        }
    }

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: scalar_type_representation,
    } in &metadata_accessor.data_connector_scalar_representations
    {
        let scalar_type_name: &String = &scalar_type_representation.data_connector_scalar_type;
        let qualified_data_connector_name = Qualified::new(
            subgraph.to_string(),
            scalar_type_representation.data_connector_name.to_owned(),
        );
        let connector_context = data_connectors
            .get_mut(&qualified_data_connector_name)
            .ok_or_else(|| Error::ScalarTypeFromUnknownDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        let scalar_type = connector_context
            .scalars
            .get_mut(
                scalar_type_representation
                    .data_connector_scalar_type
                    .as_str(),
            )
            .ok_or_else(|| Error::UnknownScalarTypeInDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        if scalar_type.representation.is_none() {
            match &scalar_type_representation.representation {
                TypeName::Inbuilt(_) => {} // TODO: Validate Nullable and Array types in Inbuilt
                TypeName::Custom(type_name) => {
                    let qualified_type_name =
                        Qualified::new(subgraph.to_string(), type_name.to_owned());
                    let _representation = types.get(&qualified_type_name).ok_or_else(|| {
                        Error::ScalarTypeUnknownRepresentation {
                            scalar_type: scalar_type_name.clone(),
                            type_name: qualified_type_name,
                        }
                    })?;
                }
            }
            scalar_type.representation = Some(scalar_type_representation.representation.clone());
        } else {
            return Err(Error::DuplicateDataConnectorScalarRepresentation {
                data_connector: qualified_data_connector_name.clone(),
                scalar_type: scalar_type_name.clone(),
            });
        }
        scalar_type.comparison_expression_name = match scalar_type_representation.graphql.as_ref() {
            None => Ok(None),
            Some(graphql) => match &graphql.comparison_expression_type_name {
                None => Ok(None),
                Some(type_name) => mk_name(type_name.0.as_ref()).map(ast::TypeName).map(Some),
            },
        }?;
        // We are allowing conflicting graphql types for scalar comparison expressions, but we still want the typename
        // to not conflict with other graphql type names
        //
        // TODO: This means that comparison expression names conflicting with already encountered graphql type names
        // will pass through. They'll eventually be caught during schema generation but only if the expression was
        // reachable in the graphql API. Ideally, we should just fail the build here.
        if let Some(new_graphql_type) = &scalar_type.comparison_expression_name {
            existing_graphql_types.insert(new_graphql_type.clone());
        };
    }

    // TODO: validate types

    let mut models = IndexMap::new();
    let mut global_id_models = HashMap::new();
    let graphql_config =
        GraphqlConfig::new(&metadata_accessor.graphql_config, &metadata_accessor.flags)?;

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: model,
    } in &metadata_accessor.models
    {
        let mut resolved_model =
            resolve_model(subgraph, model, &types, &mut global_id_enabled_types)?;
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
                &data_connectors,
                &types,
            )?;
        }
        if let Some(model_graphql_definition) = &model.graphql {
            resolve_model_graphql_api(
                model_graphql_definition,
                &mut resolved_model,
                subgraph,
                &mut existing_graphql_types,
                &data_connectors,
                &model.description,
                &graphql_config,
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

    // To check if global_id_fields are defined in object type but no model has global_id_source set to true:
    //   - Throw an error if no model with globalIdSource:true is found for the object type.
    for (object_type, model_name_list) in global_id_enabled_types {
        if model_name_list.is_empty() {
            return Err(Error::GlobalIdSourceNotDefined { object_type });
        }
    }

    // resolve all the command without their sources
    let mut commands: IndexMap<Qualified<CommandName>, Command> = IndexMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        let mut resolved_command = resolve_command(command, subgraph, &types)?;
        if let Some(command_source) = &command.source {
            resolve_command_source(
                command_source,
                &mut resolved_command,
                subgraph,
                &data_connectors,
                &types,
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

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: relationship,
    } in &metadata_accessor.relationships
    {
        let qualified_relationship_source_type_name =
            Qualified::new(subgraph.to_string(), relationship.source.to_owned());
        let source_type = types
            .get_mut(&qualified_relationship_source_type_name)
            .ok_or_else(|| Error::RelationshipDefinedOnUnknownType {
                relationship_name: relationship.name.clone(),
                type_name: qualified_relationship_source_type_name.clone(),
            })?;

        match source_type {
            TypeRepresentation::Object(object_representation) => {
                let resolved_relationship = resolve_relationship(
                    relationship,
                    subgraph,
                    &models,
                    &commands,
                    &data_connectors,
                    object_representation,
                )?;
                if object_representation
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
            TypeRepresentation::ScalarType { .. } => {
                return Err(Error::NotSupported {
                    reason: "A relationship can only be defined on an OBJECT type.".to_string(),
                })
            }
        }
    }

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
            command.permissions = Some(resolve_command_permissions(command_permissions)?);
        } else {
            return Err(Error::DuplicateCommandPermission {
                command_name: qualified_command_name.clone(),
            });
        }
    }

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
                &data_connectors,
                &types,
                &models, // This is required to get the model for the relationship target
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

    Ok(Metadata {
        types,
        models,
        commands,
        graphql_config: graphql_config.global.clone(),
    })
}
