use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};

use open_dds::{
    commands::CommandName,
    data_connector::DataConnectorName,
    models::ModelName,
    types::{CustomTypeName, TypeName},
};

use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::command;
use crate::metadata::resolved::data_connector::DataConnectorContext;
use crate::metadata::resolved::error::{BooleanExpressionError, Error};
use crate::metadata::resolved::model::{
    resolve_model, resolve_model_graphql_api, resolve_model_select_permissions,
    resolve_model_source, Model,
};
use crate::metadata::resolved::relationship::resolve_relationship;
use crate::metadata::resolved::types::{
    mk_name, resolve_object_type, resolve_output_type_permission, store_new_graphql_type,
    TypeRepresentation,
};

use super::types::{
    resolve_data_connector_type_mapping, resolve_object_boolean_expression_type,
    ObjectBooleanExpressionType, ScalarTypeRepresentation, TypeMapping,
};
use crate::metadata::resolved::stages::graphql_config::{GlobalGraphqlConfig, GraphqlConfig};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub types: HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub commands: IndexMap<Qualified<CommandName>, command::Command>,
    pub boolean_expression_types: HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    pub graphql_config: GlobalGraphqlConfig,
}

pub type DataConnectorTypeMappingsForObjectType =
    HashMap<Qualified<DataConnectorName>, HashMap<String, TypeMapping>>;
pub struct DataConnectorTypeMappings(
    HashMap<Qualified<CustomTypeName>, DataConnectorTypeMappingsForObjectType>,
);

impl Default for DataConnectorTypeMappings {
    fn default() -> Self {
        Self::new()
    }
}

impl DataConnectorTypeMappings {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(
        &self,
        object_type_name: &Qualified<CustomTypeName>,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &str,
    ) -> Option<&TypeMapping> {
        self.0
            .get(object_type_name)
            .and_then(|connectors| {
                connectors
                    .get(data_connector_name)
                    .map(|data_connector_object_types| {
                        data_connector_object_types.get(data_connector_object_type)
                    })
            })
            .flatten()
    }

    fn insert(
        &mut self,
        object_type_name: &Qualified<CustomTypeName>,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &str,
        type_mapping: TypeMapping,
    ) -> Result<(), Error> {
        if self
            .0
            .entry(object_type_name.clone())
            .or_default()
            .entry(data_connector_name.clone())
            .or_default()
            .insert(data_connector_object_type.to_string(), type_mapping)
            .is_some()
        {
            return Err(Error::DuplicateDataConnectorTypeMapping {
                type_name: object_type_name.clone(),
                data_connector: data_connector_name.clone(),
                data_connector_object_type: data_connector_object_type.to_string(),
            });
        }
        Ok(())
    }
}

/*******************
    Functions to validate and resolve OpenDD spec to internal metadata
*******************/
pub fn resolve_metadata(
    metadata_accessor: open_dds::accessor::MetadataAccessor,
    graphql_config: GraphqlConfig,
) -> Result<Metadata, Error> {
    // resolve data connectors
    let mut data_connectors = resolve_data_connectors(&metadata_accessor)?;

    let mut existing_graphql_types: HashSet<ast::TypeName> = HashSet::new();

    // we collect all the types with global id fields, and models with global id source for that field. this is used
    // later for validation, such that a type with global id field must have atleast one model with global id source
    let mut global_id_enabled_types: HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>> =
        HashMap::new();

    let mut apollo_federation_entity_enabled_types: HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<ModelName>>,
    > = HashMap::new();

    // resolve object types
    let (data_connector_type_mappings, types) = resolve_data_connector_type_mappings_and_objects(
        &metadata_accessor,
        &data_connectors,
        &mut existing_graphql_types,
        &mut global_id_enabled_types,
        &mut apollo_federation_entity_enabled_types,
    )?;

    // resolve scalar types
    let scalar_types = resolve_scalar_types(&metadata_accessor, &mut existing_graphql_types)?;

    // resolve type permissions
    let types = resolve_type_permissions(&metadata_accessor, extend_types(types, scalar_types)?)?;

    // resolve object boolean expression types
    let boolean_expression_types = resolve_boolean_expression_types(
        &metadata_accessor,
        &data_connectors,
        &data_connector_type_mappings,
        &types,
        &mut existing_graphql_types,
    )?;

    // resolve data connector scalar representations
    // TODO: make this return values rather than blindly mutating it's inputs
    resolve_data_connector_scalar_representations(
        &metadata_accessor,
        &mut data_connectors,
        &types,
        &mut existing_graphql_types,
    )?;

    // resolve models
    // TODO: validate types
    let mut models = resolve_models(
        &metadata_accessor,
        &data_connectors,
        &data_connector_type_mappings,
        &types,
        &mut existing_graphql_types,
        &mut global_id_enabled_types,
        &mut apollo_federation_entity_enabled_types,
        &boolean_expression_types,
        &graphql_config,
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
        &metadata_accessor,
        &data_connectors,
        &data_connector_type_mappings,
        &types,
    )?;

    // resolve relationships
    let types = resolve_relationships(
        &metadata_accessor,
        &data_connectors,
        types,
        &models,
        &commands,
    )?;

    // resolve command permissions
    // TODO: make this return values rather than blindly mutating it's inputs
    resolve_command_permissions(&metadata_accessor, &mut commands)?;

    // resolve model permissions
    // Note: Model permissions's predicate can include the relationship field,
    // hence Model permissions should be resolved after the relationships of a
    // model is resolved.
    // TODO: make this return values rather than blindly mutating it's inputs
    resolve_model_permissions(&metadata_accessor, &data_connectors, &types, &mut models)?;

    Ok(Metadata {
        types,
        models,
        commands,
        boolean_expression_types,
        graphql_config: graphql_config.global.clone(),
    })
}

/// resolve commands
fn resolve_commands(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    data_connector_type_mappings: &DataConnectorTypeMappings,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<IndexMap<Qualified<CommandName>, command::Command>, Error> {
    let mut commands: IndexMap<Qualified<CommandName>, command::Command> = IndexMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        let mut resolved_command = command::resolve_command(command, subgraph, types)?;
        if let Some(command_source) = &command.source {
            command::resolve_command_source(
                command_source,
                &mut resolved_command,
                subgraph,
                data_connectors,
                types,
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

/// resolve data connectors
fn resolve_data_connectors(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<HashMap<Qualified<DataConnectorName>, DataConnectorContext>, Error> {
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
    Ok(data_connectors)
}

/// resolve object types, matching them to that in the data connectors
/// this currently works by mutating `types` and `existing_graphql_types`, we should try
/// and change this to return new values here and make the caller combine them together
fn resolve_data_connector_type_mappings_and_objects(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    global_id_enabled_types: &mut HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
) -> Result<
    (
        DataConnectorTypeMappings,
        HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    ),
    Error,
> {
    let mut data_connector_type_mappings = DataConnectorTypeMappings::new();
    let mut types = HashMap::new();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: object_type_definition,
    } in &metadata_accessor.object_types
    {
        let qualified_object_type_name =
            Qualified::new(subgraph.to_string(), object_type_definition.name.clone());

        let mut resolved_type = resolve_object_type(
            object_type_definition,
            existing_graphql_types,
            &qualified_object_type_name,
            subgraph,
            global_id_enabled_types,
            apollo_federation_entity_enabled_types,
        )?;

        // resolve object types' type mappings
        if let TypeRepresentation::Object(ref mut resolved_object_type) = resolved_type {
            for dc_type_mapping in &object_type_definition.data_connector_type_mapping {
                let qualified_data_connector_name = Qualified::new(
                    subgraph.to_string(),
                    dc_type_mapping.data_connector_name.clone(),
                );
                let type_mapping = resolve_data_connector_type_mapping(
                    dc_type_mapping,
                    &qualified_object_type_name,
                    subgraph,
                    resolved_object_type,
                    data_connectors,
                )
                .map_err(|type_validation_error| {
                    Error::DataConnectorTypeMappingValidationError {
                        type_name: qualified_object_type_name.clone(),
                        error: type_validation_error,
                    }
                })?;
                data_connector_type_mappings.insert(
                    &qualified_object_type_name,
                    &qualified_data_connector_name,
                    &dc_type_mapping.data_connector_object_type,
                    type_mapping,
                )?;
            }
        }

        if types
            .insert(qualified_object_type_name.clone(), resolved_type)
            .is_some()
        {
            return Err(Error::DuplicateTypeDefinition {
                name: qualified_object_type_name,
            });
        }
    }

    Ok((data_connector_type_mappings, types))
}

/// combine two sets of types, returning a `DuplicateTypeDefinition` error if we find duplicates
/// along the way
fn extend_types(
    mut old_types: HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    new_types: HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<HashMap<Qualified<CustomTypeName>, TypeRepresentation>, Error> {
    for new_type_name in new_types.keys() {
        if old_types.contains_key(new_type_name) {
            return Err(Error::DuplicateTypeDefinition {
                name: new_type_name.clone(),
            });
        }
    }
    old_types.extend(new_types);
    Ok(old_types)
}

/// resolve scalar types
/// this currently works by mutating `existing_graphql_types`, we should try
/// and change this to return new values here and make the caller combine them together
fn resolve_scalar_types(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
) -> Result<HashMap<Qualified<CustomTypeName>, TypeRepresentation>, Error> {
    let mut scalar_types = HashMap::new();
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

        if scalar_types
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
        store_new_graphql_type(existing_graphql_types, graphql_type_name.as_ref())?;
    }
    Ok(scalar_types)
}

/// resolve type permissions
/// this works by mutating `types`, and returning an owned value
fn resolve_type_permissions(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    mut types: HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<HashMap<Qualified<CustomTypeName>, TypeRepresentation>, Error> {
    // resolve type permissions
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
    Ok(types)
}

/// resolve object boolean expression types
fn resolve_boolean_expression_types(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    data_connector_type_mappings: &DataConnectorTypeMappings,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
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
            types,
            data_connector_type_mappings,
            existing_graphql_types,
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

/// resolve data connector scalar representations
/// this currently works by mutating `existing_graphql_types`, let's change it to
/// return new values instead if possible
fn resolve_data_connector_scalar_representations(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &mut HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
) -> Result<(), Error> {
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
    Ok(())
}

/// resolve models
fn resolve_models(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    data_connector_type_mappings: &DataConnectorTypeMappings,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    global_id_enabled_types: &mut HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    boolean_expression_types: &HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    graphql_config: &GraphqlConfig,
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
            types,
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
                types,
                data_connector_type_mappings,
            )?;
        }
        if let Some(model_graphql_definition) = &model.graphql {
            resolve_model_graphql_api(
                model_graphql_definition,
                &mut resolved_model,
                subgraph,
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
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    mut types: HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, Model>,
    commands: &IndexMap<Qualified<CommandName>, command::Command>,
) -> Result<HashMap<Qualified<CustomTypeName>, TypeRepresentation>, Error> {
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
                    models,
                    commands,
                    data_connectors,
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
    Ok(types)
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
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
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
                types,
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
