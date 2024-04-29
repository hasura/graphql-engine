use std::collections::HashMap;

use hasura_authn_core::Role;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::model::resolve_model_select_permissions;
use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::stages::{
    boolean_expressions, command_permissions, data_connector_scalar_types,
    data_connector_type_mappings, graphql_config, models, relationships, roles, scalar_types,
};

/// Resolved and validated metadata for a project. Used internally in the v3 server.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Metadata {
    pub object_types:
        HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    pub scalar_types: HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    pub models: IndexMap<Qualified<ModelName>, models::Model>,
    pub commands: IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
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
    object_types: HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    mut models: IndexMap<Qualified<ModelName>, models::Model>,
    commands: IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
) -> Result<Metadata, Error> {
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

/// resolve model permissions
/// this currently works by mutating `models`, let's change it to
/// return new values instead where possible
fn resolve_model_permissions(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &HashMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
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
