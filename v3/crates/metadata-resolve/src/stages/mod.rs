pub mod aggregates;
mod apollo;
pub mod boolean_expressions;
pub mod command_permissions;
pub mod commands;
pub mod data_connector_scalar_types;
pub mod data_connectors;
pub mod graphql_config;
pub mod model_permissions;
pub mod models;
pub mod models_graphql;
pub mod object_boolean_expressions;
pub mod object_types;
pub mod relationships;
pub mod roles;
pub mod scalar_types;
pub mod type_permissions;
mod types;

pub use types::Metadata;

use crate::types::error::Error;
use crate::types::internal_flags::MetadataResolveFlagsInternal;

/// This is where we take the input metadata and attempt to resolve a working `Metadata` object.
pub fn resolve(
    metadata: open_dds::Metadata,
    flags: MetadataResolveFlagsInternal,
) -> Result<Metadata, Error> {
    let metadata_accessor: open_dds::accessor::MetadataAccessor =
        open_dds::accessor::MetadataAccessor::new(metadata);

    // The graphql config represents the shape of the Hasura features in the graphql schema,
    // and which features should be enabled or disabled. We check this structure is valid.
    let graphql_config =
        graphql_config::resolve(&metadata_accessor.graphql_config, metadata_accessor.flags)?;

    // Fetch and check schema information for all our data connectors
    let data_connectors = data_connectors::resolve(&metadata_accessor)?;

    // Validate object types defined in metadata
    let object_types::DataConnectorTypeMappingsOutput {
        graphql_types,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        object_types,
    } = object_types::resolve(&metadata_accessor, &data_connectors)?;

    // Validate custom defined scalar types
    let scalar_types::ScalarTypesOutput {
        scalar_types,
        graphql_types,
    } = scalar_types::resolve(&metadata_accessor, &graphql_types)?;

    // Validate `DataConnectorScalarType` metadata. This will soon be deprecated and subsumed by
    // `BooleanExpressionType`
    let data_connector_scalar_types::DataConnectorWithScalarsOutput {
        data_connector_scalars,
        graphql_types,
    } = data_connector_scalar_types::resolve(
        &metadata_accessor,
        &data_connectors,
        &scalar_types,
        &graphql_types,
    )?;

    // Fetch and validate permissions, and attach them to the relevant object types
    let object_types_with_permissions =
        type_permissions::resolve(&metadata_accessor, &object_types)?;

    // Check aggregate expressions
    let aggregates::AggregateExpressionsOutput {
        aggregate_expressions,
        graphql_types,
    } = aggregates::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        &scalar_types,
        graphql_types,
        &graphql_config,
    )?;

    // Validate `ObjectBooleanExpressionType` metadata. This will soon be deprecated and subsumed
    // by `BooleanExpressionType`.
    let object_boolean_expressions::ObjectBooleanExpressionsOutput {
        object_boolean_expression_types,
        graphql_types,
    } = object_boolean_expressions::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        &graphql_types,
        &graphql_config,
    )?;

    // Resolve fancy new boolean expression types
    let boolean_expressions::BooleanExpressionsOutput {
        boolean_expression_types,
        graphql_types,
    } = boolean_expressions::resolve(
        &metadata_accessor,
        flags,
        &graphql_types,
        &graphql_config,
        &data_connectors,
        &object_types_with_permissions,
    )?;

    // Resolve models and their sources
    let models::ModelsOutput {
        models,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
    } = models::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &global_id_enabled_types,
        &apollo_federation_entity_enabled_types,
        &object_types_with_permissions,
        &scalar_types,
        &object_boolean_expression_types,
        &boolean_expression_types,
        &aggregate_expressions,
    )?;

    let commands = commands::resolve(
        &metadata_accessor,
        &data_connectors,
        &object_types_with_permissions,
        &scalar_types,
        &object_boolean_expression_types,
        &boolean_expression_types,
    )?;

    apollo::resolve(
        &global_id_enabled_types,
        &apollo_federation_entity_enabled_types,
    )?;

    let object_types_with_relationships = relationships::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        &models,
        &commands,
        &aggregate_expressions,
        &graphql_config,
    )?;

    // Resolve the filter expressions and graphql settings for models
    // This is a separate step so we can look up resolved models and their sources
    let models_with_graphql = models_graphql::resolve(
        &metadata_accessor,
        &models,
        &data_connector_scalars,
        &object_types_with_relationships,
        &object_boolean_expression_types,
        &boolean_expression_types,
        &graphql_types,
        &graphql_config,
    )?;

    let commands_with_permissions = command_permissions::resolve(
        &metadata_accessor,
        &commands,
        &object_types_with_relationships,
        &scalar_types,
        &object_boolean_expression_types,
        &boolean_expression_types,
        &models_with_graphql,
        &data_connectors,
        &data_connector_scalars,
    )?;

    let models_with_permissions = model_permissions::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_relationships,
        &scalar_types,
        &models_with_graphql,
        &object_boolean_expression_types,
        &boolean_expression_types,
    )?;

    let roles = roles::resolve(
        &object_types_with_relationships,
        &models_with_permissions,
        &commands_with_permissions,
    );

    Ok(Metadata {
        scalar_types,
        object_types: object_types_with_relationships,
        models: models_with_permissions,
        commands: commands_with_permissions,
        object_boolean_expression_types,
        boolean_expression_types,
        aggregate_expressions,
        graphql_config: graphql_config.global,
        roles,
    })
}
