pub mod aggregate_boolean_expressions;
pub mod aggregates;
pub mod apollo;
pub mod argument_presets;
pub mod arguments;
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
pub mod object_relationships;
pub mod object_types;
pub mod order_by_expressions;
pub mod plugins;
pub mod relationships;
pub mod relay;
pub mod roles;
pub mod scalar_boolean_expressions;
pub mod scalar_types;
pub mod type_permissions;
mod types;
use crate::types::warning::Warning;
use open_dds::flags;
pub use types::Metadata;
pub mod scalar_type_representations;

use crate::types::configuration::Configuration;
use crate::types::error::{Error, SeparatedBy, ShouldBeAnError, WithContext};

/// This is where we take the input metadata and attempt to resolve a working `Metadata` object.
pub fn resolve(
    metadata: open_dds::Metadata,
    configuration: &Configuration,
) -> Result<(Metadata, Vec<Warning>), WithContext<Error>> {
    // all issues raised throughout metadata-resolve. These will be turned into `warnings` or
    // `errors` at the end of this function, depending on OpenDDS flags.
    let mut all_issues = vec![];

    let metadata_accessor: open_dds::accessor::MetadataAccessor =
        open_dds::accessor::MetadataAccessor::new(metadata);

    // The graphql config represents the shape of the Hasura features in the graphql schema,
    // and which features should be enabled or disabled. We check this structure is valid.
    let graphql_config =
        graphql_config::resolve(&metadata_accessor.graphql_config, metadata_accessor.flags)
            .map_err(Error::from)?;

    // Fetch and check schema information for all our data connectors
    let data_connectors::DataConnectorsOutput {
        data_connectors,
        issues,
    } = data_connectors::resolve(&metadata_accessor, configuration).map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Validate object types defined in metadata
    let object_types::ObjectTypesOutput {
        graphql_types,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        object_types,
    } = object_types::resolve(&metadata_accessor, &data_connectors).map_err(Error::from)?;

    // Validate custom defined scalar types
    let scalar_types::ScalarTypesOutput {
        scalar_types,
        graphql_types,
        issues,
    } = scalar_types::resolve(&metadata_accessor, graphql_types).map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Validate scalar `BooleanExpressionType`s
    let scalar_boolean_expressions::ScalarBooleanExpressionsOutput {
        graphql_types,
        boolean_expression_scalar_types,
    } = scalar_boolean_expressions::resolve(
        &metadata_accessor,
        graphql_types,
        &data_connectors,
        &object_types,
        &scalar_types,
    )
    .map_err(Error::from)?;

    // Validate `DataConnectorScalarType` metadata. This will soon be deprecated and subsumed by
    // `BooleanExpressionType`
    let data_connector_scalar_types::DataConnectorWithScalarsOutput {
        data_connector_scalars,
        graphql_types,
    } = data_connector_scalar_types::resolve(
        &metadata_accessor,
        &data_connectors,
        &scalar_types,
        graphql_types,
    )
    .map_err(Error::from)?;

    // Fetch and validate permissions, and attach them to the relevant object types
    let object_types_with_permissions =
        type_permissions::resolve(&metadata_accessor, object_types).map_err(Error::from)?;

    // collect raw relationships information
    let relationships = relationships::resolve(&metadata_accessor, &object_types_with_permissions)
        .map_err(Error::from)?;

    // Check aggregate expressions
    let aggregates::AggregateExpressionsOutput {
        aggregate_expressions,
        graphql_types,
        issues,
    } = aggregates::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        &scalar_types,
        graphql_types,
        &graphql_config,
    )
    .map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    let aggregate_boolean_expressions::AggregateBooleanExpressionsOutput {
        scalar_aggregates,
        object_aggregates,
        issues,
        graphql_types,
    } = aggregate_boolean_expressions::resolve(
        &configuration.unstable_features,
        &metadata_accessor,
        &boolean_expression_scalar_types,
        &aggregate_expressions,
        &relationships,
        &object_types_with_permissions,
        &scalar_types,
        &graphql_config,
        graphql_types,
    )
    .map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Resolve fancy new boolean expression types
    let boolean_expressions::BooleanExpressionsOutput {
        boolean_expression_types,
        graphql_types,
        issues,
    } = boolean_expressions::resolve(
        &metadata_accessor,
        boolean_expression_scalar_types,
        object_aggregates,
        scalar_aggregates,
        graphql_types,
        &graphql_config,
        &object_types_with_permissions,
        &relationships,
    )
    .map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    let order_by_expressions::OrderByExpressionsOutput {
        order_by_expressions,
        graphql_types,
    } = order_by_expressions::resolve(
        &metadata_accessor,
        &object_types_with_permissions,
        &relationships,
        &scalar_types,
        graphql_types,
    )?;

    // Validate `ObjectBooleanExpressionType` metadata. This will soon be deprecated and subsumed
    // by `BooleanExpressionType`.
    let object_boolean_expressions::ObjectBooleanExpressionsOutput {
        object_boolean_expression_types,
        graphql_types,
        issues,
    } = object_boolean_expressions::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        graphql_types,
        &graphql_config,
    )
    .map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Resolve models and their sources
    let models::ModelsOutput {
        models,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        order_by_expressions,
        mut graphql_types,
        issues,
    } = models::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        &object_types_with_permissions,
        &scalar_types,
        &object_boolean_expression_types,
        &boolean_expression_types,
        &aggregate_expressions,
        order_by_expressions,
        graphql_types,
    )
    .map_err(WithContext::coerce)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    let commands::CommandsOutput { commands, issues } = commands::resolve(
        &metadata_accessor,
        &data_connectors,
        &object_types_with_permissions,
        &mut graphql_types,
        &scalar_types,
        &object_boolean_expression_types,
        &boolean_expression_types,
    )
    .map_err(Error::from)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    apollo::resolve(apollo_federation_entity_enabled_types).map_err(Error::from)?;

    relay::resolve(global_id_enabled_types).map_err(Error::from)?;

    let object_types_with_relationships = object_relationships::resolve(
        object_types_with_permissions,
        &relationships,
        &data_connectors,
        &data_connector_scalars,
        &models,
        &commands,
        &aggregate_expressions,
        &graphql_config,
    )?;

    // now we know about relationships, we can check our arguments (particularly, any
    // boolean expressions they use and whether their relationships are valid)
    let issues = arguments::resolve(
        &commands,
        &models,
        &object_types_with_relationships,
        &scalar_types,
        &object_boolean_expression_types,
        &boolean_expression_types,
    )?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Resolve the filter expressions and graphql settings for models
    // This is a separate step so we can look up resolved models and their sources
    let models_graphql::ModelsWithGraphqlOutput {
        models_with_graphql,
        issues,
    } = models_graphql::resolve(
        &metadata_accessor,
        &models,
        &data_connector_scalars,
        &object_types_with_relationships,
        &object_boolean_expression_types,
        &boolean_expression_types,
        &order_by_expressions,
        &graphql_types,
        &graphql_config,
    )?;

    all_issues.extend(issues);

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

    // calculate any preset arguments for these models
    let argument_presets::ArgumentPresetsOutput { models, commands } = argument_presets::resolve(
        &models_with_permissions,
        &commands_with_permissions,
        &object_types_with_relationships,
    )
    .map_err(Error::from)?;

    let roles = roles::resolve(
        &object_types_with_relationships,
        &models_with_permissions,
        &commands_with_permissions,
    );

    // include data connector information for each scalar type
    let scalar_types_with_representations =
        scalar_type_representations::resolve(&data_connector_scalars, &scalar_types);

    let plugin_configs = plugins::resolve(&metadata_accessor);

    let all_warnings = warnings_as_errors_by_compatibility(&metadata_accessor.flags, all_issues)?;

    Ok((
        Metadata {
            scalar_types: scalar_types_with_representations,
            object_types: object_types_with_relationships,
            models,
            commands,
            object_boolean_expression_types,
            boolean_expression_types,
            order_by_expressions,
            aggregate_expressions,
            graphql_config: graphql_config.global,
            roles,
            plugin_configs,
        },
        all_warnings,
    ))
}

fn warnings_as_errors_by_compatibility(
    flags: &flags::Flags,
    all_issues: Vec<Warning>,
) -> Result<Vec<Warning>, Error> {
    let (warnings_that_are_errors, remaining_warnings): (Vec<Warning>, Vec<Warning>) = all_issues
        .into_iter()
        .partition(|warning| warning.should_be_an_error(flags));

    if warnings_that_are_errors.is_empty() {
        Ok(remaining_warnings)
    } else {
        Err(Error::CompatibilityError {
            warnings_as_errors: SeparatedBy {
                lines_of: warnings_that_are_errors,
                separator: "\n".to_string(),
            },
        })
    }
}
