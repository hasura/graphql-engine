pub mod aggregate_boolean_expressions;
pub mod aggregates;
pub mod apollo;
pub mod arguments;
pub mod boolean_expressions;
pub mod command_permissions;
pub mod commands;
mod conflicting_types;
pub mod data_connector_scalar_types;
pub mod data_connectors;
pub mod graphql_config;
pub mod model_permissions;
pub mod models;
pub mod models_graphql;
pub mod object_relationships;
pub mod object_types;
pub mod order_by_expressions;
pub mod plugins;
pub mod relationships;
pub mod relay;
pub mod roles;
pub mod scalar_boolean_expressions;
pub mod scalar_type_representations;
pub mod scalar_types;
pub mod type_permissions;
mod types;
pub mod views;

use command_permissions::CommandPermissionsOutput;
use model_permissions::ModelPermissionsOutput;
use open_dds::flags;
pub use types::Metadata;

use crate::flags::RuntimeFlags;
use crate::helpers::types::TrackGraphQLRootFields;
use crate::types::condition::Conditions;
use crate::types::configuration::Configuration;
use crate::types::error::{ContextualError, Error, SeparatedBy, ShouldBeAnError, WithContext};
use crate::types::warning::Warning;

/// This is where we take the input metadata and attempt to resolve a working `Metadata` object.
pub fn resolve(
    metadata: open_dds::Metadata,
    configuration: &Configuration,
) -> Result<(Metadata, Vec<Warning>), WithContext<Error>> {
    resolve_internal(metadata, configuration)
        .map_err(super::types::error::ContextualError::add_context_if_exists)
}

/// This is where we take the input metadata and attempt to resolve a working `Metadata` object.
fn resolve_internal(
    metadata: open_dds::Metadata,
    configuration: &Configuration,
) -> Result<(Metadata, Vec<Warning>), Error> {
    // all issues raised throughout metadata-resolve. These will be turned into `warnings` or
    // `errors` at the end of this function, depending on OpenDDS flags.
    let mut all_issues = vec![];
    let mut graphql_types = graphql_config::GraphqlTypeNames::new();

    // Create a empty tracked root fields
    let mut track_root_fields = TrackGraphQLRootFields::new();

    let metadata_accessor: open_dds::accessor::MetadataAccessor =
        open_dds::accessor::MetadataAccessor::new(metadata);

    // The graphql config represents the shape of the Hasura features in the graphql schema,
    // and which features should be enabled or disabled. We check this structure is valid.
    let graphql_config =
        graphql_config::resolve(&metadata_accessor.graphql_config, &metadata_accessor.flags)?;

    // Resolve SQL views and their dependencies
    let views::ViewsOutput { views, issues } = views::resolve(&metadata_accessor.views)?;

    all_issues.extend(issues);

    // Fetch and check schema information for all our data connectors
    let data_connectors::DataConnectorsOutput {
        data_connectors,
        issues,
    } = data_connectors::resolve(&metadata_accessor).map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Validate custom defined scalar types
    let scalar_types::ScalarTypesOutput {
        scalar_types,
        issues,
    } = scalar_types::resolve(&metadata_accessor, &mut graphql_types)
        .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Validate `DataConnectorScalarType` metadata.
    let data_connector_scalars = data_connector_scalar_types::resolve(
        &metadata_accessor,
        &data_connectors,
        &scalar_types,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    // Validate object types defined in metadata
    let object_types::ObjectTypesOutput {
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        object_types,
        issues,
    } = object_types::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &scalar_types,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Validate scalar `BooleanExpressionType`s
    let scalar_boolean_expressions::ScalarBooleanExpressionsOutput {
        boolean_expression_scalar_types,
        issues,
    } = scalar_boolean_expressions::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types,
        &scalar_types,
        &graphql_config,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // we de-dupe Conditions as we collect them, recording the hash
    // in their place
    let mut conditions = Conditions::new();

    // Fetch and validate permissions, and attach them to the relevant object types
    let (object_types_with_permissions, type_permission_issues) =
        type_permissions::resolve(&metadata_accessor, object_types, &mut conditions)
            .map_err(flatten_multiple_errors)?;

    all_issues.extend(type_permission_issues.into_iter().map(Warning::from));

    // collect raw relationships information
    let relationships = relationships::resolve(&metadata_accessor, &object_types_with_permissions)
        .map_err(flatten_multiple_errors)?;

    // Check aggregate expressions
    let aggregates::AggregateExpressionsOutput {
        aggregate_expressions,
        issues,
    } = aggregates::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        &scalar_types,
        &graphql_config,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    let aggregate_boolean_expressions::AggregateBooleanExpressionsOutput {
        scalar_aggregates,
        object_aggregates,
        issues,
    } = aggregate_boolean_expressions::resolve(
        &configuration.unstable_features,
        &metadata_accessor,
        &boolean_expression_scalar_types,
        &aggregate_expressions,
        &relationships,
        &object_types_with_permissions,
        &scalar_types,
        &graphql_config,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Resolve object boolean expression types
    let boolean_expressions::BooleanExpressionsOutput {
        boolean_expression_types,
        issues,
    } = boolean_expressions::resolve(
        &metadata_accessor,
        boolean_expression_scalar_types,
        object_aggregates,
        scalar_aggregates,
        &data_connectors,
        &data_connector_scalars,
        &graphql_config,
        &object_types_with_permissions,
        &relationships,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    let order_by_expressions::OrderByExpressionsOutput {
        mut order_by_expressions,
        issues,
    } = order_by_expressions::resolve(
        &metadata_accessor,
        &object_types_with_permissions,
        &relationships,
        &scalar_types,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Resolve models and their sources
    let models::ModelsOutput {
        models,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        issues,
    } = models::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        &object_types_with_permissions,
        &scalar_types,
        &boolean_expression_types,
        &aggregate_expressions,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    let commands::CommandsOutput { commands, issues } = commands::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_permissions,
        &mut track_root_fields,
        &scalar_types,
        &boolean_expression_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    apollo::resolve(apollo_federation_entity_enabled_types)?;

    relay::resolve(global_id_enabled_types)?;

    let object_relationships::ObjectRelationshipsOutput {
        object_types: object_types_with_relationships,
        issues,
    } = object_relationships::resolve(
        object_types_with_permissions,
        &relationships,
        &data_connectors,
        &data_connector_scalars,
        &models,
        &commands,
        &aggregate_expressions,
        &graphql_config,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // now we know about relationships, we can check our arguments (particularly, any
    // boolean expressions they use and whether their relationships are valid)
    let arguments::ArgumentsOutput { issues, arguments } = arguments::resolve(
        &commands,
        &models,
        &object_types_with_relationships,
        &scalar_types,
        &boolean_expression_types,
        &data_connector_scalars,
        &metadata_accessor.flags,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues.into_iter().map(Warning::from));

    // Resolve the filter expressions and graphql settings for models
    // This is a separate step so we can look up resolved models and their sources
    let models_graphql::ModelsWithGraphqlOutput {
        models_with_graphql,
        issues,
    } = models_graphql::resolve(
        &metadata_accessor,
        &models,
        &commands,
        &object_types_with_relationships,
        &arguments,
        &boolean_expression_types,
        &mut track_root_fields,
        &graphql_config,
        &scalar_types,
        &mut order_by_expressions,
        &mut graphql_types,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(issues);

    let CommandPermissionsOutput {
        permissions: commands_with_permissions,
        issues: command_permission_issues,
    } = command_permissions::resolve(
        &metadata_accessor,
        &commands,
        &object_types_with_relationships,
        &scalar_types,
        &arguments,
        &boolean_expression_types,
        &models_with_graphql,
        &data_connector_scalars,
        &mut conditions,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(command_permission_issues.into_iter().map(Warning::from));

    let ModelPermissionsOutput {
        permissions: models_with_permissions,
        issues: model_permission_issues,
    } = model_permissions::resolve(
        &metadata_accessor,
        &data_connectors,
        &data_connector_scalars,
        &object_types_with_relationships,
        &scalar_types,
        &models_with_graphql,
        &boolean_expression_types,
        &mut conditions,
    )
    .map_err(flatten_multiple_errors)?;

    all_issues.extend(model_permission_issues.into_iter().map(Warning::from));

    let roles = roles::resolve(
        &object_types_with_relationships,
        &models_with_permissions,
        &commands_with_permissions,
    );

    // include data connector information for each scalar type
    let scalar_types_with_representations =
        scalar_type_representations::resolve(&data_connector_scalars, &scalar_types);

    let plugin_configs = plugins::resolve(&metadata_accessor).map_err(flatten_multiple_errors)?;

    // check for duplicate names across types
    all_issues.extend(conflicting_types::check_conflicting_names_across_types(
        &scalar_types_with_representations,
        &object_types_with_relationships,
        &boolean_expression_types,
    ));

    let runtime_flags = RuntimeFlags::from_open_dds_flags(&metadata_accessor.flags);

    let all_warnings = warnings_as_errors_by_compatibility(&metadata_accessor.flags, all_issues)?;

    Ok((
        Metadata {
            scalar_types: scalar_types_with_representations,
            object_types: object_types_with_relationships,
            models: models_with_permissions,
            commands: commands_with_permissions,
            boolean_expression_types,
            order_by_expressions,
            aggregate_expressions,
            graphql_config: graphql_config.global,
            roles,
            plugin_configs,
            conditions,
            runtime_flags,
            views,
        },
        all_warnings,
    ))
}

// if a step returns multiple errors, add context and combine them
fn flatten_multiple_errors<E: Into<Error>>(errors: Vec<E>) -> Error {
    if errors.len() == 1 {
        errors.into_iter().next().unwrap().into()
    } else {
        Error::MultipleErrors {
            errors: SeparatedBy {
                lines_of: errors
                    .into_iter()
                    .map(derive_more::Into::into)
                    .map(ContextualError::add_context_if_exists)
                    .collect(),
            },
        }
    }
}

fn warnings_as_errors_by_compatibility(
    flags: &flags::OpenDdFlags,
    all_issues: Vec<Warning>,
) -> Result<Vec<Warning>, Error> {
    let (warnings_that_are_errors, remaining_warnings): (Vec<Warning>, Vec<Warning>) = all_issues
        .into_iter()
        .partition(|warning| warning.should_be_an_error(flags));

    if warnings_that_are_errors.is_empty() {
        Ok(remaining_warnings)
    } else {
        let mut errors = vec![];
        for warning in warnings_that_are_errors {
            errors.push(
                (Error::CompatibilityError {
                    warning_as_error: warning,
                })
                .add_context_if_exists(),
            );
        }
        Err(Error::MultipleErrors {
            errors: SeparatedBy { lines_of: errors },
        })
    }
}
