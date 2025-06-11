mod error;
mod filter;
mod graphql;
mod order_by;
mod types;

use crate::{ArgumentInfo, Warning};
pub use error::ModelGraphqlError;
use indexmap::IndexMap;
use open_dds::query::ArgumentName;
use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;
use std::sync::Arc;

use crate::helpers::types::TrackGraphQLRootFields;
use crate::stages::{
    arguments, boolean_expressions, commands, graphql_config, models, object_relationships,
    scalar_types,
};
use crate::types::subgraph::Qualified;

pub(crate) use types::ModelWithGraphql;
pub use types::{
    Model, ModelGraphQlApi, ModelGraphqlIssue, ModelOrderByExpression, ModelsWithGraphqlOutput,
    SelectAggregateGraphQlDefinition, SelectManyGraphQlDefinition, SelectUniqueGraphQlDefinition,
    SubscriptionGraphQlDefinition, UniqueIdentifierField,
};

use super::order_by_expressions;

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    arguments: &BTreeMap<arguments::ArgumentSource, IndexMap<ArgumentName, ArgumentInfo>>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    track_root_fields: &mut TrackGraphQLRootFields,
    graphql_config: &graphql_config::GraphqlConfig,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    order_by_expressions: &mut order_by_expressions::OrderByExpressions,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<ModelsWithGraphqlOutput, Vec<ModelGraphqlError>> {
    let mut models_with_graphql = IndexMap::new();
    let mut issues = vec![];

    let mut results = vec![];

    for (model_name, model) in models.clone() {
        results.push(resolve_model_with_graphql(
            model_name,
            model,
            metadata_accessor,
            object_types,
            arguments,
            boolean_expression_types,
            models,
            commands,
            track_root_fields,
            graphql_config,
            scalar_types,
            order_by_expressions,
            graphql_types,
            &mut models_with_graphql,
            &mut issues,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| types::ModelsWithGraphqlOutput {
        models_with_graphql,
        issues,
    })
}

fn resolve_model_with_graphql(
    model_name: Qualified<ModelName>,
    model: models::Model,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    arguments: &BTreeMap<arguments::ArgumentSource, IndexMap<ArgumentName, ArgumentInfo>>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    track_root_fields: &mut TrackGraphQLRootFields,
    graphql_config: &graphql_config::GraphqlConfig,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    order_by_expressions: &mut order_by_expressions::OrderByExpressions,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    models_with_graphql: &mut IndexMap<Qualified<ModelName>, ModelWithGraphql>,
    issues: &mut Vec<Warning>,
) -> Result<(), ModelGraphqlError> {
    let filter_expression_type = match &model.raw.filter_expression_type {
        Some(filter_expression_type_name) => {
            // We can only create a boolean expression if a source is attached to a model,
            // throw an error if this is not the case
            let model_source = match model.source {
                Some(ref source) => Ok(source),
                None => Err(
                    boolean_expressions::BooleanExpressionError::CannotUseFilterExpressionsWithoutSource {
                        model: model.name.clone(),
                    }
                ),
            }?;

            let (filter_expression_type, filter_issues) = filter::resolve_filter_expression_type(
                &model.name,
                &model.data_type,
                model_source,
                filter_expression_type_name,
                boolean_expression_types,
                object_types,
                models,
                &metadata_accessor.flags,
            )?;

            issues.extend(filter_issues.into_iter().map(Warning::from));

            Some(Arc::new(filter_expression_type))
        }
        None => None,
    };

    let order_by_expression = order_by::resolve_order_by_expression(
        &model,
        model.source.as_ref().map(AsRef::as_ref),
        object_types,
        models,
        commands,
        scalar_types,
        order_by_expressions,
        graphql_types,
        issues,
    )?;

    let graphql_api = match model.raw.graphql {
        Some(ref model_graphql_definition) => graphql::resolve_model_graphql_api(
            metadata_accessor,
            model_graphql_definition,
            &model,
            track_root_fields,
            model.raw.description.as_ref(),
            model.aggregate_expression.as_ref(),
            order_by_expression.as_ref(),
            order_by_expressions,
            graphql_config,
            graphql_types,
            issues,
        )?,
        None => types::ModelGraphQlApi::default(),
    };

    // use the resolved arguments
    let arguments = arguments
        .get(&arguments::ArgumentSource::Model(model.name.clone()))
        .cloned()
        .unwrap_or_default();

    let description = model.raw.description;

    let model = types::Model {
        path: model.path,
        name: model.name,
        data_type: model.data_type,
        type_fields: model.type_fields,
        aggregate_expression: model.aggregate_expression,
        source: model.source,
        apollo_federation_key_source: model.apollo_federation_key_source,
        global_id_source: model.global_id_source,
        global_id_fields: model.global_id_fields,
    };

    models_with_graphql.insert(
        model_name,
        types::ModelWithGraphql {
            inner: model,
            arguments,
            description,
            graphql_api,
            filter_expression_type,
        },
    );

    Ok(())
}
