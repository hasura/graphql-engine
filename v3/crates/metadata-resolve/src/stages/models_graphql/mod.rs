mod filter;
mod graphql;
mod types;
use crate::types::subgraph::Qualified;
mod aggregation;

use crate::stages::{
    aggregates, boolean_expressions, data_connector_scalar_types, graphql_config, models,
    object_boolean_expressions, type_permissions,
};
use crate::types::error::Error;
pub use aggregation::resolve_aggregate_expression;
pub use types::{
    ModelExpressionType, ModelGraphQlApi, ModelOrderByExpression, ModelWithGraphql,
    ModelsGraphqlOutput, SelectAggregateGraphQlDefinition, SelectManyGraphQlDefinition,
    SelectUniqueGraphQlDefinition,
};

use open_dds::{
    aggregates::AggregateExpressionName, data_connector::DataConnectorName, models::ModelName,
    types::CustomTypeName,
};

use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast};
use std::collections::{BTreeMap, BTreeSet};

pub fn resolve(
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    existing_graphql_types: &BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ModelsGraphqlOutput, Error> {
    let mut models_with_graphql = IndexMap::new();
    let mut graphql_types = existing_graphql_types.clone();

    for (model_name, model) in models.clone() {
        let filter_expression_type = match &model.raw.filter_expression_type {
            Some(filter_expression_type_name) => {
                // We can only create a boolean expression if a source is attached to a model,
                // throw an error if this is not the case
                let model_source = match model.source {
                    Some(ref source) => Ok(source),
                    None => Err(Error::CannotUseFilterExpressionsWithoutSource {
                        model: model.name.clone(),
                    }),
                }?;

                Some(filter::resolve_filter_expression_type(
                    &model.name,
                    model_source,
                    &model.data_type,
                    filter_expression_type_name,
                    object_boolean_expression_types,
                    boolean_expression_types,
                )?)
            }
            None => None,
        };

        let qualified_aggregate_expression_name = model
            .raw
            .aggregate_expression
            .as_ref()
            .map(|aggregate_expression_name| {
                aggregation::resolve_aggregate_expression(
                    aggregate_expression_name,
                    &model.name,
                    &model.data_type,
                    &model.source,
                    aggregate_expressions,
                    object_types,
                )
            })
            .transpose()?;

        let graphql_api = match model.raw.graphql {
            Some(ref model_graphql_definition) => graphql::resolve_model_graphql_api(
                model_graphql_definition,
                &model,
                &mut graphql_types,
                data_connector_scalars,
                &model.raw.description,
                &qualified_aggregate_expression_name,
                graphql_config,
            )?,
            None => ModelGraphQlApi::default(),
        };

        models_with_graphql.insert(
            model_name,
            ModelWithGraphql {
                inner: model,
                graphql_api,
                filter_expression_type,
            },
        );
    }

    Ok(ModelsGraphqlOutput {
        models_with_graphql,
        graphql_types,
    })
}
