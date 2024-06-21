use open_dds::aggregates::AggregateExpressionName;
use open_dds::data_connector::DataConnectorName;
pub use types::{
    ConnectorArgumentName, Model, ModelExpressionType, ModelGraphQlApi, ModelOrderByExpression,
    ModelSource, ModelsOutput, NDCFieldSourceMapping, SelectAggregateGraphQlDefinition,
    SelectManyGraphQlDefinition, SelectUniqueGraphQlDefinition,
};
mod aggregation;
mod filter;
mod graphql;
mod ordering;
mod source;
mod types;

pub use aggregation::resolve_aggregate_expression;
pub use source::get_ndc_column_for_comparison;

use crate::types::error::Error;

use crate::stages::{
    aggregates, boolean_expressions, data_connector_scalar_types, data_connectors, graphql_config,
    object_boolean_expressions, scalar_types, type_permissions,
};
use crate::types::subgraph::{mk_qualified_type_reference, ArgumentInfo, Qualified};

use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast};

use open_dds::{
    models::{ModelName, ModelV1},
    types::CustomTypeName,
};

use std::collections::{BTreeMap, BTreeSet};

/// resolve models
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,

    existing_graphql_types: &BTreeSet<ast::TypeName>,
    global_id_enabled_types: &BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ModelsOutput, Error> {
    // resolve models
    // TODO: validate types
    let mut models = IndexMap::new();
    let mut global_id_models = BTreeMap::new();
    let mut graphql_types = existing_graphql_types.clone();
    let mut global_id_enabled_types = global_id_enabled_types.clone();
    let mut apollo_federation_entity_enabled_types = apollo_federation_entity_enabled_types.clone();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: model,
    } in &metadata_accessor.models
    {
        let qualified_model_name = Qualified::new(subgraph.to_string(), model.name.clone());
        let mut resolved_model = resolve_model(
            subgraph,
            model,
            object_types,
            &mut global_id_enabled_types,
            &mut apollo_federation_entity_enabled_types,
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
            let resolved_model_source = source::resolve_model_source(
                model_source,
                &mut resolved_model,
                subgraph,
                data_connectors,
                data_connector_scalars,
                object_types,
                scalar_types,
                object_boolean_expression_types,
            )?;
            resolved_model.source = Some(resolved_model_source);
        }

        if let Some(filter_expression_type) = &model.filter_expression_type {
            // We can only create a boolean expression if a source is attached to a model,
            // throw an error if this is not the case
            let model_source = match resolved_model.source {
                Some(ref source) => Ok(source),
                None => Err(Error::CannotUseFilterExpressionsWithoutSource {
                    model: Qualified::new(subgraph.to_string(), model.name.clone()),
                }),
            }?;

            let resolved_expression = filter::resolve_filter_expression_type(
                &resolved_model.name,
                model_source,
                &resolved_model.data_type,
                &Qualified::new(subgraph.to_string(), filter_expression_type.clone()),
                object_boolean_expression_types,
                boolean_expression_types,
            )?;
            resolved_model.filter_expression_type = Some(resolved_expression);
        }

        let qualified_aggregate_expression_name = model
            .aggregate_expression
            .as_ref()
            .map(|aggregate_expression_name| {
                aggregation::resolve_aggregate_expression(
                    aggregate_expression_name,
                    &qualified_model_name,
                    &resolved_model.data_type,
                    &resolved_model.source,
                    aggregate_expressions,
                    object_types,
                )
            })
            .transpose()?;

        if let Some(model_graphql_definition) = &model.graphql {
            let graphql_api = graphql::resolve_model_graphql_api(
                model_graphql_definition,
                &mut resolved_model,
                &mut graphql_types,
                data_connector_scalars,
                &model.description,
                &qualified_aggregate_expression_name,
                graphql_config,
            )?;
            resolved_model.graphql_api = graphql_api;
        }

        if models
            .insert(qualified_model_name.clone(), resolved_model)
            .is_some()
        {
            return Err(Error::DuplicateModelDefinition {
                name: qualified_model_name,
            });
        }
    }
    Ok(ModelsOutput {
        models,
        graphql_types,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
    })
}

fn resolve_model(
    subgraph: &str,
    model: &ModelV1,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    global_id_enabled_types: &mut BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<ModelName>>,
    >,
) -> Result<Model, Error> {
    let qualified_object_type_name =
        Qualified::new(subgraph.to_string(), model.object_type.clone());
    let qualified_model_name = Qualified::new(subgraph.to_string(), model.name.clone());
    let object_type_representation = source::get_model_object_type_representation(
        object_types,
        &qualified_object_type_name,
        &qualified_model_name,
    )?;
    let mut global_id_source = None;
    if model.global_id_source {
        // Check if there are any global fields present in the related
        // object type, if the model is marked as a global source.
        if object_type_representation
            .object_type
            .global_id_fields
            .is_empty()
        {
            return Err(Error::NoGlobalFieldsPresentInGlobalIdSource {
                type_name: qualified_object_type_name,
                model_name: model.name.clone(),
            });
        }
        if !model.arguments.is_empty() {
            return Err(Error::ModelWithArgumentsAsGlobalIdSource {
                model_name: qualified_model_name,
            });
        }
        // model has `global_id_source`; insert into the BTreeMap of `global_id_enabled_types`
        match global_id_enabled_types.get_mut(&qualified_object_type_name) {
            None => {
                // this shouldn't happen; but for some reason the object type
                // containing globalIdFields is not inserted. Insert it now
                global_id_enabled_types.insert(
                    qualified_object_type_name.clone(),
                    vec![qualified_model_name.clone()],
                );
            }
            Some(model_names) => {
                model_names.push(qualified_model_name.clone());
            }
        }
        global_id_source = Some(NDCFieldSourceMapping {
            ndc_mapping: BTreeMap::new(),
        });
    };
    let mut apollo_federation_key_source = None;
    if model
        .graphql
        .as_ref()
        .and_then(|g| g.apollo_federation.as_ref().map(|a| a.entity_source))
        .unwrap_or_default()
    {
        // Check if there are any apollo federation keys present in the related
        // object type, if the model is marked as an apollo federation entity source.
        if object_type_representation
            .object_type
            .apollo_federation_config
            .is_some()
        {
            if !model.arguments.is_empty() {
                return Err(Error::ModelWithArgumentsAsApolloFederationEntitySource {
                    model_name: qualified_model_name,
                });
            }
            // model has `apollo_federation_entity_source`; insert into the BTreeMap of
            // `apollo_federation_entity_enabled_types`
            match apollo_federation_entity_enabled_types.get_mut(&qualified_object_type_name) {
                None => {
                    // the model's graphql configuration has `apollo_federation.entitySource` but the object type
                    // of the model doesn't have any apollo federation keys
                    return Err(Error::NoKeysFieldsPresentInEntitySource {
                        type_name: qualified_object_type_name,
                        model_name: model.name.clone(),
                    });
                }
                Some(type_name) => {
                    match type_name {
                        None => {
                            *type_name = Some(qualified_model_name.clone());
                        }
                        // Multiple models are marked as apollo federation entity source
                        Some(_) => {
                            return Err(Error::MultipleEntitySourcesForType {
                                type_name: qualified_object_type_name,
                            });
                        }
                    }
                }
            }
            apollo_federation_key_source = Some(NDCFieldSourceMapping {
                ndc_mapping: BTreeMap::new(),
            });
        }
    }

    let mut arguments = IndexMap::new();
    for argument in &model.arguments {
        if arguments
            .insert(
                argument.name.clone(),
                ArgumentInfo {
                    argument_type: mk_qualified_type_reference(&argument.argument_type, subgraph),
                    description: argument.description.clone(),
                },
            )
            .is_some()
        {
            return Err(Error::DuplicateModelArgumentDefinition {
                model_name: qualified_model_name,
                argument_name: argument.name.clone(),
            });
        }
    }

    Ok(Model {
        name: qualified_model_name,
        data_type: qualified_object_type_name,
        type_fields: object_type_representation.object_type.fields.clone(),
        global_id_fields: object_type_representation
            .object_type
            .global_id_fields
            .clone(),
        arguments,
        graphql_api: ModelGraphQlApi::default(),
        source: None,
        global_id_source,
        apollo_federation_key_source,
        filter_expression_type: None,
        orderable_fields: ordering::resolve_orderable_fields(
            model,
            &object_type_representation.object_type.fields,
        )?,
    })
}
