use lang_graphql::ast::common as ast;
use open_dds::identifier::SubgraphName;
use open_dds::order_by_expression::{OrderByExpressionName, OrderByExpressionOrderableField};
pub use types::{Model, ModelRaw, ModelSource, ModelsIssue, ModelsOutput, NDCFieldSourceMapping};
mod aggregation;
mod error;
mod helpers;
mod source;
mod types;
pub use error::ModelsError;

pub use crate::helpers::argument::get_argument_kind;
use crate::helpers::types::store_new_graphql_type;
use crate::{mk_name, OrderByExpression};
pub use aggregation::resolve_aggregate_expression;
pub use helpers::get_ndc_column_for_comparison;

use crate::stages::{
    aggregates, apollo, boolean_expressions, data_connector_scalar_types, data_connectors,
    object_boolean_expressions, relay, scalar_types, type_permissions,
};
use crate::types::subgraph::{mk_qualified_type_reference, ArgumentInfo, Qualified};

use indexmap::IndexMap;

use open_dds::{
    aggregates::AggregateExpressionName, data_connector::DataConnectorName, models::ModelName,
    types::CustomTypeName,
};

use std::collections::{BTreeMap, BTreeSet};

use super::order_by_expressions::{
    resolve_orderable_fields, OrderByExpressionGraphqlConfig, OrderByExpressionIdentifier,
    OrderByExpressions, OrderableRelationships,
};

/// resolve models and their sources
/// we check aggregations, filtering and graphql in the next stage (`models_graphql`)
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    mut global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    mut apollo_federation_entity_enabled_types: BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    mut order_by_expressions: OrderByExpressions,
    mut graphql_types: BTreeSet<ast::TypeName>,
) -> Result<ModelsOutput, ModelsError> {
    // resolve models
    // TODO: validate types
    let mut models = IndexMap::new();
    let mut global_id_models = BTreeMap::new();
    let mut issues = vec![];

    let order_by_expression_names_and_types: BTreeMap<OrderByExpressionName, CustomTypeName> =
        metadata_accessor
            .order_by_expressions
            .iter()
            .map(|o| (o.object.name.clone(), o.object.ordered_type.clone()))
            .collect();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: model,
    } in &metadata_accessor.models
    {
        let qualified_model_name = Qualified::new(subgraph.clone(), model.name().clone());
        let mut resolved_model = resolve_model(
            subgraph,
            model,
            object_types,
            object_boolean_expression_types,
            boolean_expression_types,
            &order_by_expression_names_and_types,
            &mut global_id_enabled_types,
            &mut apollo_federation_entity_enabled_types,
            &mut order_by_expressions,
            &mut graphql_types,
        )?;
        if resolved_model.global_id_source.is_some() {
            match global_id_models.insert(
                resolved_model.data_type.clone(),
                resolved_model.name.clone(),
            ) {
                None => {}
                Some(duplicate_model_name) => {
                    return Err(ModelsError::from(
                        relay::RelayError::DuplicateModelGlobalIdSource {
                            model_1: resolved_model.name,
                            model_2: duplicate_model_name,
                            object_type: resolved_model.data_type,
                        },
                    ))
                }
            }
        }

        if let Some(model_source) = &model.source() {
            let (resolved_model_source, model_source_issues) = source::resolve_model_source(
                model_source,
                &mut resolved_model,
                subgraph,
                data_connectors,
                data_connector_scalars,
                object_types,
                scalar_types,
                object_boolean_expression_types,
                boolean_expression_types,
            )?;
            resolved_model.source = Some(resolved_model_source);
            issues.extend(model_source_issues);
        }

        let qualified_aggregate_expression_name = model
            .aggregate_expression()
            .as_ref()
            .map(|aggregate_expression_name| {
                aggregation::resolve_aggregate_expression(
                    &Qualified::new(subgraph.clone(), aggregate_expression_name.clone()),
                    &qualified_model_name,
                    &resolved_model.data_type,
                    &resolved_model.source,
                    aggregate_expressions,
                    object_types,
                )
            })
            .transpose()?;

        resolved_model.aggregate_expression = qualified_aggregate_expression_name;

        if models
            .insert(qualified_model_name.clone(), resolved_model)
            .is_some()
        {
            return Err(ModelsError::DuplicateModelDefinition {
                name: qualified_model_name,
            });
        }
    }
    Ok(ModelsOutput {
        models,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        order_by_expressions,
        graphql_types,
        issues,
    })
}

fn resolve_model(
    subgraph: &SubgraphName,
    model: &open_dds::models::Model,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, CustomTypeName>,
    global_id_enabled_types: &mut BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<ModelName>>,
    >,
    order_by_expressions: &mut OrderByExpressions,
    graphql_types: &mut BTreeSet<ast::TypeName>,
) -> Result<Model, ModelsError> {
    let qualified_object_type_name = Qualified::new(subgraph.clone(), model.object_type().clone());
    let qualified_model_name = Qualified::new(subgraph.clone(), model.name().clone());
    let object_type_representation = source::get_model_object_type_representation(
        object_types,
        &qualified_object_type_name,
        &qualified_model_name,
    )?;
    let mut global_id_source = None;
    if model.global_id_source() {
        // Check if there are any global fields present in the related
        // object type, if the model is marked as a global source.
        if object_type_representation
            .object_type
            .global_id_fields
            .is_empty()
        {
            return Err(ModelsError::from(
                relay::RelayError::NoGlobalFieldsPresentInGlobalIdSource {
                    type_name: qualified_object_type_name,
                    model_name: model.name().clone(),
                },
            ));
        }
        if !model.arguments().is_empty() {
            return Err(ModelsError::from(
                relay::RelayError::ModelWithArgumentsAsGlobalIdSource {
                    model_name: qualified_model_name,
                },
            ));
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
    let graphql = model.graphql();
    if graphql
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
            if !model.arguments().is_empty() {
                return Err(ModelsError::from(
                    apollo::ApolloError::ModelWithArgumentsAsApolloFederationEntitySource {
                        model_name: qualified_model_name,
                    },
                ));
            }
            // model has `apollo_federation_entity_source`; insert into the BTreeMap of
            // `apollo_federation_entity_enabled_types`
            match apollo_federation_entity_enabled_types.get_mut(&qualified_object_type_name) {
                None => {
                    // the model's graphql configuration has `apollo_federation.entitySource` but the object type
                    // of the model doesn't have any apollo federation keys
                    return Err(ModelsError::from(
                        apollo::ApolloError::NoKeysFieldsPresentInEntitySource {
                            type_name: qualified_object_type_name,
                            model_name: model.name().clone(),
                        },
                    ));
                }
                Some(type_name) => {
                    match type_name {
                        None => {
                            *type_name = Some(qualified_model_name.clone());
                        }
                        // Multiple models are marked as apollo federation entity source
                        Some(_) => {
                            return Err(ModelsError::from(
                                apollo::ApolloError::MultipleEntitySourcesForType {
                                    type_name: qualified_object_type_name,
                                },
                            ));
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
    for argument in model.arguments() {
        // is this an expression or not?
        let argument_kind = get_argument_kind(
            &argument.argument_type,
            subgraph,
            object_boolean_expression_types,
            boolean_expression_types,
        );

        if arguments
            .insert(
                argument.name.clone(),
                ArgumentInfo {
                    argument_type: mk_qualified_type_reference(&argument.argument_type, subgraph),
                    argument_kind,
                    description: argument.description.clone(),
                },
            )
            .is_some()
        {
            return Err(ModelsError::DuplicateModelArgumentDefinition {
                model_name: qualified_model_name,
                argument_name: argument.name.clone(),
            });
        }
    }

    let model_raw = ModelRaw {
        description: model.description().clone(),
        filter_expression_type: model
            .filter_expression_type()
            .as_ref()
            .map(|filter_name| Qualified::new(subgraph.clone(), filter_name.clone())),
        graphql,
    };

    let order_by_expression = match model {
        open_dds::models::Model::V1(model_v1) => Ok(Some(make_order_by_expression(
            model_v1,
            order_by_expression_names_and_types,
            subgraph,
            object_types,
            graphql_types,
            &qualified_model_name,
            order_by_expressions,
        )?)),
        // Check that the order_by_expression exists and refers to the correct object type for the model
        open_dds::models::Model::V2(model_v2) => {
            if let Some(order_by_expression_name) = model_v2.order_by_expression.as_ref() {
                let order_by_expression_identifier = Qualified::new(
                    subgraph.clone(),
                    OrderByExpressionIdentifier::FromOrderByExpression(
                        order_by_expression_name.clone(),
                    ),
                );
                if let Some(order_by_expression) =
                    order_by_expressions.0.get(&order_by_expression_identifier)
                {
                    if order_by_expression.ordered_type == qualified_object_type_name {
                        Ok(Some(order_by_expression_identifier))
                    } else {
                        Err(ModelsError::OrderByExpressionTypeMismatch {
                            model_name: qualified_model_name.clone(),
                            model_type: qualified_object_type_name.clone(),
                            order_by_expression_name: Qualified::new(
                                subgraph.clone(),
                                order_by_expression_name.clone(),
                            ),
                            order_by_expression_type: order_by_expression.ordered_type.clone(),
                        })
                    }
                } else {
                    Err(ModelsError::UnknownOrderByExpressionIdentifier {
                        model_name: qualified_model_name.clone(),
                        order_by_expression_identifier: order_by_expression_identifier.clone(),
                    })
                }
            } else {
                Ok(None)
            }
        }
    }?;

    Ok(Model {
        name: qualified_model_name,
        data_type: qualified_object_type_name,
        order_by_expression,
        aggregate_expression: None, // we fill this in once we have resolved the model source
        type_fields: object_type_representation.object_type.fields.clone(),
        global_id_fields: object_type_representation
            .object_type
            .global_id_fields
            .clone(),
        arguments,
        source: None,
        global_id_source,
        apollo_federation_key_source,
        raw: model_raw,
    })
}

fn make_order_by_expression(
    model_v1: &open_dds::models::ModelV1,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, CustomTypeName>,
    subgraph: &SubgraphName,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    qualified_model_name: &Qualified<ModelName>,
    order_by_expressions: &mut OrderByExpressions,
) -> Result<Qualified<OrderByExpressionIdentifier>, ModelsError> {
    let identifier = Qualified::new(
        subgraph.clone(),
        OrderByExpressionIdentifier::FromModel(model_v1.name.clone()),
    );
    let ordered_type = Qualified::new(subgraph.clone(), model_v1.object_type.clone());
    let open_dds_orderable_fields = model_v1
        .orderable_fields
        .iter()
        .map(|orderable_field| OrderByExpressionOrderableField {
            field_name: orderable_field.field_name.clone(),
            enable_order_by_directions: Some(orderable_field.order_by_directions.clone()),
            order_by_expression: None,
        })
        .collect::<Vec<_>>();
    let orderable_fields = resolve_orderable_fields(
        subgraph,
        object_types,
        &ordered_type,
        order_by_expression_names_and_types,
        &open_dds_orderable_fields,
    )
    .map_err(|error| ModelsError::ModelV1OrderableFieldsError {
        model_name: qualified_model_name.clone(),
        error,
    })?;
    let graphql = model_v1
        .graphql
        .as_ref()
        .and_then(|g| {
            g.order_by_expression_type.as_ref().map(|type_name| {
                let expression_type_name = mk_name(type_name.as_str()).map(ast::TypeName)?;
                store_new_graphql_type(graphql_types, Some(&expression_type_name))?;
                Ok::<_, ModelsError>(OrderByExpressionGraphqlConfig {
                    expression_type_name,
                })
            })
        })
        .transpose()?;

    let order_by_expression = OrderByExpression {
        identifier: identifier.clone(),
        ordered_type,
        orderable_fields,
        orderable_relationships: OrderableRelationships::ModelV1AllowAll,
        graphql,
        description: Some(format!(
            "OrderByExpression for Model {qualified_model_name}"
        )),
    };
    order_by_expressions
        .0
        .insert(identifier.clone(), order_by_expression);
    Ok(identifier)
}
