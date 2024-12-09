use std::sync::Arc;

use open_dds::aggregates::AggregateExpressionName;
use open_dds::data_connector::DataConnectorName;
use open_dds::models::{ModelGraphQlDefinitionV2, ModelName};
use open_dds::relationships::{ModelRelationshipTarget, RelationshipTarget};

use super::types::{
    LimitFieldGraphqlConfig, ModelGraphQlApi, ModelGraphqlApiArgumentsConfig, ModelGraphqlIssue,
    ModelOrderByExpression, OffsetFieldGraphqlConfig, OrderByExpressionInfo,
    SelectAggregateGraphQlDefinition, SelectManyGraphQlDefinition, SelectUniqueGraphQlDefinition,
    SubscriptionGraphQlDefinition, UniqueIdentifierField,
};
use crate::helpers::types::{mk_name, store_new_graphql_type, TrackGraphQLRootFields};
use crate::stages::order_by_expressions::OrderByExpressions;
use crate::stages::{data_connector_scalar_types, graphql_config, models, object_types};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use crate::Warning;
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast};

use std::collections::{BTreeMap, BTreeSet};

pub(crate) fn resolve_model_graphql_api(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    model_graphql_definition: &ModelGraphQlDefinitionV2,
    model: &models::Model,
    existing_graphql_types: &mut BTreeSet<ast::TypeName>,
    track_root_fields: &mut TrackGraphQLRootFields,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    model_description: Option<&String>,
    aggregate_expression_name: Option<&Qualified<AggregateExpressionName>>,
    order_by_expressions: &OrderByExpressions,
    graphql_config: &graphql_config::GraphqlConfig,
    issues: &mut Vec<Warning>,
) -> Result<ModelGraphQlApi, Error> {
    let model_name = &model.name;
    let mut graphql_api = ModelGraphQlApi::default();

    for select_unique in &model_graphql_definition.select_uniques {
        let mut unique_identifier_fields = IndexMap::new();
        for field_name in &select_unique.unique_identifier {
            let field_type = &model
                .type_fields
                .get(field_name)
                .ok_or_else(|| Error::UnknownFieldInUniqueIdentifier {
                    model_name: model_name.clone(),
                    field_name: field_name.clone(),
                })?
                .field_type;

            let ndc_column = model
                .source
                .as_ref()
                .map(|model_source| {
                    models::get_ndc_column_for_comparison(
                        &model.name,
                        &model.data_type,
                        model_source,
                        field_name,
                        data_connector_scalars,
                        || "the unique identifier for select unique".to_string(),
                    )
                })
                .transpose()?;

            let unique_identifier_field = UniqueIdentifierField {
                field_type: field_type.clone(),
                ndc_column,
            };
            if unique_identifier_fields
                .insert(field_name.clone(), unique_identifier_field)
                .is_some()
            {
                return Err(Error::DuplicateFieldInUniqueIdentifier {
                    model_name: model_name.clone(),
                    field_name: field_name.clone(),
                });
            }
        }
        let select_unique_field_name = mk_name(select_unique.query_root_field.as_str())?;
        // Let's track and check if the select_unique field name is already used
        track_root_fields
            .track_query_root_field(&select_unique_field_name)
            .unwrap_or_else(|error| {
                issues.push(Warning::from(ModelGraphqlIssue::DuplicateRootField {
                    model_name: model_name.clone(),
                    error,
                }));
            });
        let select_unique_description = if select_unique.description.is_some() {
            select_unique.description.clone()
        } else {
            model_description.as_ref().map(|description| {
                format!("Selects a single object from the model. Model description: {description}")
            })
        };
        let subscription = select_unique
            .subscription
            .as_ref()
            .map(|s| resolve_subscription_graphql_api(s, model_name, track_root_fields, issues))
            .transpose()?;
        graphql_api
            .select_uniques
            .push(SelectUniqueGraphQlDefinition {
                query_root_field: select_unique_field_name,
                unique_identifier: unique_identifier_fields,
                description: select_unique_description,
                deprecated: select_unique.deprecated.clone(),
                subscription,
            });
    }

    graphql_api.order_by_expression = model
        .source
        .as_ref()
        .map(
            |model_source: &Arc<models::ModelSource>| -> Result<Option<ModelOrderByExpression>, Error> {
                let order_by_expression = model.order_by_expression.as_ref().map(|n|
                    order_by_expressions.objects.get(n)
                    .ok_or_else(|| models::ModelsError::UnknownOrderByExpressionIdentifier {
                        model_name: model.name.clone(),
                        order_by_expression_identifier: n.clone()
                    })).transpose()?;
                // TODO: (paritosh) should we check for conflicting graphql types for default order_by type name as well?
                order_by_expression
                    .and_then(|order_by_expression| {
                        order_by_expression.graphql.clone()
                        .map(|graphql| {
                        let object_types::TypeMapping::Object { field_mappings, .. } = model_source
                            .type_mappings
                            .get(&model.data_type)
                            .ok_or(models::ModelsError::TypeMappingRequired {
                                model_name: model_name.clone(),
                                type_name: model.data_type.clone(),
                                data_connector: model_source.data_connector.name.clone(),
                            })?;

                        let mut order_by_fields = BTreeMap::new();
                        for (field_name, field_mapping) in field_mappings {
                            // fields with arguments are not allowed in sorting expression
                            if field_mapping.argument_mappings.is_empty() {
                                order_by_fields.insert(
                                    field_name.clone(),
                                OrderByExpressionInfo {
                                        ndc_column: field_mapping.column.clone(),
                                    },
                                );
                            }
                        }

                        match &graphql_config.query.order_by_field_name {
                            None => Err(Error::GraphqlConfigError {
                                graphql_config_error:
                                    graphql_config::GraphqlConfigError::MissingOrderByInputFieldInGraphqlConfig,
                            }),
                            Some(order_by_field_name) => Ok(ModelOrderByExpression {
                                data_connector_name: model_source.data_connector.name.clone(),
                                order_by_type_name: graphql.expression_type_name,
                                order_by_field_name: order_by_field_name.clone(),
                                order_by_expression_identifier: order_by_expression.identifier.clone(),
                            }),
                        }
                    })})
                    .transpose()
            },
        )
        .transpose()?
        .flatten();

    // record select_many root field
    graphql_api.select_many = match &model_graphql_definition.select_many {
        None => Ok(None),
        Some(gql_definition) => {
            let subscription = gql_definition
                .subscription
                .as_ref()
                .map(|s| resolve_subscription_graphql_api(s, model_name, track_root_fields, issues))
                .transpose()?;
            mk_name(gql_definition.query_root_field.as_str()).map(|f: ast::Name| {
                // Let's track and check if the select_many field name is already used
                track_root_fields.track_query_root_field(&f).unwrap_or_else(|error| {
                    issues.push(Warning::from(ModelGraphqlIssue::DuplicateRootField {
                        model_name: model_name.clone(),
                        error,
                    }));
                });
                let select_many_description = if gql_definition.description.is_some() {
                    gql_definition.description.clone()
                } else {
                    model_description.as_ref().map(|description| {
                        format!(
                            "Selects multiple objects from the model. Model description: {description}"
                        )
                    })
                };
                Some(SelectManyGraphQlDefinition {
                    query_root_field: f,
                    description: select_many_description,
                    deprecated: gql_definition.deprecated.clone(),
                    subscription,
                })
            })
        }
    }?;

    // record the filter input type name, if set
    let filter_input_type_name = model_graphql_definition
        .filter_input_type_name
        .as_ref()
        .map(|filter_input_type_name| mk_name(filter_input_type_name.as_str()).map(ast::TypeName))
        .transpose()?;
    store_new_graphql_type(existing_graphql_types, filter_input_type_name.as_ref())?;
    graphql_api.filter_input_type_name = filter_input_type_name;

    let aggregates_are_used_with_this_model_type = aggregate_expression_name.is_some()
        || is_model_used_in_any_aggregate_relationship(metadata_accessor, &model.name);

    // If aggregates are not used with this model type then we don't need a
    // filter input type name
    if graphql_api.filter_input_type_name.is_some() && !aggregates_are_used_with_this_model_type {
        return Err(Error::UnnecessaryFilterInputTypeNameGraphqlConfiguration {
            model_name: model_name.clone(),
        });
    }
    // But if they are used, then we need a filter input type name
    else if graphql_api.filter_input_type_name.is_none()
        && aggregates_are_used_with_this_model_type
    {
        return Err(Error::MissingFilterInputTypeNameGraphqlConfiguration {
            model_name: model_name.clone(),
        });
    }

    // record select_aggregate root field
    graphql_api.select_aggregate = match (
        &model_graphql_definition.aggregate,
        aggregate_expression_name,
        &graphql_config.query.aggregate_config,
    ) {
        (Some(_graphql_aggregate), Some(_aggregate_expression_name), None) => {
            // If the user has an aggregate expression and has specified the graphql select aggregate root field
            // but is missing the global aggregate GraphqlConfig, this is probably a mistake and so let's raise
            // a warning for them
            issues.push(Warning::from(
                ModelGraphqlIssue::MissingAggregateFilterInputFieldNameInGraphqlConfig {
                    model_name: model_name.clone(),
                },
            ));
            None
        }
        (Some(graphql_aggregate), Some(aggregate_expression_name), Some(aggregate_config)) => {
            let subscription = graphql_aggregate
                .subscription
                .as_ref()
                .map(|s| resolve_subscription_graphql_api(s, model_name, track_root_fields, issues))
                .transpose()?;

            let aggregate_root_field = mk_name(graphql_aggregate.query_root_field.as_str())?;
            // Let's track and check if the select_aggregate field name is already used
            track_root_fields
                .track_query_root_field(&aggregate_root_field)
                .unwrap_or_else(|error| {
                    issues.push(Warning::from(ModelGraphqlIssue::DuplicateRootField {
                        model_name: model_name.clone(),
                        error,
                    }));
                });
            Some(SelectAggregateGraphQlDefinition {
                query_root_field: aggregate_root_field,
                description: graphql_aggregate.description.clone(),
                deprecated: graphql_aggregate.deprecated.clone(),
                aggregate_expression_name: aggregate_expression_name.clone(),
                filter_input_field_name: aggregate_config.filter_input_field_name.clone(),
                subscription,
            })
        }
        _ => None,
    };

    // record limit and offset field names
    graphql_api.limit_field = graphql_config
        .query
        .limit_field_name
        .as_ref()
        .map(|limit_field| LimitFieldGraphqlConfig {
            field_name: limit_field.clone(),
        });

    graphql_api.offset_field =
        graphql_config
            .query
            .offset_field_name
            .as_ref()
            .map(|offset_field| OffsetFieldGraphqlConfig {
                field_name: offset_field.clone(),
            });

    if model.arguments.is_empty() {
        if model_graphql_definition.arguments_input_type.is_some() {
            return Err(Error::UnnecessaryModelArgumentsGraphQlInputConfiguration {
                model_name: model_name.clone(),
            });
        }
    } else {
        let arguments_input_type_name = match &model_graphql_definition.arguments_input_type {
            None => Ok(None),
            Some(type_name) => mk_name(type_name.as_str()).map(ast::TypeName).map(Some),
        }?;
        store_new_graphql_type(existing_graphql_types, arguments_input_type_name.as_ref())?;

        if let Some(type_name) = arguments_input_type_name {
            let argument_input_field_name = graphql_config
                .query
                .arguments_field_name
                .as_ref()
                .ok_or_else(|| Error::GraphqlConfigError {
                    graphql_config_error:
                        graphql_config::GraphqlConfigError::MissingArgumentsInputFieldInGraphqlConfig,
                })?;
            graphql_api.arguments_input_config = Some(ModelGraphqlApiArgumentsConfig {
                field_name: argument_input_field_name.clone(),
                type_name,
            });
        }
    }

    Ok(graphql_api)
}

fn is_model_used_in_any_aggregate_relationship(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    model_name: &Qualified<ModelName>,
) -> bool {
    // Check all relationships to see if any target this model and if they
    // define an aggregate
    metadata_accessor
        .relationships
        .iter()
        .any(|relationship| match &relationship.object.target {
            RelationshipTarget::Model(
                target @ ModelRelationshipTarget {
                    aggregate: Some(_), // If the relationship defines an aggregate
                    name,
                    ..
                },
            ) => {
                // And the target of the relationship is this model
                target
                    .subgraph()
                    .is_some_and(|subgraph| subgraph == model_name.subgraph)
                    && *name == model_name.name
            }
            _ => false,
        })
}

fn resolve_subscription_graphql_api(
    subscription: &open_dds::models::SubscriptionGraphQlDefinition,
    model_name: &Qualified<ModelName>,
    track_root_fields: &mut TrackGraphQLRootFields,
    issues: &mut Vec<Warning>,
) -> Result<SubscriptionGraphQlDefinition, Error> {
    let open_dds::models::SubscriptionGraphQlDefinition {
        root_field,
        description,
        deprecated,
        polling_interval_ms,
    } = subscription;
    let root_field_name = mk_name(root_field.as_str())?;
    // Let's track and check if the subscription root field name is already used
    track_root_fields
        .track_subscription_root_field(&root_field_name)
        .unwrap_or_else(|error| {
            issues.push(Warning::from(ModelGraphqlIssue::DuplicateRootField {
                model_name: model_name.clone(),
                error,
            }));
        });
    Ok(SubscriptionGraphQlDefinition {
        root_field: root_field_name,
        description: description.clone(),
        deprecated: deprecated.clone(),
        polling_interval_ms: *polling_interval_ms,
    })
}
