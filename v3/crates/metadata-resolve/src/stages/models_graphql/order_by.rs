use crate::helpers::types::store_new_graphql_type;
use crate::Warning;
use crate::{mk_name, QualifiedTypeName};
use lang_graphql::ast::common as ast;
use open_dds::types::CustomTypeName;

use crate::stages::{models, object_relationships, order_by_expressions, scalar_types};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;

use open_dds::{models::ModelName, types::GraphQlTypeName};

use std::collections::{BTreeMap, BTreeSet};

use super::order_by_expressions::{
    OrderByExpressionError, OrderByExpressionGraphqlConfig, OrderByExpressionIdentifier,
    OrderByExpressions, OrderableField, OrderableScalarField, ScalarOrderByExpression,
};

pub fn resolve_order_by_expression(
    model: &models::Model,
    model_source: Option<&models::ModelSource>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    order_by_expressions: &mut OrderByExpressions,
    issues: &mut Vec<Warning>,
) -> Result<Option<Qualified<OrderByExpressionIdentifier>>, models::ModelsError> {
    match &model.raw.order_by {
        models::ModelOrderBy::ModelV1 {
            graphql_type_name,
            orderable_fields: model_v1_orderable_fields,
        } => {
            let object_type_representation =
                object_types.get(&model.data_type).ok_or_else(|| {
                    models::ModelsError::UnknownModelDataType {
                        model_name: model.name.clone(),
                        data_type: model.data_type.clone(),
                    }
                })?;

            Ok(Some(make_order_by_expression(
                model,
                model.source.as_deref(),
                model_v1_orderable_fields,
                graphql_type_name.as_ref(),
                object_type_representation,
                models,
                scalar_types,
                graphql_types,
                order_by_expressions,
                issues,
            )?))
        }
        // Check that the order_by_expression exists and refers to the correct object type for the model
        models::ModelOrderBy::ModelV2(order_by_expression) => order_by_expression
            .as_ref()
            .map(|order_by_expression_name| {
                resolve_order_by_expression_for_model(
                    &model.name,
                    order_by_expression_name,
                    model_source,
                    order_by_expressions,
                    &model.data_type,
                    issues,
                )
            })
            .transpose(),
    }
}

pub fn make_order_by_expression(
    model: &models::Model,
    model_source: Option<&models::ModelSource>,
    model_v1_orderable_fields: &Vec<open_dds::models::OrderableField>,
    order_by_graphql_type_name: Option<&GraphQlTypeName>,
    object_type_representation: &object_relationships::ObjectTypeWithRelationships,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    order_by_expressions: &mut OrderByExpressions,
    issues: &mut Vec<Warning>,
) -> Result<Qualified<OrderByExpressionIdentifier>, models::ModelsError> {
    let identifier = Qualified::new(
        model.name.subgraph.clone(),
        OrderByExpressionIdentifier::FromModel(model.name.name.clone()),
    );

    // we have to generate a new leaf type per field for the model
    let mut new_scalar_order_by_expressions = BTreeMap::new();

    // collect fields, creating scalar order by expressions as we go
    let mut orderable_fields = BTreeMap::new();
    for orderable_field in model_v1_orderable_fields {
        let identifier = Qualified::new(
            model.name.subgraph.clone(),
            OrderByExpressionIdentifier::FromModelField(
                model.name.name.clone(),
                orderable_field.field_name.clone(),
            ),
        );

        // does field actually exist?
        if let Some(field) = object_type_representation
            .object_type
            .fields
            .get(&orderable_field.field_name)
        {
            // Check that the field type is a scalar type
            match &field.field_type.underlying_type {
                crate::QualifiedBaseType::Named(qualified_type_name) => {
                    match qualified_type_name {
                        QualifiedTypeName::Inbuilt(_) => {} // All inbuilts are scalars, so this is fine
                        QualifiedTypeName::Custom(custom_type_name) => {
                            // Is this named type a scalar type?
                            if !scalar_types.contains_key(custom_type_name) {
                                issues.push(
                                    models::ModelsIssue::ModelV1OrderableFieldIsNotAScalarField {
                                        model_name: model.name.clone(),
                                        field_name: orderable_field.field_name.clone(),
                                        field_type: field.field_type.clone(),
                                    }
                                    .into(),
                                );
                            }
                        }
                    }
                }
                // The field type is a list, this is not supported
                crate::QualifiedBaseType::List(_) => {
                    issues.push(
                        models::ModelsIssue::ModelV1OrderableFieldIsAnArrayType {
                            model_name: model.name.clone(),
                            field_name: orderable_field.field_name.clone(),
                            field_type: field.field_type.clone(),
                        }
                        .into(),
                    );
                }
            }
        } else {
            return Err(models::ModelsError::ModelV1OrderableFieldsError {
                model_name: model.name.clone(),
                error: OrderByExpressionError::InvalidOrderByExpressionOrderableField {
                    field_name: orderable_field.field_name.clone(),
                },
            });
        }

        let scalar_order_by_expression = ScalarOrderByExpression {
            identifier,
            enable_order_by_directions: orderable_field.order_by_directions.clone(),
            description: None,
            graphql: None,
        };

        new_scalar_order_by_expressions.insert(
            scalar_order_by_expression.identifier.clone(),
            scalar_order_by_expression.clone(),
        );

        orderable_fields.insert(
            orderable_field.field_name.clone(),
            OrderableField::Scalar(OrderableScalarField {
                order_by_expression_identifier: scalar_order_by_expression.identifier,
            }),
        );
    }

    let mut orderable_relationships = BTreeMap::new();
    for (relationship_name, relationship) in &object_type_representation.relationship_fields {
        // we automatically add a relationship if
        // a) we have a model source
        // b) the relationship target is a model with a model source
        // c) it's a local rather than remote relationship
        // d) it's an object relationship
        if let object_relationships::RelationshipTarget::Model(model_relationship_target) =
            &relationship.target
        {
            let object_relationships::ModelRelationshipTarget {
                model_name: target_model_name,
                relationship_type,
                target_typename: _,
                mappings: _,
                relationship_aggregate: _,
            } = model_relationship_target.as_ref();

            let target_model = models.get(target_model_name).ok_or_else(|| {
                models::ModelsError::ModelNotFound {
                    model_name: target_model_name.clone(),
                }
            })?;

            // Build relationship field in filter expression only when the target_model is backed by a source, we have a
            // check for the source model during the runtime
            if let (Some(target_source), Some(model_source), Some(target_capabilities)) = (
                &target_model.source,
                &model_source,
                &relationship.target_capabilities,
            ) {
                if let object_relationships::RelationshipExecutionCategory::Local =
                    object_relationships::relationship_execution_category(
                        crate::FieldNestedness::NotNested, // we don't support nested fields in
                        // legacy OrderByExpressions
                        &model_source.data_connector,
                        &target_source.data_connector,
                        target_capabilities,
                    )
                {
                    // TODO(naveen): Support Array relationships in order_by when the support for aggregates is implemented
                    if open_dds::relationships::RelationshipType::Object == *relationship_type {
                        // If the relationship target model does not have orderByExpressionType do not include
                        // it in the source model order_by input type.
                        orderable_relationships.insert(
                            relationship_name.clone(),
                            order_by_expressions::OrderableRelationship {
                                order_by_expression: None,
                            },
                        );
                    }
                }
            }
        }
    }

    let graphql = order_by_graphql_type_name
        .as_ref()
        .map(|type_name| {
            let expression_type_name = mk_name(type_name.as_str()).map(ast::TypeName)?;
            store_new_graphql_type(graphql_types, Some(&expression_type_name))?;
            Ok::<_, models::ModelsError>(OrderByExpressionGraphqlConfig {
                expression_type_name,
            })
        })
        .transpose()?;

    let object_order_by_expression = order_by_expressions::ObjectOrderByExpression {
        identifier: identifier.clone(),
        ordered_type: model.data_type.clone(),
        orderable_fields,
        orderable_relationships,
        graphql,
        description: Some(format!("OrderByExpression for Model {}", model.name)),
    };

    order_by_expressions
        .objects
        .insert(identifier.clone(), object_order_by_expression);

    Ok(identifier)
}

fn resolve_order_by_expression_for_model(
    qualified_model_name: &Qualified<ModelName>,
    order_by_expression_name: &open_dds::order_by_expression::OrderByExpressionName,
    model_source: Option<&models::ModelSource>,
    order_by_expressions: &OrderByExpressions,
    qualified_model_object_type_name: &Qualified<CustomTypeName>,
    issues: &mut Vec<Warning>,
) -> Result<Qualified<OrderByExpressionIdentifier>, models::ModelsError> {
    let order_by_expression_identifier = Qualified::new(
        qualified_model_name.subgraph.clone(),
        OrderByExpressionIdentifier::FromOrderByExpression(order_by_expression_name.clone()),
    );

    // Check if the order by expression exists
    let order_by_expression = order_by_expressions
        .objects
        .get(&order_by_expression_identifier)
        .ok_or_else(|| models::ModelsError::UnknownOrderByExpressionIdentifier {
            model_name: qualified_model_name.clone(),
            order_by_expression_identifier: order_by_expression_identifier.clone(),
        })?;

    // Check that the order by expression's type is the same as the model's type
    if order_by_expression.ordered_type != *qualified_model_object_type_name {
        return Err(models::ModelsError::OrderByExpressionTypeMismatch {
            model_name: qualified_model_name.clone(),
            model_type: qualified_model_object_type_name.clone(),
            order_by_expression_name: Qualified::new(
                qualified_model_name.subgraph.clone(),
                order_by_expression_name.clone(),
            ),
            order_by_expression_type: order_by_expression.ordered_type.clone(),
        });
    }

    // Validate compatibility with the model's data connector. We can only do that
    // if a model source (ie the data connector) has been configured
    if let Some(model_source) = model_source {
        validate_data_connector_compatibility(
            order_by_expression,
            model_source,
            qualified_model_name,
            order_by_expressions,
            issues,
        )?;
    }

    Ok(order_by_expression_identifier)
}

fn validate_data_connector_compatibility(
    order_by_expression: &order_by_expressions::ObjectOrderByExpression,
    model_source: &models::ModelSource,
    model_name: &Qualified<ModelName>,
    order_by_expressions: &OrderByExpressions,
    issues: &mut Vec<Warning>,
) -> Result<(), models::ModelsError> {
    for (field_name, orderable_field) in &order_by_expression.orderable_fields {
        match orderable_field {
            OrderableField::Object(orderable_object_field) => {
                // If we support ordering on nested fields ...
                if model_source
                    .data_connector
                    .capabilities
                    .supports_nested_object_ordering
                {
                    // ... check that we support nested relationships if the nested object has orderable relationships
                    let nested_order_by_expression = order_by_expressions
                        .objects
                        .get(&orderable_object_field.order_by_expression_identifier)
                        .ok_or_else(|| models::ModelsError::UnknownOrderByExpressionIdentifier {
                            model_name: model_name.clone(),
                            order_by_expression_identifier: orderable_object_field
                                .order_by_expression_identifier
                                .clone(),
                        })?;
                    validate_nested_relationship_compatibility(
                        nested_order_by_expression,
                        model_source,
                        model_name,
                        order_by_expressions,
                        issues,
                    )?;
                }
                // If we don't support ordering nested fields, log an issue
                else {
                    issues.push(
                        models::ModelsIssue::OrderByExpressionContainsUnsupportedNestedField {
                            order_by_expression_identifier: order_by_expression.identifier.clone(),
                            model_name: model_name.clone(),
                            nested_field_name: field_name.clone(),
                            data_connector_name: model_source.data_connector.name.clone(),
                        }
                        .into(),
                    );
                }
            }
            OrderableField::Scalar(_) => {}
        }
    }

    Ok(())
}

fn validate_nested_relationship_compatibility(
    order_by_expression: &order_by_expressions::ObjectOrderByExpression,
    model_source: &models::ModelSource,
    model_name: &Qualified<ModelName>,
    order_by_expressions: &OrderByExpressions,
    issues: &mut Vec<Warning>,
) -> Result<(), models::ModelsError> {
    let connector_supports_ordering_nested_relationships = model_source
        .data_connector
        .capabilities
        .supports_relationships
        .as_ref()
        .is_some_and(|rel| {
            rel.supports_nested_relationships
                .as_ref()
                .is_some_and(|n| n.supports_nested_in_ordering)
        });

    // If we have any relationships, and we don't support ordering by nested relationships, log an issue
    for relationship_name in order_by_expression.orderable_relationships.keys() {
        if !connector_supports_ordering_nested_relationships {
            issues.push(
                models::ModelsIssue::OrderByExpressionContainsUnsupportedNestedRelationship {
                    data_connector_name: model_source.data_connector.name.clone(),
                    model_name: model_name.clone(),
                    relationship_name: relationship_name.clone(),
                    order_by_expression_identifier: order_by_expression.identifier.clone(),
                }
                .into(),
            );
        }
    }

    // If we have any orderable nested object fields, recur into them to see whether they have
    // orderable relationships
    for orderable_field in order_by_expression.orderable_fields.values() {
        match orderable_field {
            OrderableField::Object(orderable_object_field) => {
                let nested_order_by_expression = order_by_expressions
                    .objects
                    .get(&orderable_object_field.order_by_expression_identifier)
                    .ok_or_else(|| models::ModelsError::UnknownOrderByExpressionIdentifier {
                        model_name: model_name.clone(),
                        order_by_expression_identifier: orderable_object_field
                            .order_by_expression_identifier
                            .clone(),
                    })?;
                validate_nested_relationship_compatibility(
                    nested_order_by_expression,
                    model_source,
                    model_name,
                    order_by_expressions,
                    issues,
                )?;
            }
            OrderableField::Scalar(_) => {}
        }
    }

    Ok(())
}
