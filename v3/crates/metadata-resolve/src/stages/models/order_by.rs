pub use super::error::ModelsError;
use super::{source, ModelSource, ModelsIssue};
use lang_graphql::ast::common as ast;
use open_dds::identifier::SubgraphName;
use open_dds::types::CustomTypeName;

use crate::helpers::types::store_new_graphql_type;
use crate::{mk_name, QualifiedTypeName};

use crate::stages::{order_by_expressions, scalar_types, type_permissions};
use crate::types::subgraph::Qualified;

use open_dds::models::ModelName;

use std::collections::{BTreeMap, BTreeSet};

use super::order_by_expressions::{
    OrderByExpressionError, OrderByExpressionGraphqlConfig, OrderByExpressionIdentifier,
    OrderByExpressions, OrderableField, OrderableRelationships, OrderableScalarField,
    ScalarOrderByExpression,
};

pub fn resolve_order_by_expression(
    qualified_model_name: &Qualified<ModelName>,
    model: &open_dds::models::Model,
    qualified_model_object_type_name: &Qualified<CustomTypeName>,
    model_source: Option<&ModelSource>,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    order_by_expressions: &mut OrderByExpressions,
    issues: &mut Vec<ModelsIssue>,
) -> Result<Option<Qualified<OrderByExpressionIdentifier>>, ModelsError> {
    match model {
        open_dds::models::Model::V1(model_v1) => {
            let object_type_representation = source::get_model_object_type_representation(
                object_types,
                qualified_model_object_type_name,
                qualified_model_name,
            )?;
            Ok(Some(make_order_by_expression(
                model_v1,
                object_type_representation,
                &qualified_model_name.subgraph,
                scalar_types,
                graphql_types,
                qualified_model_name,
                order_by_expressions,
                issues,
            )?))
        }
        // Check that the order_by_expression exists and refers to the correct object type for the model
        open_dds::models::Model::V2(model_v2) => model_v2
            .order_by_expression
            .as_ref()
            .map(|order_by_expression_name| {
                resolve_order_by_expression_for_model(
                    qualified_model_name,
                    order_by_expression_name,
                    model_source,
                    order_by_expressions,
                    qualified_model_object_type_name,
                    issues,
                )
            })
            .transpose(),
    }
}

pub fn make_order_by_expression(
    model_v1: &open_dds::models::ModelV1,
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    subgraph: &SubgraphName,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    qualified_model_name: &Qualified<ModelName>,
    order_by_expressions: &mut OrderByExpressions,
    issues: &mut Vec<ModelsIssue>,
) -> Result<Qualified<OrderByExpressionIdentifier>, ModelsError> {
    let identifier = Qualified::new(
        subgraph.clone(),
        OrderByExpressionIdentifier::FromModel(model_v1.name.clone()),
    );
    let ordered_type = Qualified::new(subgraph.clone(), model_v1.object_type.clone());

    // we have to generate a new leaf type per field for the model
    let mut new_scalar_order_by_expressions = BTreeMap::new();

    // collect fields, creating scalar order by expressions as we go
    let mut orderable_fields = BTreeMap::new();
    for orderable_field in &model_v1.orderable_fields {
        let identifier = Qualified::new(
            subgraph.clone(),
            OrderByExpressionIdentifier::FromModelField(
                qualified_model_name.name.clone(),
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
                                issues.push(ModelsIssue::ModelV1OrderableFieldIsNotAScalarField {
                                    model_name: qualified_model_name.clone(),
                                    field_name: orderable_field.field_name.clone(),
                                    field_type: field.field_type.clone(),
                                });
                            }
                        }
                    }
                }
                // The field type is a list, this is not supported
                crate::QualifiedBaseType::List(_) => {
                    issues.push(ModelsIssue::ModelV1OrderableFieldIsAnArrayType {
                        model_name: qualified_model_name.clone(),
                        field_name: orderable_field.field_name.clone(),
                        field_type: field.field_type.clone(),
                    });
                }
            }
        } else {
            return Err(ModelsError::ModelV1OrderableFieldsError {
                model_name: qualified_model_name.clone(),
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

    let object_order_by_expression = order_by_expressions::ObjectOrderByExpression {
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
        .objects
        .insert(identifier.clone(), object_order_by_expression);

    Ok(identifier)
}

fn resolve_order_by_expression_for_model(
    qualified_model_name: &Qualified<ModelName>,
    order_by_expression_name: &open_dds::order_by_expression::OrderByExpressionName,
    model_source: Option<&ModelSource>,
    order_by_expressions: &OrderByExpressions,
    qualified_model_object_type_name: &Qualified<CustomTypeName>,
    issues: &mut Vec<ModelsIssue>,
) -> Result<Qualified<OrderByExpressionIdentifier>, ModelsError> {
    let order_by_expression_identifier = Qualified::new(
        qualified_model_name.subgraph.clone(),
        OrderByExpressionIdentifier::FromOrderByExpression(order_by_expression_name.clone()),
    );

    // Check if the order by expression exists
    let order_by_expression = order_by_expressions
        .objects
        .get(&order_by_expression_identifier)
        .ok_or_else(|| ModelsError::UnknownOrderByExpressionIdentifier {
            model_name: qualified_model_name.clone(),
            order_by_expression_identifier: order_by_expression_identifier.clone(),
        })?;

    // Check that the order by expression's type is the same as the model's type
    if order_by_expression.ordered_type != *qualified_model_object_type_name {
        return Err(ModelsError::OrderByExpressionTypeMismatch {
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
    model_source: &ModelSource,
    model_name: &Qualified<ModelName>,
    order_by_expressions: &OrderByExpressions,
    issues: &mut Vec<ModelsIssue>,
) -> Result<(), ModelsError> {
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
                        .ok_or_else(|| ModelsError::UnknownOrderByExpressionIdentifier {
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
                        ModelsIssue::OrderByExpressionContainsUnsupportedNestedField {
                            order_by_expression_identifier: order_by_expression.identifier.clone(),
                            model_name: model_name.clone(),
                            nested_field_name: field_name.clone(),
                            data_connector_name: model_source.data_connector.name.clone(),
                        },
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
    model_source: &ModelSource,
    model_name: &Qualified<ModelName>,
    order_by_expressions: &OrderByExpressions,
    issues: &mut Vec<ModelsIssue>,
) -> Result<(), ModelsError> {
    match &order_by_expression.orderable_relationships {
        // Only created by old ModelV1 ordering which doesn't handle nested fields anyway
        OrderableRelationships::ModelV1AllowAll => {}

        OrderableRelationships::ModelV2(orderable_relationships) => {
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
            for relationship_name in orderable_relationships.keys() {
                if !connector_supports_ordering_nested_relationships {
                    issues.push(
                        ModelsIssue::OrderByExpressionContainsUnsupportedNestedRelationship {
                            data_connector_name: model_source.data_connector.name.clone(),
                            model_name: model_name.clone(),
                            relationship_name: relationship_name.clone(),
                            order_by_expression_identifier: order_by_expression.identifier.clone(),
                        },
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
                            .ok_or_else(|| ModelsError::UnknownOrderByExpressionIdentifier {
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
        }
    }

    Ok(())
}
