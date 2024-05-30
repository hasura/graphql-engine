use std::collections::BTreeMap;

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use ndc_models;
use serde::Serialize;

use crate::ir::error;
use crate::model_tracking::{count_model, UsagesCounts};
use schema::FilterRelationshipAnnotation;
use schema::GDS;
use schema::{self};
use schema::{BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation};

use super::relationship::LocalModelRelationshipInfo;
use crate::ir::selection_set::NDCRelationshipName;

#[derive(Debug, Serialize)]
pub(crate) struct ResolvedFilterExpression<'s> {
    pub expression: Option<ndc_models::Expression>,
    // relationships that were used in the filter expression. This is helpful
    // for collecting relatinships and sending collection_relationships
    pub relationships: BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
}

/// Generates the IR for GraphQL 'where' boolean expression
pub(crate) fn resolve_filter_expression<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    usage_counts: &mut UsagesCounts,
) -> Result<ResolvedFilterExpression<'s>, error::Error> {
    let mut expressions = Vec::new();
    let mut relationships = BTreeMap::new();
    for field in fields.values() {
        let field_filter_expression =
            build_filter_expression(field, &mut relationships, usage_counts)?;
        expressions.push(field_filter_expression);
    }
    let expression = ndc_models::Expression::And { expressions };
    let resolved_filter_expression = ResolvedFilterExpression {
        expression: Some(expression),
        relationships,
    };
    Ok(resolved_filter_expression)
}

// Build the NDC filter expression by traversing the relationships when present
fn build_filter_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<ndc_models::Expression, error::Error> {
    match field.info.generic {
        // "_and"
        schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: schema::ModelFilterArgument::AndOp,
            },
        )) => {
            let mut and_expressions = Vec::new();
            // The "_and" field value should be a list
            let and_values = field.value.as_list()?;

            for value in and_values {
                // Each value in the list should be an object
                let value_object = value.as_object()?;
                and_expressions.push(resolve_filter_object(
                    value_object,
                    relationships,
                    usage_counts,
                )?);
            }
            Ok(ndc_models::Expression::And {
                expressions: and_expressions,
            })
        }
        // "_or"
        schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: schema::ModelFilterArgument::OrOp,
            },
        )) => {
            let mut or_expressions = Vec::new();
            // The "_or" field value should be a list
            let or_values = field.value.as_list()?;

            for value in or_values {
                let value_object = value.as_object()?;
                or_expressions.push(resolve_filter_object(
                    value_object,
                    relationships,
                    usage_counts,
                )?);
            }

            Ok(ndc_models::Expression::Or {
                expressions: or_expressions,
            })
        }
        // "_not"
        schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: schema::ModelFilterArgument::NotOp,
            },
        )) => {
            // The "_not" field value should be an object
            let not_value = field.value.as_object()?;

            let not_filter_expression =
                resolve_filter_object(not_value, relationships, usage_counts)?;
            Ok(ndc_models::Expression::Not {
                expression: Box::new(not_filter_expression),
            })
        }
        // The column that we want to use for filtering.
        schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: schema::ModelFilterArgument::Field { ndc_column: column },
            },
        )) => {
            let mut expressions = Vec::new();
            for (_op_name, op_value) in field.value.as_object()? {
                match op_value.info.generic {
                    schema::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::IsNullOperation,
                    )) => {
                        let expression = build_is_null_expression(column.clone(), &op_value.value)?;
                        expressions.push(expression);
                    }
                    schema::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ComparisonOperation { operator },
                    )) => {
                        let expression = build_binary_comparison_expression(
                            operator,
                            column.clone(),
                            &op_value.value,
                        );
                        expressions.push(expression)
                    }
                    annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?,
                }
            }
            Ok(ndc_models::Expression::And { expressions })
        }
        // Relationship field used for filtering.
        // This relationship can either point to another relationship or a column.
        schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field:
                    schema::ModelFilterArgument::RelationshipField(FilterRelationshipAnnotation {
                        relationship_name,
                        relationship_type,
                        source_type,
                        source_data_connector,
                        source_type_mappings,
                        target_source,
                        target_type,
                        target_model_name,
                        mappings,
                    }),
            },
        )) => {
            // Add the target model being used in the usage counts
            count_model(target_model_name, usage_counts);

            let ndc_relationship_name = NDCRelationshipName::new(source_type, relationship_name)?;
            relationships.insert(
                ndc_relationship_name.clone(),
                LocalModelRelationshipInfo {
                    relationship_name,
                    relationship_type,
                    source_type,
                    source_data_connector,
                    source_type_mappings,
                    target_source,
                    target_type,
                    mappings,
                },
            );

            // This map contains the relationships or the columns of the
            // relationship that needs to be used for ordering.
            let filter_object = field.value.as_object()?;

            let mut expressions = Vec::new();

            for field in filter_object.values() {
                let field_filter_expression =
                    build_filter_expression(field, relationships, usage_counts)?;
                expressions.push(field_filter_expression);
            }

            // Using exists clause to build the filter expression for relationship fields.
            let exists_filter_clause = ndc_models::Expression::And { expressions };
            let exists_in_relationship = ndc_models::ExistsInCollection::Related {
                relationship: ndc_relationship_name.0,
                arguments: BTreeMap::new(),
            };
            Ok(ndc_models::Expression::Exists {
                in_collection: exists_in_relationship,
                predicate: Some(Box::new(exists_filter_clause)),
            })
        }
        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        })?,
    }
}

/// Generate a filter expression from an input object fields
fn resolve_filter_object<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<ndc_models::Expression, error::Error> {
    let mut expressions = Vec::new();

    for field in fields.values() {
        expressions.push(build_filter_expression(field, relationships, usage_counts)?)
    }
    Ok(ndc_models::Expression::And { expressions })
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression(
    operator: &str,
    column: String,
    value: &normalized_ast::Value<'_, GDS>,
) -> ndc_models::Expression {
    ndc_models::Expression::BinaryComparisonOperator {
        column: ndc_models::ComparisonTarget::Column {
            name: column,
            path: Vec::new(),
        },
        operator: operator.to_string(),
        value: ndc_models::ComparisonValue::Scalar {
            value: value.as_json(),
        },
    }
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression(
    column: String,
    value: &normalized_ast::Value<'_, GDS>,
) -> Result<ndc_models::Expression, error::Error> {
    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression = ndc_models::Expression::UnaryComparisonOperator {
        column: ndc_models::ComparisonTarget::Column {
            name: column,
            path: Vec::new(),
        },
        operator: ndc_models::UnaryComparisonOperator::IsNull,
    };
    // Get `_is_null` input value as boolean
    let is_null = value.as_boolean()?;
    if is_null {
        // When _is_null: true. Just return 'IsNull' unary comparison expression.
        Ok(unary_comparison_expression)
    } else {
        // When _is_null: false. Return negated 'IsNull' unary comparison expression by wrapping it in 'Not'.
        Ok(ndc_models::Expression::Not {
            expression: Box::new(unary_comparison_expression),
        })
    }
}
