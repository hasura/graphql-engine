use std::collections::BTreeMap;

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use ndc_models;
use serde::Serialize;

use crate::execute::error;
use crate::execute::model_tracking::{count_model, UsagesCounts};
use crate::schema::types::output_type::relationship::FilterRelationshipAnnotation;
use crate::schema::types::{self};
use crate::schema::types::{BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation};
use crate::schema::GDS;

use super::relationship::LocalModelRelationshipInfo;
use crate::execute::ir::selection_set::NDCRelationshipName;

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
        let relationship_paths = Vec::new();
        let expression =
            build_filter_expression(field, relationship_paths, &mut relationships, usage_counts)?;
        expressions.extend(expression);
    }
    let expression = ndc_models::Expression::And { expressions };
    let resolved_filter_expression = ResolvedFilterExpression {
        expression: Some(expression),
        relationships,
    };
    Ok(resolved_filter_expression)
}

// Build the NDC filter expression by traversing the relationships when present
pub(crate) fn build_filter_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    // The path to access the relationship column. If the column is a
    // non-relationship column, this will be empty. The paths contains the names
    // of relationships (in order) that needs to be traversed to access the
    // column.
    mut relationship_paths: Vec<NDCRelationshipName>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<Vec<ndc_models::Expression>, error::Error> {
    match field.info.generic {
        // "_and"
        types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: types::ModelFilterArgument::AndOp,
            },
        )) => {
            let mut expressions = Vec::new();
            let values = field.value.as_list()?;

            for value in values.iter() {
                let resolved_filter_expression =
                    resolve_filter_expression(value.as_object()?, usage_counts)?;
                expressions.extend(resolved_filter_expression.expression);
                relationships.extend(resolved_filter_expression.relationships);
            }

            let expression = ndc_models::Expression::And { expressions };

            Ok(vec![expression])
        }
        // "_or"
        types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: types::ModelFilterArgument::OrOp,
            },
        )) => {
            let mut expressions = Vec::new();
            let values = field.value.as_list()?;

            for value in values.iter() {
                let resolved_filter_expression =
                    resolve_filter_expression(value.as_object()?, usage_counts)?;
                expressions.extend(resolved_filter_expression.expression);
                relationships.extend(resolved_filter_expression.relationships);
            }

            let expression = ndc_models::Expression::Or { expressions };

            Ok(vec![expression])
        }
        // "_not"
        types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: types::ModelFilterArgument::NotOp,
            },
        )) => {
            let value = field.value.as_object()?;

            let resolved_filter_expression = resolve_filter_expression(value, usage_counts)?;
            relationships.extend(resolved_filter_expression.relationships);

            let expressions = match resolved_filter_expression.expression {
                Some(expression) => vec![ndc_models::Expression::Not {
                    expression: Box::new(expression),
                }],
                None => Vec::new(),
            };
            Ok(expressions)
        }
        // The column that we want to use for filtering. If the column happens
        // to be a relationship column, we'll have to join all the paths to
        // specify NDC, what relationships needs to be traversed to access this
        // column. The order decides how to access the column.
        types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: types::ModelFilterArgument::Field { ndc_column: column },
            },
        )) => {
            let mut expressions = Vec::new();
            for (_op_name, op_value) in field.value.as_object()? {
                match op_value.info.generic {
                    types::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::IsNullOperation,
                    )) => {
                        let expression = build_is_null_expression(
                            column.clone(),
                            &op_value.value,
                            &relationship_paths,
                        )?;
                        expressions.push(expression);
                    }
                    types::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ComparisonOperation { operator },
                    )) => {
                        let expression = build_binary_comparison_expression(
                            operator,
                            column.clone(),
                            &op_value.value,
                            &relationship_paths,
                        );
                        expressions.push(expression)
                    }
                    annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?,
                }
            }
            Ok(expressions)
        }
        // Relationship field used for filtering.
        // This relationship can either point to another relationship or a column.
        types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field:
                    types::ModelFilterArgument::RelationshipField(FilterRelationshipAnnotation {
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

            let mut expressions = Vec::new();

            // This map contains the relationships or the columns of the
            // relationship that needs to be used for ordering.
            let argument_value_map = field.value.as_object()?;
            // Add the current relationship to the relationship paths.
            relationship_paths.push(ndc_relationship_name);
            // Keep track of relationship paths as we keep traversing down the
            // relationships.
            for argument in argument_value_map.values() {
                let expression = build_filter_expression(
                    argument,
                    relationship_paths.clone(),
                    relationships,
                    usage_counts,
                )?;
                expressions.extend(expression);
            }
            Ok(expressions)
        }
        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        })?,
    }
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression(
    operator: &str,
    column: String,
    value: &normalized_ast::Value<'_, GDS>,
    relationship_paths: &Vec<NDCRelationshipName>,
) -> ndc_models::Expression {
    let path_elements = build_path_elements(relationship_paths);

    ndc_models::Expression::BinaryComparisonOperator {
        column: ndc_models::ComparisonTarget::Column {
            name: column,
            path: path_elements,
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
    relationship_paths: &Vec<NDCRelationshipName>,
) -> Result<ndc_models::Expression, error::Error> {
    let path_elements = build_path_elements(relationship_paths);

    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression = ndc_models::Expression::UnaryComparisonOperator {
        column: ndc_models::ComparisonTarget::Column {
            name: column,
            path: path_elements,
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

pub fn build_path_elements(
    relationship_paths: &Vec<NDCRelationshipName>,
) -> Vec<ndc_models::PathElement> {
    let mut path_elements = Vec::new();
    for path in relationship_paths {
        path_elements.push(ndc_models::PathElement {
            relationship: path.0.clone(),
            arguments: BTreeMap::new(),
            // 'AND' predicate indicates that the column can be accessed
            // by joining all the relationships paths provided
            predicate: Some(Box::new(ndc_models::Expression::And {
                expressions: Vec::new(),
            })),
        })
    }
    path_elements
}
