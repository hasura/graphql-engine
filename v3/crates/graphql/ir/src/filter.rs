use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use serde::Serialize;

use crate::error;
use crate::flags::GraphqlIrFlags;
use graphql_schema::{self};
use graphql_schema::{BooleanExpressionAnnotation, InputAnnotation, ObjectFieldKind};
use graphql_schema::{
    FilterRelationshipAnnotation, ObjectBooleanExpressionField, ScalarBooleanExpressionField,
};
use graphql_schema::{GDS, LogicalOperatorField};
use open_dds::types::FieldName;
use plan::count_model;
use plan_types::{Expression, UsagesCounts};

/// Filter expression to be applied on a model/command selection set
#[derive(Debug, Serialize, Clone)]
pub struct FilterExpression<'s> {
    /// Filter obtained from the GraphQL query request
    pub query_filter: QueryFilter<'s>,
    /// Permission filters
    pub permission_filter: Option<Expression<'s>>,
    /// Filter for representing local relationship joins
    pub relationship_join_filter: Option<Expression<'s>>,
}

/// Filter expression derived from GraphQL query.
#[derive(Debug, Serialize, Clone)]
pub struct QueryFilter<'s> {
    /// Filter derived from `where` clause.
    pub where_clause: Option<Expression<'s>>,
    /// Relay global ID or unique field comparisons
    pub additional_filter: Option<Expression<'s>>,
}

/// Generate the OpenDD IR for GraphQL 'where' boolean expression
pub fn resolve_filter_expression_open_dd(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'_, GDS>>,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::BooleanExpression, error::Error> {
    resolve_object_boolean_expression_open_dd(fields, &[], flags, usage_counts)
}

fn resolve_object_boolean_expression_open_dd(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'_, GDS>>,
    field_path: &[open_dds::query::ObjectFieldTarget],
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::BooleanExpression, error::Error> {
    let field_expressions = fields
        .values()
        .map(|field| {
            let field_annotation =
                extract_object_boolean_expression_field_annotation(field.info.generic)?;

            let field_expression = match field_annotation {
                // "_and" field
                ObjectBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::AndOp) => {
                    // The "_and" field value should be a list
                    let and_values = field.value.as_list()?;

                    let and_expressions = and_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_object_boolean_expression_open_dd(
                                value_object,
                                field_path,
                                flags,
                                usage_counts,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    open_dds::query::BooleanExpression::And(and_expressions)
                }
                // "_or" field
                ObjectBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::OrOp) => {
                    // The "_or" field value should be a list
                    let or_values = field.value.as_list()?;

                    let or_expressions = or_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_object_boolean_expression_open_dd(
                                value_object,
                                field_path,
                                flags,
                                usage_counts,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    open_dds::query::BooleanExpression::Or(or_expressions)
                }
                // "_not" field
                ObjectBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::NotOp) => {
                    // The "_not" field value should be an object
                    let not_value = field.value.as_object()?;

                    let not_filter_expression = resolve_object_boolean_expression_open_dd(
                        not_value,
                        field_path,
                        flags,
                        usage_counts,
                    )?;
                    open_dds::query::BooleanExpression::Not(Box::new(not_filter_expression))
                }
                // comparableField field
                ObjectBooleanExpressionField::Field {
                    field_name,
                    object_type: _,
                    object_field_kind,
                    deprecated: _,
                } => {
                    let field_value = field.value.as_object()?;

                    match object_field_kind {
                        ObjectFieldKind::Object => {
                            // Append the current column to the column_path before descending into the nested object expression
                            let new_field_path = field_path
                                .iter()
                                .cloned()
                                .chain([open_dds::query::ObjectFieldTarget {
                                    field_name: field_name.clone(),
                                    arguments: IndexMap::new(),
                                }])
                                .collect::<Vec<_>>();

                            resolve_object_boolean_expression_open_dd(
                                field_value,
                                &new_field_path,
                                flags,
                                usage_counts,
                            )?
                        }
                        ObjectFieldKind::ObjectArray => {
                            if flags.fix_exists_in_nested_arrays {
                                // correctly use `BooleanExpression::Exists` to nest "AND" inside
                                // nested arrays properly
                                let inner = resolve_object_boolean_expression_open_dd(
                                    field_value,
                                    &[],
                                    flags,
                                    usage_counts,
                                )?;

                                open_dds::query::BooleanExpression::Exists {
                                    operand: build_nested_field_path(field_path).map(|a| *a),
                                    predicate: Box::new(inner),
                                    field_name: field_name.clone(),
                                }
                            } else {
                                // old behaviour, should match `ObjectFieldKind::Object` above
                                // Append the current column to the column_path before descending into the nested object expression
                                let new_field_path = field_path
                                    .iter()
                                    .cloned()
                                    .chain([open_dds::query::ObjectFieldTarget {
                                        field_name: field_name.clone(),
                                        arguments: IndexMap::new(),
                                    }])
                                    .collect::<Vec<_>>();

                                resolve_object_boolean_expression_open_dd(
                                    field_value,
                                    &new_field_path,
                                    flags,
                                    usage_counts,
                                )?
                            }
                        }

                        ObjectFieldKind::Scalar | ObjectFieldKind::ScalarArray => {
                            resolve_scalar_boolean_expression_open_dd(
                                field_value,
                                field_path,
                                field_name,
                            )?
                        }
                    }
                }
                ObjectBooleanExpressionField::RelationshipField(FilterRelationshipAnnotation {
                    relationship_name,
                    target_model_name,
                    ..
                }) => {
                    // Add the target model being used in the usage counts
                    count_model(target_model_name, usage_counts);

                    // This map contains the relationships or the columns of the
                    // relationship that needs to be used for ordering.
                    let field_value = field.value.as_object()?;

                    let inner = resolve_object_boolean_expression_open_dd(
                        field_value,
                        &[], // should we reset this?
                        flags,
                        usage_counts,
                    )?;

                    open_dds::query::BooleanExpression::Relationship {
                        operand: build_nested_field_path(field_path).map(|a| *a),
                        relationship_name: relationship_name.clone(),
                        predicate: Box::new(inner),
                    }
                }
            };

            Ok(field_expression)
        })
        .collect::<Result<Vec<open_dds::query::BooleanExpression>, error::Error>>()?;

    Ok(open_dds::query::BooleanExpression::And(field_expressions))
}

fn extract_object_boolean_expression_field_annotation(
    annotation: &graphql_schema::Annotation,
) -> Result<&ObjectBooleanExpressionField, error::Error> {
    match annotation {
        graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                object_boolean_expression_field,
            ),
        )) => Ok(object_boolean_expression_field),
        _ => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }
        .into()),
    }
}

fn resolve_scalar_boolean_expression_open_dd(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'_, GDS>>,
    field_path: &[open_dds::query::ObjectFieldTarget],
    field_name: &FieldName,
) -> Result<open_dds::query::BooleanExpression, error::Error> {
    let field_expressions = fields
        .values()
        .map(|field| {
            let field_annotation =
                extract_scalar_boolean_expression_field_annotation(field.info.generic)?;

            let field_expression = match field_annotation {
                // "_and" field
                ScalarBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::AndOp) => {
                    // The "_and" field value should be a list
                    let and_values = field.value.as_list()?;

                    let and_expressions = and_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_scalar_boolean_expression_open_dd(
                                value_object,
                                field_path,
                                field_name,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    open_dds::query::BooleanExpression::And(and_expressions)
                }
                // "_or" field
                ScalarBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::OrOp) => {
                    // The "_or" field value should be a list
                    let or_values = field.value.as_list()?;

                    let or_expressions = or_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_scalar_boolean_expression_open_dd(
                                value_object,
                                field_path,
                                field_name,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    open_dds::query::BooleanExpression::Or(or_expressions)
                }
                // "_not" field
                ScalarBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::NotOp) => {
                    // The "_not" field value should be an object
                    let not_value = field.value.as_object()?;

                    let not_filter_expression = resolve_scalar_boolean_expression_open_dd(
                        not_value, field_path, field_name,
                    )?;
                    open_dds::query::BooleanExpression::Not(Box::new(not_filter_expression))
                }
                ScalarBooleanExpressionField::IsNullOperation => {
                    build_is_null_expression_open_dd(field_path, field_name, &field.value)?
                }
                ScalarBooleanExpressionField::ComparisonOperation { operator_name, .. } => {
                    build_binary_comparison_expression_open_dd(
                        operator_name,
                        field_path,
                        field_name,
                        &field.value,
                    )
                }
            };

            Ok(field_expression)
        })
        .collect::<Result<Vec<open_dds::query::BooleanExpression>, error::Error>>()?;

    Ok(open_dds::query::BooleanExpression::And(field_expressions))
}

fn extract_scalar_boolean_expression_field_annotation(
    annotation: &graphql_schema::Annotation,
) -> Result<&ScalarBooleanExpressionField, error::Error> {
    match annotation {
        graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::ScalarBooleanExpressionField(
                scalar_boolean_expression_field,
            ),
        )) => Ok(scalar_boolean_expression_field),
        _ => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }
        .into()),
    }
}

fn build_nested_field_path(
    field_path: &[open_dds::query::ObjectFieldTarget],
) -> Option<Box<open_dds::query::Operand>> {
    nonempty::NonEmpty::from_slice(field_path).map(|field_path| {
        let nested = build_nested_field_path(field_path.tail());

        Box::new(open_dds::query::Operand::Field(
            open_dds::query::ObjectFieldOperand {
                target: Box::new(field_path.first().clone()),
                nested,
            },
        ))
    })
}

fn reset_field_path(
    field_path: &[open_dds::query::ObjectFieldTarget],
    field_name: &FieldName,
) -> Vec<open_dds::query::ObjectFieldTarget> {
    field_path
        .iter()
        .cloned()
        .chain([open_dds::query::ObjectFieldTarget {
            field_name: field_name.clone(),
            arguments: IndexMap::new(),
        }])
        .skip(1)
        .collect()
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression_open_dd(
    field_path: &[open_dds::query::ObjectFieldTarget],
    field_name: &FieldName,
    value: &normalized_ast::Value<'_, GDS>,
) -> Result<open_dds::query::BooleanExpression, error::Error> {
    let unary_comparison_expression =
        open_dds::query::BooleanExpression::IsNull(operand_from_field_path(field_path, field_name));

    let is_null = value.as_boolean()?;
    if is_null {
        // When _is_null: true. Just return 'IsNull' unary comparison expression.
        Ok(unary_comparison_expression)
    } else {
        // When _is_null: false. Return negated 'IsNull' unary comparison expression by wrapping it in 'Not'.
        Ok(open_dds::query::BooleanExpression::Not(Box::new(
            unary_comparison_expression,
        )))
    }
}

fn operand_from_field_path(
    field_path: &[open_dds::query::ObjectFieldTarget],
    field_name: &FieldName,
) -> open_dds::query::Operand {
    let nested = build_nested_field_path(&reset_field_path(field_path, field_name));
    match field_path.first() {
        Some(target) => open_dds::query::Operand::Field(open_dds::query::ObjectFieldOperand {
            target: Box::new(target.clone()),
            nested,
        }),

        None => open_dds::query::Operand::Field(open_dds::query::ObjectFieldOperand {
            target: Box::new(open_dds::query::ObjectFieldTarget {
                field_name: field_name.clone(),
                arguments: IndexMap::new(),
            }),
            nested,
        }),
    }
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression_open_dd(
    operator_name: &open_dds::types::OperatorName,
    field_path: &[open_dds::query::ObjectFieldTarget],
    field_name: &FieldName,
    value: &normalized_ast::Value<'_, GDS>,
) -> open_dds::query::BooleanExpression {
    open_dds::query::BooleanExpression::Comparison {
        argument: Box::new(open_dds::query::Value::Literal(value.as_json())),
        operator: open_dds::query::ComparisonOperator::Custom(operator_name.clone()),
        operand: operand_from_field_path(field_path, field_name),
    }
}
