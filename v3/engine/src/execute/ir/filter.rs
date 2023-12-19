use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use ndc_client as gdc;

use crate::execute::error;
use crate::schema::types;
use crate::schema::types::{InputAnnotation, ModelInputAnnotation};
use crate::schema::GDS;

/// Generates the IR for GraphQL 'where' boolean expression
pub(crate) fn resolve_filter_expression(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'_, GDS>>,
) -> Result<Vec<gdc::models::Expression>, error::Error> {
    let mut expressions = Vec::new();
    for (_field_name, field) in fields {
        match field.info.generic {
            // "_and"
            types::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelFilterArgument {
                    field: types::ModelFilterArgument::AndOp,
                },
            )) => {
                let values = field.value.as_list()?;
                let expression = gdc::models::Expression::And {
                    expressions: values
                        .iter()
                        .map(|value| {
                            Ok(gdc::models::Expression::And {
                                expressions: resolve_filter_expression(value.as_object()?)?,
                            })
                        })
                        .collect::<Result<Vec<gdc::models::Expression>, error::Error>>()?,
                };
                expressions.push(expression);
            }
            // "_or"
            types::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelFilterArgument {
                    field: types::ModelFilterArgument::OrOp,
                },
            )) => {
                let values = field.value.as_list()?;
                let expression = gdc::models::Expression::Or {
                    expressions: values
                        .iter()
                        .map(|value| {
                            Ok(gdc::models::Expression::And {
                                expressions: resolve_filter_expression(value.as_object()?)?,
                            })
                        })
                        .collect::<Result<Vec<gdc::models::Expression>, error::Error>>()?,
                };
                expressions.push(expression);
            }
            // "_not"
            types::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelFilterArgument {
                    field: types::ModelFilterArgument::NotOp,
                },
            )) => {
                let value = field.value.as_object()?;
                expressions.push(gdc::models::Expression::Not {
                    expression: Box::new(gdc::models::Expression::And {
                        expressions: resolve_filter_expression(value)?,
                    }),
                })
            }
            types::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelFilterArgument {
                    field: types::ModelFilterArgument::Field { ndc_column: column },
                },
            )) => {
                for (op_name, op_value) in field.value.as_object()? {
                    let expression = match op_name.as_str() {
                        "_eq" => build_binary_comparison_expression(
                            gdc::models::BinaryComparisonOperator::Equal,
                            column.clone(),
                            &op_value.value,
                        ),
                        "_is_null" => build_is_null_expression(column.clone(), &op_value.value)?,
                        other => {
                            let operator = gdc::models::BinaryComparisonOperator::Other {
                                name: other.to_string(),
                            };
                            build_binary_comparison_expression(
                                operator,
                                column.clone(),
                                &op_value.value,
                            )
                        }
                    };
                    expressions.push(expression)
                }
            }
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(expressions)
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression(
    operator: gdc::models::BinaryComparisonOperator,
    column: String,
    value: &normalized_ast::Value<'_, GDS>,
) -> gdc::models::Expression {
    gdc::models::Expression::BinaryComparisonOperator {
        column: gdc::models::ComparisonTarget::Column {
            name: column,
            path: vec![],
        },
        operator,
        value: gdc::models::ComparisonValue::Scalar {
            value: value.as_json(),
        },
    }
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression(
    column: String,
    value: &normalized_ast::Value<'_, GDS>,
) -> Result<gdc::models::Expression, error::Error> {
    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression = gdc::models::Expression::UnaryComparisonOperator {
        column: gdc::models::ComparisonTarget::Column {
            name: column,
            path: vec![],
        },
        operator: gdc::models::UnaryComparisonOperator::IsNull,
    };
    // Get `_is_null` input value as boolean
    let is_null = value.as_boolean()?;
    if is_null {
        // When _is_null: true. Just return 'IsNull' unary comparison expression.
        Ok(unary_comparison_expression)
    } else {
        // When _is_null: false. Return negated 'IsNull' unary comparison expression by wrapping it in 'Not'.
        Ok(gdc::models::Expression::Not {
            expression: Box::new(unary_comparison_expression),
        })
    }
}
