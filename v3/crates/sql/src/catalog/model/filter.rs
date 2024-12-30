mod like;

use std::sync::Arc;

use crate::catalog::model::Model;
use datafusion::{
    logical_expr::{expr::ScalarFunction, BinaryExpr, Expr, Operator, TableProviderFilterPushDown},
    scalar::ScalarValue,
};
use like::LikeString;
use metadata_resolve::Metadata;
use open_dds::query::ComparisonOperator;

pub(crate) fn can_pushdown_filters(
    _metadata: &Arc<Metadata>,
    _model: &Arc<Model>,
    filters: &[&Expr],
) -> Vec<TableProviderFilterPushDown> {
    let mut results = vec![];

    for &filter in filters {
        results.push(if can_pushdown_filter(filter) {
            TableProviderFilterPushDown::Exact
        } else {
            TableProviderFilterPushDown::Unsupported
        });
    }

    results
}

pub(crate) fn can_pushdown_filter(expr: &Expr) -> bool {
    pushdown_filter(expr).is_ok()
}

pub(crate) fn pushdown_filter(
    expr: &datafusion::prelude::Expr,
) -> datafusion::error::Result<open_dds::query::BooleanExpression> {
    match expr {
        Expr::BinaryExpr(BinaryExpr { left, op, right }) => {
            match op {
                Operator::Eq
                | Operator::NotEq
                | Operator::Lt
                | Operator::LtEq
                | Operator::Gt
                | Operator::GtEq => {
                    let Some((path, column)) = super::common::try_into_column(left.as_ref())?
                    else {
                        return Err(datafusion::error::DataFusionError::Internal(format!(
                            "unsupported left-hand-side in binary expression: {left:?}"
                        )));
                    };

                    let Expr::Literal(value) = right.as_ref() else {
                        return Err(datafusion::error::DataFusionError::Internal(format!(
                            "unsupported right-hand-side in binary expression: {left:?}"
                        )));
                    };

                    let operand = super::common::to_operand(column, path)?;

                    // TODO: here we pretend the _eq, _neq etc. operators exist
                    // in OpenDD, which are then mapped to the underlying NDC
                    // operators, but we should really be using the _actual_
                    // OpenDD operators, and using the mapping defined in the
                    // scalar representation.
                    let operator = match op {
                        Operator::Eq => ComparisonOperator::Equals,
                        Operator::NotEq => ComparisonOperator::NotEquals,
                        Operator::Gt => ComparisonOperator::GreaterThan,
                        Operator::GtEq => ComparisonOperator::GreaterThanOrEqual,
                        Operator::Lt => ComparisonOperator::LessThan,
                        Operator::LtEq => ComparisonOperator::LessThanOrEqual,
                        _ => panic!("operator not handled in pushdown_filter"),
                    };

                    let argument = Box::new(open_dds::query::Value::Literal(
                        super::common::to_value(value)?,
                    ));

                    Ok(open_dds::query::BooleanExpression::Comparison {
                        operand,
                        operator,
                        argument,
                    })
                }

                Operator::LikeMatch => pushdown_like(left, right, false, '\\'),
                Operator::NotLikeMatch => {
                    let expr = pushdown_like(left, right, false, '\\')?;
                    Ok(open_dds::query::BooleanExpression::Not(Box::new(expr)))
                }
                Operator::ILikeMatch => pushdown_like(left, right, true, '\\'),
                Operator::NotILikeMatch => {
                    let expr = pushdown_like(left, right, true, '\\')?;
                    Ok(open_dds::query::BooleanExpression::Not(Box::new(expr)))
                }

                Operator::And => {
                    let left_expr = pushdown_filter(left)?;
                    let right_expr = pushdown_filter(right)?;
                    Ok(open_dds::query::BooleanExpression::And(vec![
                        left_expr, right_expr,
                    ]))
                }
                Operator::Or => {
                    let left_expr = pushdown_filter(left)?;
                    let right_expr = pushdown_filter(right)?;
                    Ok(open_dds::query::BooleanExpression::Or(vec![
                        left_expr, right_expr,
                    ]))
                }
                _ => Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported operator in filter expression: {op:?}"
                ))),
            }
        }
        Expr::Not(expr) => {
            let expr = pushdown_filter(expr)?;
            Ok(open_dds::query::BooleanExpression::Not(Box::new(expr)))
        }
        Expr::IsNull(expr_inner) | Expr::IsNotNull(expr_inner) => {
            let Some((path, column)) = super::common::try_into_column(expr_inner.as_ref())? else {
                return Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported argument in IS NULL expression: {expr_inner:?}"
                )));
            };

            let operand = super::common::to_operand(column, path)?;

            Ok(match expr {
                Expr::IsNull(_) => open_dds::query::BooleanExpression::IsNull(operand),
                Expr::IsNotNull(_) => open_dds::query::BooleanExpression::Not(Box::new(
                    open_dds::query::BooleanExpression::IsNull(operand),
                )),
                _ => panic!("operator not handled in pushdown_filter"),
            })
        }
        Expr::Like(like) => {
            let expr = pushdown_like(
                &like.expr,
                &like.pattern,
                like.case_insensitive,
                like.escape_char.unwrap_or('\\'),
            )?;

            if like.negated {
                Ok(open_dds::query::BooleanExpression::Not(Box::new(expr)))
            } else {
                Ok(expr)
            }
        }
        _ => Err(datafusion::error::DataFusionError::Internal(format!(
            "unable to push down filter expression: {expr:?}"
        ))),
    }
}

fn pushdown_like(
    expr: &datafusion::logical_expr::Expr,
    pattern: &datafusion::logical_expr::Expr,
    case_insensitive: bool,
    escape_char: char,
) -> Result<open_dds::query::BooleanExpression, datafusion::error::DataFusionError> {
    match expr {
        Expr::ScalarFunction(ScalarFunction { func, args }) if func.name() == "lower" => {
            let [inner_expr] = args.as_slice() else {
                return Err(datafusion::error::DataFusionError::Internal(
                    "unsupported arguments to LOWER".to_string(),
                ));
            };
            let Expr::Literal(ScalarValue::Utf8(Some(pattern_str))) = pattern else {
                return Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported right-hand-side in LIKE expression: {pattern:?}"
                )));
            };
            if &pattern_str.to_lowercase() != pattern_str {
                return Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported right-hand-side in LOWER() LIKE expression: {pattern:?}"
                )));
            }
            pushdown_like_impl(inner_expr, pattern, true, escape_char)
        }
        Expr::ScalarFunction(ScalarFunction { func, args }) if func.name() == "upper" => {
            let [inner_expr] = args.as_slice() else {
                return Err(datafusion::error::DataFusionError::Internal(
                    "unsupported arguments to UPPER".to_string(),
                ));
            };
            let Expr::Literal(ScalarValue::Utf8(Some(pattern_str))) = pattern else {
                return Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported right-hand-side in LIKE expression: {pattern:?}"
                )));
            };
            if &pattern_str.to_uppercase() != pattern_str {
                return Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported right-hand-side in UPPER() LIKE expression: {pattern:?}"
                )));
            }
            pushdown_like_impl(inner_expr, pattern, true, escape_char)
        }
        expr => pushdown_like_impl(expr, pattern, case_insensitive, escape_char),
    }
}

fn pushdown_like_impl(
    expr: &datafusion::logical_expr::Expr,
    pattern: &datafusion::logical_expr::Expr,
    case_insensitive: bool,
    escape_char: char,
) -> Result<open_dds::query::BooleanExpression, datafusion::error::DataFusionError> {
    let Some((path, column)) = super::common::try_into_column(expr)? else {
        return Err(datafusion::error::DataFusionError::Internal(format!(
            "unsupported left-hand-side in LIKE expression: {expr:?}"
        )));
    };

    let Expr::Literal(ScalarValue::Utf8(Some(argument))) = pattern else {
        return Err(datafusion::error::DataFusionError::Internal(format!(
            "unsupported right-hand-side in LIKE expression: {pattern:?}"
        )));
    };

    let operand = super::common::to_operand(column, path)?;

    if let Some(like_string) = like::parse_like_string(argument, escape_char) {
        let (operator, value) = match like_string {
            LikeString::Contains(substring) => (
                if case_insensitive {
                    ComparisonOperator::ContainsInsensitive
                } else {
                    ComparisonOperator::Contains
                },
                substring,
            ),
            LikeString::StartsWith(prefix) => (
                if case_insensitive {
                    ComparisonOperator::StartsWithInsensitive
                } else {
                    ComparisonOperator::StartsWith
                },
                prefix,
            ),
            LikeString::EndsWith(suffix) => (
                if case_insensitive {
                    ComparisonOperator::EndsWithInsensitive
                } else {
                    ComparisonOperator::EndsWith
                },
                suffix,
            ),
        };

        Ok(open_dds::query::BooleanExpression::Comparison {
            operand,
            operator,
            argument: Box::new(open_dds::query::Value::Literal(serde_json::Value::String(
                value,
            ))),
        })
    } else {
        Err(datafusion::error::DataFusionError::Internal(format!(
            "unsupported like string: {argument}"
        )))
    }
}
