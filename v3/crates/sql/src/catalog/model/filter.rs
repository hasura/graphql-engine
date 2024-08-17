use std::sync::Arc;

use crate::catalog::model::Model;
use datafusion::logical_expr::{BinaryExpr, Expr, Operator, TableProviderFilterPushDown};
use metadata_resolve::Metadata;
use open_dds::types::OperatorName;

pub(crate) fn can_pushdown_filters(
    _metadata: &Arc<Metadata>,
    _model: &Arc<Model>,
    filters: &[&Expr],
) -> Vec<TableProviderFilterPushDown> {
    let mut results = vec![];

    // let object_type =
    //     metadata
    //     .object_types
    //     .get(&self.model.data_type)
    //     .ok_or_else(|| {
    //         DataFusionError::Internal(
    //             format!("unable to find object type {}", model.data_type).into(),
    //         )
    //     })?;

    // let (_, boolean_expression_type) = metadata
    //     .boolean_expression_types
    //     .objects
    //     .iter()
    //     .find(|(_, value)| value.object_type == model.data_type)
    //     .ok_or_else(|| {
    //         DataFusionError::Internal(
    //             format!(
    //                 "unable to find boolean_expression_type for type {}",
    //                 model.data_type
    //             )
    //             .into(),
    //         )
    //     })?;

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
                        Operator::Eq => OperatorName::from("_eq"),
                        Operator::NotEq => OperatorName::from("_neq"),
                        Operator::Gt => OperatorName::from("_gt"),
                        Operator::GtEq => OperatorName::from("_gte"),
                        Operator::Lt => OperatorName::from("_lt"),
                        Operator::LtEq => OperatorName::from("_lte"),
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
        // Expr::Like(_) => todo!(),
        _ => Err(datafusion::error::DataFusionError::Internal(format!(
            "unable to push down filter expression: {expr:?}"
        ))),
    }
}
