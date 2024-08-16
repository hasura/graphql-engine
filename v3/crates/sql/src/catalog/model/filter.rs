use std::sync::Arc;

use crate::catalog::model::Model;
use datafusion::{
    common::Column,
    error::{DataFusionError, Result},
    logical_expr::{expr::ScalarFunction, BinaryExpr, Expr, Operator, TableProviderFilterPushDown},
    scalar::ScalarValue,
};
use indexmap::IndexMap;
use metadata_resolve::Metadata;
use open_dds::{
    identifier::Identifier,
    query::{ObjectFieldOperand, ObjectFieldTarget},
    types::{FieldName, OperatorName},
};

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

pub(crate) fn try_into_column(expr: &Expr) -> Result<Option<(Vec<FieldName>, &Column)>> {
    let mut path = vec![];
    let mut expr = expr;

    loop {
        match expr {
            Expr::Column(column) => {
                return Ok(Some((path, column)));
            }
            Expr::ScalarFunction(ScalarFunction { func, args }) if func.name() == "get_field" => {
                let [inner_expr, Expr::Literal(ScalarValue::Utf8(Some(field_name)))] =
                    args.as_slice()
                else {
                    return Ok(None);
                };

                let ident = Identifier::new(field_name).map_err(|e| {
                    DataFusionError::Internal(format!("invalid identifier in path: {e}"))
                })?;

                path.push(FieldName::new(ident));
                expr = inner_expr;
            }
            _ => {
                return Ok(None);
            }
        }
    }
}

pub(crate) fn pushdown_filter(
    expr: &datafusion::prelude::Expr,
) -> datafusion::error::Result<open_dds::query::BooleanExpression> {
    match expr {
        Expr::BinaryExpr(BinaryExpr { left, op, right }) => {
            match op {
                Operator::Eq | Operator::NotEq => {
                    // | Operator::Lt
                    // | Operator::LtEq
                    // | Operator::Gt
                    // | Operator::GtEq
                    let Some((path, column)) = try_into_column(left.as_ref())? else {
                        return Err(datafusion::error::DataFusionError::Internal(format!(
                            "unsupported left-hand-side in binary expression: {left:?}"
                        )));
                    };

                    let Expr::Literal(value) = right.as_ref() else {
                        return Err(datafusion::error::DataFusionError::Internal(format!(
                            "unsupported right-hand-side in binary expression: {left:?}"
                        )));
                    };

                    let operand = to_operand(column, path)?;

                    // TODO: here we pretend the _eq, _neq etc. operators exist
                    // in OpenDD, which are then mapped to the underlying NDC
                    // operators, but we should really be using the _actual_
                    // OpenDD operators, and using the mapping defined in the
                    // scalar representation.
                    let operator = match op {
                        Operator::Eq => OperatorName::from("_eq"),
                        Operator::NotEq => OperatorName::from("_neq"),
                        _ => panic!("operator not handled in pushdown_filter"),
                    };

                    let argument = Box::new(open_dds::query::Value::Literal(to_value(value)?));

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
            let Some((path, column)) = try_into_column(expr_inner.as_ref())? else {
                return Err(datafusion::error::DataFusionError::Internal(format!(
                    "unsupported argument in IS NULL expression: {expr_inner:?}"
                )));
            };

            let operand = to_operand(column, path)?;

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

pub(crate) fn to_operand(
    column: &datafusion::prelude::Column,
    path: Vec<FieldName>,
) -> ::datafusion::error::Result<open_dds::query::Operand> {
    let mut nested = None;

    let mut path_rev = path;
    path_rev.reverse();

    for field_name in path_rev {
        nested = Some(Box::new(open_dds::query::Operand::Field(
            ObjectFieldOperand {
                target: Box::new(ObjectFieldTarget {
                    field_name,
                    arguments: IndexMap::new(),
                }),
                nested: None,
            },
        )));
    }

    Ok(open_dds::query::Operand::Field(ObjectFieldOperand {
        target: Box::new(ObjectFieldTarget {
            field_name: FieldName::new(Identifier::new(column.name.clone()).map_err(|e| {
                datafusion::error::DataFusionError::Internal(format!(
                    "cannot convert binary expr left-hand-side: {e}"
                ))
            })?),
            arguments: IndexMap::new(),
        }),
        nested,
    }))
}

pub(crate) fn to_value(
    value: &datafusion::scalar::ScalarValue,
) -> datafusion::error::Result<serde_json::Value> {
    match value {
        datafusion::scalar::ScalarValue::Null => Ok(serde_json::Value::Null),
        datafusion::scalar::ScalarValue::Boolean(b) => {
            Ok(b.map_or(serde_json::Value::Null, serde_json::Value::Bool))
        }
        datafusion::scalar::ScalarValue::Float32(f) => {
            Ok(f.map_or(serde_json::Value::Null, serde_json::Value::from))
        }
        datafusion::scalar::ScalarValue::Int32(i) => {
            Ok(i.map_or(serde_json::Value::Null, serde_json::Value::from))
        }
        datafusion::scalar::ScalarValue::Utf8(s) => {
            Ok(s.as_ref().map_or(serde_json::Value::Null, |s| {
                serde_json::Value::from(s.clone())
            }))
        }
        // datafusion::scalar::ScalarValue::Float16(f) => todo!(),
        // datafusion::scalar::ScalarValue::Float64(f) => todo!(),
        // datafusion::scalar::ScalarValue::Decimal128(_, _, _) => todo!(),
        // datafusion::scalar::ScalarValue::Decimal256(_, _, _) => todo!(),
        // datafusion::scalar::ScalarValue::Int8(_) => todo!(),
        // datafusion::scalar::ScalarValue::Int16(_) => todo!(),
        // datafusion::scalar::ScalarValue::Int64(_) => todo!(),
        // datafusion::scalar::ScalarValue::UInt8(_) => todo!(),
        // datafusion::scalar::ScalarValue::UInt16(_) => todo!(),
        // datafusion::scalar::ScalarValue::UInt32(_) => todo!(),
        // datafusion::scalar::ScalarValue::UInt64(_) => todo!(),
        // datafusion::scalar::ScalarValue::Date32(_) => todo!(),
        // datafusion::scalar::ScalarValue::Date64(_) => todo!(),
        // datafusion::scalar::ScalarValue::Time32Second(_) => todo!(),
        // datafusion::scalar::ScalarValue::Time32Millisecond(_) => todo!(),
        // datafusion::scalar::ScalarValue::Time64Microsecond(_) => todo!(),
        // datafusion::scalar::ScalarValue::Time64Nanosecond(_) => todo!(),
        // datafusion::scalar::ScalarValue::TimestampSecond(_, _) => todo!(),
        // datafusion::scalar::ScalarValue::TimestampMillisecond(_, _) => todo!(),
        // datafusion::scalar::ScalarValue::TimestampMicrosecond(_, _) => todo!(),
        // datafusion::scalar::ScalarValue::TimestampNanosecond(_, _) => todo!(),
        _ => Err(DataFusionError::Internal(format!(
            "cannot convert literal to OpenDD literal: {value:?}"
        ))),
    }
}
