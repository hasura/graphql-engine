use datafusion::{
    common::Column,
    error::DataFusionError,
    logical_expr::{expr::ScalarFunction, Expr},
    scalar::ScalarValue,
};
use indexmap::IndexMap;
use open_dds::{
    identifier::Identifier,
    query::{ObjectFieldOperand, ObjectFieldTarget},
    types::FieldName,
};

pub(crate) fn try_into_column(
    expr: &Expr,
) -> datafusion::error::Result<Option<(Vec<FieldName>, &Column)>> {
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

pub(crate) fn to_operand(
    column: &datafusion::prelude::Column,
    path: Vec<FieldName>,
) -> datafusion::error::Result<open_dds::query::Operand> {
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
