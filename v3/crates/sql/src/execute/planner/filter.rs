use datafusion::error::{DataFusionError, Result};
use execute::plan::ResolvedFilterExpression;
use metadata_resolve::{Qualified, TypeMapping};
use open_dds::{query::BooleanExpression, types::CustomTypeName};
use std::collections::BTreeMap;

use super::common::{to_resolved_column, ResolvedColumn};

pub(crate) fn to_resolved_filter_expr(
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    expr: &BooleanExpression,
) -> Result<ResolvedFilterExpression> {
    match expr {
        BooleanExpression::And(exprs) => Ok(ResolvedFilterExpression::mk_and(
            exprs
                .iter()
                .map(|expr| {
                    to_resolved_filter_expr(
                        metadata,
                        type_mappings,
                        type_name,
                        model_object_type,
                        expr,
                    )
                })
                .collect::<Result<Vec<_>>>()?,
        )),
        BooleanExpression::Or(exprs) => Ok(ResolvedFilterExpression::mk_or(
            exprs
                .iter()
                .map(|expr| {
                    to_resolved_filter_expr(
                        metadata,
                        type_mappings,
                        type_name,
                        model_object_type,
                        expr,
                    )
                })
                .collect::<Result<Vec<_>>>()?,
        )),
        BooleanExpression::Not(expr) => Ok(ResolvedFilterExpression::mk_not(
            to_resolved_filter_expr(metadata, type_mappings, type_name, model_object_type, expr)?,
        )),
        BooleanExpression::IsNull(open_dds::query::Operand::Field(field)) => {
            let ResolvedColumn {
                column_name,
                field_path,
                field_mapping: _,
            } = to_resolved_column(metadata, type_mappings, type_name, model_object_type, field)?;
            Ok(ResolvedFilterExpression::LocalFieldComparison(
                ir::LocalFieldComparison::UnaryComparison {
                    column: ir::ComparisonTarget::Column {
                        name: column_name,
                        field_path,
                    },
                    operator: metadata_resolve::UnaryComparisonOperator::IsNull,
                },
            ))
        }
        BooleanExpression::Comparison {
            operand: open_dds::query::Operand::Field(field),
            operator,
            argument,
        } => {
            let ResolvedColumn {
                column_name,
                field_path,
                field_mapping,
            } = to_resolved_column(metadata, type_mappings, type_name, model_object_type, field)?;

            let ndc_operator = field_mapping
                .equal_operators
                .first()
                .ok_or_else(|| {
                    DataFusionError::Internal(format!(
                        "no equality operator(s) found for type: {type_name:?}"
                    ))
                })?
                .clone();

            let value = match argument.as_ref() {
                open_dds::query::Value::Literal(value) => Ok(ir::ComparisonValue::Scalar {
                    value: value.clone(),
                }),
                open_dds::query::Value::BooleanExpression(b) => Err(DataFusionError::Internal(
                    format!("boolean expressions in comparison values are not supported: {b:?}"),
                )),
            }?;

            let eq_expr = ResolvedFilterExpression::LocalFieldComparison(
                ir::LocalFieldComparison::BinaryComparison {
                    column: ir::ComparisonTarget::Column {
                        name: column_name,
                        field_path,
                    },
                    operator: ndc_operator,
                    value,
                },
            );

            match operator.as_str() {
                "_eq" => Ok(eq_expr),
                "_neq" => Ok(ResolvedFilterExpression::Not {
                    expression: Box::new(eq_expr),
                }),
                _ => Err(DataFusionError::Internal(format!(
                    "unsupported comparison operator expression: {operator:?}"
                ))),
            }
        }
        _ => Err(DataFusionError::Internal(format!(
            "unsupported boolean expression: {expr:?}"
        ))),
    }
}
