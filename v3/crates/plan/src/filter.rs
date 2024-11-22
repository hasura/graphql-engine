use super::types::PlanError;
use metadata_resolve::{Qualified, TypeMapping};
use open_dds::{
    data_connector::DataConnectorName, query::BooleanExpression, types::CustomTypeName,
};
use plan_types::ResolvedFilterExpression;
use std::collections::BTreeMap;

use super::column::{to_resolved_column, ResolvedColumn};

pub fn to_resolved_filter_expr(
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    boolean_expression_type: &metadata_resolve::ResolvedObjectBooleanExpressionType,
    expr: &BooleanExpression,
    data_connector_name: &Qualified<DataConnectorName>,
) -> Result<ResolvedFilterExpression, PlanError> {
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
                        boolean_expression_type,
                        expr,
                        data_connector_name,
                    )
                })
                .collect::<Result<Vec<_>, PlanError>>()?,
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
                        boolean_expression_type,
                        expr,
                        data_connector_name,
                    )
                })
                .collect::<Result<Vec<_>, PlanError>>()?,
        )),
        BooleanExpression::Not(expr) => {
            Ok(ResolvedFilterExpression::mk_not(to_resolved_filter_expr(
                metadata,
                type_mappings,
                type_name,
                model_object_type,
                boolean_expression_type,
                expr,
                data_connector_name,
            )?))
        }
        BooleanExpression::IsNull(open_dds::query::Operand::Field(field)) => {
            let ResolvedColumn {
                column_name,
                field_path,
                ..
            } = to_resolved_column(metadata, type_mappings, type_name, model_object_type, field)?;
            Ok(ResolvedFilterExpression::LocalFieldComparison(
                plan_types::LocalFieldComparison::UnaryComparison {
                    column: plan_types::ComparisonTarget::Column {
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

            let value = match argument.as_ref() {
                open_dds::query::Value::Literal(value) => Ok(plan_types::ComparisonValue::Scalar {
                    value: value.clone(),
                }),
                open_dds::query::Value::BooleanExpression(b) => Err(PlanError::Internal(format!(
                    "boolean expressions in comparison values are not supported: {b:?}"
                ))),
            }?;

            let comparison_expression_info =
                boolean_expression_for_comparison(metadata, boolean_expression_type, field)?;

            // ideally everything would be resolved via boolean expression types, but
            // we need this information from the object types as we mark which operators
            // mean equality there
            let comparison_operators = field_mapping.comparison_operators.ok_or_else(|| {
                PlanError::Internal(format!(
                    "no comparisons operators found for type: {type_name:?}"
                ))
            })?;

            let operator_str = operator.as_str();

            // if these operators really are special cased we should consider change the string
            // representation for an enum
            if operator_str == "_eq" || operator_str == "_neq" {
                let operator = comparison_operators
                    .equality_operators
                    .first()
                    .ok_or_else(|| {
                        PlanError::Internal(format!(
                            "no equality operator(s) found for type: {type_name:?}"
                        ))
                    })?
                    .clone();

                let eq_expr = ResolvedFilterExpression::LocalFieldComparison(
                    plan_types::LocalFieldComparison::BinaryComparison {
                        column: plan_types::ComparisonTarget::Column {
                            name: column_name,
                            field_path,
                        },
                        operator,
                        value,
                    },
                );

                match operator_str {
                    "_eq" => Ok(eq_expr),
                    "_neq" => Ok(ResolvedFilterExpression::Not {
                        expression: Box::new(eq_expr),
                    }),
                    _ => panic!("invalid pattern match in to_resolved_filter_expr: {operator_str}"),
                }
            } else {
                let data_connector_operators = comparison_expression_info
                    .operator_mapping
                    .get(data_connector_name)
                    .ok_or_else(|| {
                        PlanError::Internal(format!(
                            "Operators for data connector {data_connector_name} could not be found"
                        ))
                    })?;

                let expr = ResolvedFilterExpression::LocalFieldComparison(
                    plan_types::LocalFieldComparison::BinaryComparison {
                        column: plan_types::ComparisonTarget::Column {
                            name: column_name,
                            field_path,
                        },
                        operator: data_connector_operators.get(operator).clone(),
                        value,
                    },
                );

                Ok(expr)
            }
        }
        _ => Err(PlanError::Internal(format!(
            "unsupported boolean expression: {expr:?}"
        ))),
    }
}

// follow fields until we get to leaf boolean expression
fn boolean_expression_for_comparison(
    metadata: &metadata_resolve::Metadata,
    boolean_expression_type: &metadata_resolve::ResolvedObjectBooleanExpressionType,
    operand: &open_dds::query::ObjectFieldOperand,
) -> Result<metadata_resolve::ComparisonExpressionInfo, PlanError> {
    let open_dds::query::ObjectFieldOperand { target, nested } = operand;
    match nested {
        None => {
            // it's a leaf, so a scalar
            boolean_expression_type
                .fields
                .scalar_fields
                .get(&target.field_name)
                .ok_or_else(|| {
                    PlanError::Internal(format!("scalar field {} not found", target.field_name))
                })
                .cloned()
        }
        Some(operand) => {
            // it's a nested object (or relationship, not implemented yet)
            let object_comparison_info = boolean_expression_type
                .fields
                .object_fields
                .get(&target.field_name)
                .ok_or_else(|| {
                    PlanError::Internal(format!("object field {} not found", target.field_name))
                })?;

            let object_boolean_expression_type = metadata
                .boolean_expression_types
                .objects
                .get(&object_comparison_info.object_type_name)
                .ok_or_else(|| {
                    PlanError::Internal(format!(
                        "Could not find boolean expression {}",
                        object_comparison_info.object_type_name
                    ))
                })?;
            match operand.as_ref() {
                open_dds::query::Operand::Field(field_operand) => {
                    boolean_expression_for_comparison(
                        metadata,
                        object_boolean_expression_type,
                        field_operand,
                    )
                }
                _ => Err(PlanError::Internal(
                    "Relationships in boolean expressions not supported".into(),
                )),
            }
        }
    }
}
