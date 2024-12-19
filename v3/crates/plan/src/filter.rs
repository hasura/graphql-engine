use super::column::{to_resolved_column, ResolvedColumn};
use super::types::PlanError;
use metadata_resolve::{
    ComparisonOperators, DataConnectorLink, NdcVersion, Qualified, TypeMapping,
};
use open_dds::{
    data_connector::DataConnectorOperatorName,
    query::{BooleanExpression, ComparisonOperator},
    types::CustomTypeName,
};
use plan_types::ResolvedFilterExpression;
use std::collections::BTreeMap;

pub fn to_resolved_filter_expr(
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    boolean_expression_type: &metadata_resolve::ResolvedObjectBooleanExpressionType,
    expr: &BooleanExpression,
    data_connector: &DataConnectorLink,
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
                        data_connector,
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
                        data_connector,
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
                data_connector,
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

            match operator {
                ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
                    let data_connector_operator_name = comparison_operators
                        .eq_operator
                        .as_ref()
                        .or_else(|| {
                            find_comparison_operator(
                                "_eq",
                                &comparison_operators,
                                data_connector.capabilities.supported_ndc_version,
                            )
                        })
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "no equality operator(s) found for type: {type_name:?}"
                            ))
                        })?;

                    let eq_expr = ResolvedFilterExpression::LocalFieldComparison(
                        plan_types::LocalFieldComparison::BinaryComparison {
                            column: plan_types::ComparisonTarget::Column {
                                name: column_name,
                                field_path,
                            },
                            operator: data_connector_operator_name.clone(),
                            value,
                        },
                    );

                    match operator {
                        ComparisonOperator::Equals => Ok(eq_expr),
                        ComparisonOperator::NotEquals => Ok(ResolvedFilterExpression::Not {
                            expression: Box::new(eq_expr),
                        }),
                        _ => {
                            panic!("invalid pattern match in to_resolved_filter_expr: {operator:?}")
                        }
                    }
                }
                ComparisonOperator::LessThan
                | ComparisonOperator::GreaterThan
                | ComparisonOperator::LessThanOrEqual
                | ComparisonOperator::GreaterThanOrEqual => {
                    let data_connector_operator_name = match operator {
                        ComparisonOperator::LessThan => comparison_operators
                            .lt_operator
                            .as_ref()
                            .or_else(|| find_comparison_operator("_lt", &comparison_operators,
                                    data_connector.capabilities.supported_ndc_version,
))
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'less than' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::GreaterThan => comparison_operators
                            .gt_operator
                            .as_ref()
                            .or_else(|| find_comparison_operator("_gt", &comparison_operators,
data_connector.capabilities.supported_ndc_version,

                                    ))
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'greater than' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::LessThanOrEqual => comparison_operators
                            .lte_operator
                            .as_ref()
                            .or_else(|| find_comparison_operator("_lte", &comparison_operators,
                                    data_connector.capabilities.supported_ndc_version,

                                    ))
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'less than or equal' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::GreaterThanOrEqual => comparison_operators
                            .gte_operator
                            .as_ref()
                            .or_else(|| find_comparison_operator("_gte", &comparison_operators,
data_connector.capabilities.supported_ndc_version,

                                    ))
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'greater than or equal' operator found for type: {type_name:?}"
                                ))
                            }),_ => {panic!("invalid pattern match in to_resolved_filter_expr: {operator:?}")}
                    }?;

                    Ok(ResolvedFilterExpression::LocalFieldComparison(
                        plan_types::LocalFieldComparison::BinaryComparison {
                            column: plan_types::ComparisonTarget::Column {
                                name: column_name,
                                field_path,
                            },
                            operator: data_connector_operator_name.clone(),
                            value,
                        },
                    ))
                }
                ComparisonOperator::Custom(custom_operator) => {
                    let data_connector_operators = comparison_expression_info
                        .operator_mapping
                        .get(&data_connector.name)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "Operators for data connector {} could not be found",
                                data_connector.name
                            ))
                        })?;

                    let expr = ResolvedFilterExpression::LocalFieldComparison(
                        plan_types::LocalFieldComparison::BinaryComparison {
                            column: plan_types::ComparisonTarget::Column {
                                name: column_name,
                                field_path,
                            },
                            operator: data_connector_operators.get(custom_operator).clone(),
                            value,
                        },
                    );

                    Ok(expr)
                }
            }
        }
        _ => Err(PlanError::Internal(format!(
            "unsupported boolean expression: {expr:?}"
        ))),
    }
}

// TODO: this is a very crude backup lookup for operators.
// We keep it because v0.1 connectors don't have the newer
// set of comparison operator meanings (lt, lte, gt, gte),
// so until more connectors are on v0.2, we need a heuristic
// for finding these operators here.
fn find_comparison_operator<'a>(
    operator_str: &str,
    comparison_operators: &'a ComparisonOperators,
    ndc_version: NdcVersion,
) -> Option<&'a DataConnectorOperatorName> {
    match ndc_version {
        NdcVersion::V01 => comparison_operators
            .other_operators
            .iter()
            .find(|other_op| {
                other_op.as_str() == operator_str
                    || operator_str
                        .strip_prefix("_")
                        .is_some_and(|op| other_op.as_str() == op)
            }),
        NdcVersion::V02 => None,
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
