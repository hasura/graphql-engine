use std::collections::BTreeMap;

use crate::types::PlanError;
use metadata_resolve::{Qualified, ResolvedObjectBooleanExpressionType, TypeMapping};
use open_dds::data_connector::DataConnectorName;
use open_dds::query::{BooleanExpression, Operand};
use open_dds::types::{CustomTypeName, FieldName};

// this is a happy path implementation of this function, ie, it only works for local field
// comparisons.
// TODO: boolean expression can disallow and/or/not things, so we should be checking for that
// TODO: consider relationships / nested types
pub fn open_dd_boolean_expression_to_plan_types_expression<'metadata>(
    bool_exp: &BooleanExpression,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_type_name: &Qualified<CustomTypeName>,
    data_connector_name: &Qualified<DataConnectorName>,
    boolean_expression_type: &ResolvedObjectBooleanExpressionType,
) -> Result<plan_types::Expression<'metadata>, PlanError> {
    match bool_exp {
        BooleanExpression::And(bool_exps) => {
            let expressions = bool_exps
                .iter()
                .map(|bool_exp| {
                    open_dd_boolean_expression_to_plan_types_expression(
                        bool_exp,
                        type_mappings,
                        object_type_name,
                        data_connector_name,
                        boolean_expression_type,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(plan_types::Expression::And { expressions })
        }
        BooleanExpression::Or(bool_exps) => {
            let expressions = bool_exps
                .iter()
                .map(|bool_exp| {
                    open_dd_boolean_expression_to_plan_types_expression(
                        bool_exp,
                        type_mappings,
                        object_type_name,
                        data_connector_name,
                        boolean_expression_type,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(plan_types::Expression::Or { expressions })
        }
        BooleanExpression::Not(bool_exp) => {
            let expression = open_dd_boolean_expression_to_plan_types_expression(
                bool_exp,
                type_mappings,
                object_type_name,
                data_connector_name,
                boolean_expression_type,
            )?;

            Ok(plan_types::Expression::Not {
                expression: Box::new(expression),
            })
        }
        BooleanExpression::IsNull(operand) => {
            let local_field_comparison = match operand {
                Operand::Relationship(_) => Err(PlanError::Internal(
                    "Comparisons across relationships not supported".into(),
                )),
                Operand::RelationshipAggregate(_) => Err(PlanError::Internal(
                    "Comparisons of relationship aggregate not supported".into(),
                )),

                Operand::Field(object_field_operand) => {
                    let comparison_target = to_comparison_target(
                        &object_field_operand.target.field_name,
                        type_mappings,
                        object_type_name,
                    );

                    Ok(plan_types::LocalFieldComparison::UnaryComparison {
                        column: comparison_target?,
                        operator: metadata_resolve::UnaryComparisonOperator::IsNull,
                    })
                }
            }?;
            Ok(plan_types::Expression::LocalField(local_field_comparison))
        }
        BooleanExpression::Comparison {
            operand,
            operator,
            argument,
        } => match operand {
            Operand::Relationship(_) => Err(PlanError::Internal(
                "Comparisons across relationships not supported".into(),
            )),
            Operand::RelationshipAggregate(_) => Err(PlanError::Internal(
                "Comparisons of relationship aggregate not supported".into(),
            )),
            Operand::Field(object_field_operand) => {
                let field_name = &object_field_operand.target.field_name;

                // get data connector field name
                let comparison_target =
                    to_comparison_target(field_name, type_mappings, object_type_name);

                let scalar_field = boolean_expression_type
                    .fields
                    .scalar_fields
                    .get(field_name)
                    .ok_or_else(|| {
                        PlanError::Internal(
                            format!("Could not find scalar field for {field_name}",),
                        )
                    })?;

                let data_connector_operator_mappings = scalar_field.operator_mapping.get(data_connector_name).ok_or_else(|| PlanError::Internal(format!("Could not find operator mapping for data connector {data_connector_name}")))?;

                // we'll look up ComparisonOperator::Equals using these
                // once lookup functions in #1527 are remerged
                let _comparison_operators = type_mappings
                    .get(object_type_name)
                    .and_then(|type_mapping| {
                        let TypeMapping::Object { field_mappings, .. } = type_mapping;
                        field_mappings.get(field_name)
                    })
                    .map(|field_mapping| field_mapping.comparison_operators.as_ref())
                    .ok_or_else(|| {
                        PlanError::Internal(format!("Field mapping lookup failed for {field_name}"))
                    })?;

                let operator_name = match operator {
                    open_dds::query::ComparisonOperator::Custom(operator_name) => Ok(operator_name),
                    // need #1527 remerged to do the next part
                    _ => Err(PlanError::Internal(format!(
                        "Not implemented: data connector operator lookups for {operator:?}"
                    ))),
                }?;

                // do something with value
                let comparison_value = match **argument {
                    open_dds::query::Value::Literal(ref serde_json_value) => {
                        Ok(plan_types::ComparisonValue::Scalar {
                            value: serde_json_value.clone(),
                        })
                    }
                    open_dds::query::Value::BooleanExpression(ref _bool_exp) => {
                        Err(PlanError::Internal(
                            "Cannot do a comparison against a boolean expression value".into(),
                        ))
                    }
                }?;

                let local_field_comparison = plan_types::LocalFieldComparison::BinaryComparison {
                    column: comparison_target?,
                    operator: data_connector_operator_mappings.get(operator_name).clone(),
                    value: comparison_value,
                };
                Ok(plan_types::Expression::LocalField(local_field_comparison))
            }
        },
    }
}

fn to_comparison_target(
    field_name: &FieldName,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_type_name: &Qualified<CustomTypeName>,
) -> Result<plan_types::ComparisonTarget, PlanError> {
    let data_connector_column_name = type_mappings
        .get(object_type_name)
        .and_then(|type_mapping| {
            let TypeMapping::Object { field_mappings, .. } = type_mapping;
            field_mappings.get(field_name)
        })
        .map(|field_mapping| field_mapping.column.clone())
        .ok_or_else(|| {
            PlanError::Internal(format!("Field mapping lookup failed for {field_name}"))
        })?;

    Ok(plan_types::ComparisonTarget::Column {
        name: data_connector_column_name,
        field_path: vec![], // TODO: worry about this
    })
}
