use std::collections::BTreeMap;

use crate::types::PlanError;
use metadata_resolve::{Qualified, TypeMapping};
use open_dds::query::{BooleanExpression, Operand};
use open_dds::types::{CustomTypeName, FieldName};

// this is a happy path implementation of this function, ie, it only works for local field
// comparisons. at time of writing we do not construct any non-trivial
// open_dds::query::BooleanExpression values anyway, so this will need finishing when we come to make
// user-passed filters work in either JSONAPI or GraphQL.
// TODO: resolve this in context of BooleanExpression for the model
// TODO: boolean expression can disallow and/or/not things, so we should be checking for that
pub fn open_dd_boolean_expression_to_plan_types_expression<'metadata>(
    bool_exp: &BooleanExpression,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_type_name: &Qualified<CustomTypeName>,
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
            operator: _,
            argument,
        } => match operand {
            Operand::Relationship(_) => Err(PlanError::Internal(
                "Comparisons across relationships not supported".into(),
            )),
            Operand::RelationshipAggregate(_) => Err(PlanError::Internal(
                "Comparisons of relationship aggregate not supported".into(),
            )),
            Operand::Field(object_field_operand) => {
                // get data connector field name
                let comparison_target = to_comparison_target(
                    &object_field_operand.target.field_name,
                    type_mappings,
                    object_type_name,
                );

                // get data connector operator name
                // first we need to lookup the boolean expression type for this field
                // then use it to lookup the data connector's name for the operator
                let data_connector_operator_name = "wrong".into();

                // do something with value
                let comparison_value = match **argument {
                    open_dds::query::Value::Literal(ref serde_json_value) => {
                        Ok(plan_types::ComparisonValue::Scalar {
                            value: serde_json_value.clone(),
                        })
                    }
                    open_dds::query::Value::BooleanExpression(ref _bool_exp) => {
                        // not supporting for now, but I'm pretty sure we'll need to convert this
                        // BooleanExpression type into an `ndc_models::Expression` type and then
                        // turn that into JSON
                        Err(PlanError::Internal(
                            "User-supplied boolean expression arguments not currently supported"
                                .into(),
                        ))
                    }
                }?;

                let local_field_comparison = plan_types::LocalFieldComparison::BinaryComparison {
                    column: comparison_target?,
                    operator: data_connector_operator_name,
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
