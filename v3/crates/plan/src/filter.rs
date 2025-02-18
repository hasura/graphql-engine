use crate::process_model_predicate;

use super::column::{to_resolved_column, ResolvedColumn};
use super::types::{PermissionError, PlanError};
use crate::metadata_accessor::OutputObjectTypeView;
use hasura_authn_core::Session;
use metadata_resolve::{DataConnectorLink, ObjectTypeWithRelationships, Qualified, TypeMapping};
use open_dds::{
    query::{BooleanExpression, ComparisonOperator},
    types::CustomTypeName,
};
use plan_types::{Expression, ResolvedFilterExpression, UniqueNumber, UsagesCounts};
use std::collections::BTreeMap;

pub fn to_filter_expression<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &'_ Qualified<CustomTypeName>,
    model_object_type: &'metadata OutputObjectTypeView,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    expr: &'_ BooleanExpression,
    data_connector: &'metadata DataConnectorLink,
) -> Result<Expression<'metadata>, PlanError> {
    match expr {
        BooleanExpression::And(exprs) => Ok(Expression::mk_and(
            exprs
                .iter()
                .map(|expr| {
                    to_filter_expression(
                        metadata,
                        session,
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
        BooleanExpression::Or(exprs) => Ok(Expression::mk_or(
            exprs
                .iter()
                .map(|expr| {
                    to_filter_expression(
                        metadata,
                        session,
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
        BooleanExpression::Not(expr) => Ok(Expression::mk_not(to_filter_expression(
            metadata,
            session,
            type_mappings,
            type_name,
            model_object_type,
            boolean_expression_type,
            expr,
            data_connector,
        )?)),
        BooleanExpression::IsNull(open_dds::query::Operand::Field(field)) => {
            let ResolvedColumn {
                column_name,
                field_path,
                ..
            } = to_resolved_column(
                &session.role,
                metadata,
                type_mappings,
                type_name,
                model_object_type,
                field,
            )?;
            Ok(Expression::LocalField(
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
            } = to_resolved_column(
                &session.role,
                metadata,
                type_mappings,
                type_name,
                model_object_type,
                field,
            )?;

            let value = match argument.as_ref() {
                open_dds::query::Value::Literal(value) => Ok(plan_types::ComparisonValue::Scalar {
                    value: value.clone(),
                }),
                open_dds::query::Value::BooleanExpression(b) => Err(PlanError::Internal(format!(
                    "boolean expressions in comparison values are not supported: {b:?}"
                ))),
            }?;

            // ideally everything would be resolved via boolean expression types, but
            // we need this information from the object types as we mark which operators
            // mean equality there
            let comparison_operators = field_mapping.comparison_operators.ok_or_else(|| {
                PlanError::Internal(format!(
                    "no comparisons operators found for type: {type_name:?}"
                ))
            })?;

            let ndc_version = data_connector.capabilities.supported_ndc_version;

            match operator {
                ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
                    let data_connector_operator_name = comparison_operators
                        .get_eq_operator(ndc_version)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "no equality operator(s) found for type: {type_name:?}"
                            ))
                        })?;

                    let eq_expr = Expression::LocalField(
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
                        ComparisonOperator::NotEquals => Ok(Expression::Not {
                            expression: Box::new(eq_expr),
                        }),
                        _ => {
                            panic!("invalid pattern match in to_filter_expression: {operator:?}")
                        }
                    }
                }

                ComparisonOperator::LessThan
                | ComparisonOperator::GreaterThan
                | ComparisonOperator::LessThanOrEqual
                | ComparisonOperator::GreaterThanOrEqual
                | ComparisonOperator::Contains
                | ComparisonOperator::ContainsInsensitive
                | ComparisonOperator::StartsWith
                | ComparisonOperator::StartsWithInsensitive
                | ComparisonOperator::EndsWith
                | ComparisonOperator::EndsWithInsensitive => {
                    let data_connector_operator_name = match operator {
                        ComparisonOperator::LessThan => comparison_operators.get_lt_operator(ndc_version)
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'less than' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::GreaterThan => comparison_operators
                            .get_gt_operator(ndc_version)
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'greater than' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::LessThanOrEqual => comparison_operators
                            .get_lte_operator(ndc_version)
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'less than or equal' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::GreaterThanOrEqual => comparison_operators
                            .get_gte_operator(ndc_version)
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'greater than or equal' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::Contains => comparison_operators
                            .get_contains_operator(ndc_version)
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'contains' operator found for type: {type_name:?}"
                                ))
                            }),

                        ComparisonOperator::ContainsInsensitive => comparison_operators
                            .get_icontains_operator(ndc_version)
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "no 'case-insensitive contains' operator found for type: {type_name:?}"
                                ))
                            }),

                            ComparisonOperator::StartsWith => comparison_operators
                                .get_starts_with_operator(ndc_version)
                                .ok_or_else(|| {
                                    PlanError::Internal(format!(
                                        "no 'starts with prefix' operator found for type: {type_name:?}"
                                    ))
                                }),

                            ComparisonOperator::StartsWithInsensitive => comparison_operators
                                .get_istarts_with_operator(ndc_version)
                                .ok_or_else(|| {
                                    PlanError::Internal(format!(
                                        "no 'case-insensitive starts with prefix' operator found for type: {type_name:?}"
                                    ))
                                }),

                            ComparisonOperator::EndsWith => comparison_operators
                                .get_ends_with_operator(ndc_version)
                                .ok_or_else(|| {
                                    PlanError::Internal(format!(
                                        "no 'ends with suffix' operator found for type: {type_name:?}"
                                    ))
                                }),

                            ComparisonOperator::EndsWithInsensitive => comparison_operators
                                .get_iends_with_operator(ndc_version)
                                .ok_or_else(|| {
                                    PlanError::Internal(format!(
                                        "no 'case-insensitive ends with suffix' operator found for type: {type_name:?}"
                                    ))
                                }),

                            _ => {panic!("invalid pattern match in to_filter_expression: {operator:?}")}
                    }?;

                    Ok(Expression::LocalField(
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
                    // Boolean expression type is required to resolve custom operators
                    let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
                        PlanError::Internal(
                            "Custom operators require a boolean expression type".into(),
                        )
                    })?;
                    let comparison_expression_info = boolean_expression_for_comparison(
                        metadata,
                        boolean_expression_type,
                        field,
                    )?;
                    let data_connector_operators = comparison_expression_info
                        .operator_mapping
                        .get(&data_connector.name)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "Operators for data connector {} could not be found",
                                data_connector.name
                            ))
                        })?;

                    let expr = Expression::LocalField(
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
                .get(&object_comparison_info.boolean_expression_type_name)
                .ok_or_else(|| {
                    PlanError::Internal(format!(
                        "Could not find boolean expression {}",
                        object_comparison_info.boolean_expression_type_name
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

// Resolve the model permission filter
pub(crate) fn resolve_model_permission_filter(
    session: &Session,
    model: &metadata_resolve::ModelWithPermissions,
    model_source: &metadata_resolve::ModelSource,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
    usage_counts: &mut UsagesCounts,
) -> Result<Option<ResolvedFilterExpression>, PlanError> {
    let model_name = &model.model.name;
    let model_select_permission = model.select_permissions.get(&session.role).ok_or_else(|| {
        PlanError::Permission(PermissionError::Other(format!(
            "role {} does not have select permission for model {model_name}",
            session.role
        )))
    })?;

    match &model_select_permission.filter {
        metadata_resolve::FilterPermission::AllowAll => Ok::<_, PlanError>(None),
        metadata_resolve::FilterPermission::Filter(filter) => {
            let filter_ir = process_model_predicate(
                &model_source.data_connector,
                &model_source.type_mappings,
                filter,
                &session.variables,
                object_types,
                usage_counts,
            )?;

            let (filter, _remote_predicates) = super::query::filter::resolve_filter_expression(
                &filter_ir,
                collect_relationships,
                unique_number,
            )?;

            Ok(Some(filter))
        }
    }
}
