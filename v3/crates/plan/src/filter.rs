use crate::process_model_predicate;

use super::column::{to_resolved_column, ResolvedColumn};
use super::types::{PermissionError, PlanError};
use crate::metadata_accessor::OutputObjectTypeView;
use hasura_authn_core::Session;
use metadata_resolve::{DataConnectorLink, ObjectTypeWithRelationships, Qualified, TypeMapping};
use open_dds::{
    data_connector::DataConnectorColumnName,
    query::{BooleanExpression, ComparisonOperator},
    types::CustomTypeName,
};
use plan_types::{Expression, ResolvedFilterExpression, UniqueNumber, UsagesCounts};
use std::collections::BTreeMap;

pub fn to_filter_expression<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &'metadata Qualified<CustomTypeName>,
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
            operand,
            operator,
            argument,
        } => to_comparison_expression(
            operand,
            operator,
            argument,
            &[],
            metadata,
            session,
            type_mappings,
            type_name,
            model_object_type,
            boolean_expression_type,
            data_connector,
        ),
        BooleanExpression::IsNull(_) => Err(PlanError::Internal(format!(
            "unsupported boolean expression: {expr:?}"
        ))),
    }
}

fn to_comparison_expression<'metadata>(
    operand: &open_dds::query::Operand,
    operator: &ComparisonOperator,
    argument: &open_dds::query::Value,
    column_path: &[&'_ DataConnectorColumnName],
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &'metadata Qualified<CustomTypeName>,
    source_object_type: &OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
) -> Result<Expression<'metadata>, PlanError> {
    match operand {
        open_dds::query::Operand::Field(object_field_operand) => to_field_comparison_expression(
            object_field_operand,
            operator,
            argument,
            column_path,
            metadata,
            session,
            type_mappings,
            type_name,
            source_object_type,
            boolean_expression_type,
            data_connector,
        ),
        open_dds::query::Operand::Relationship(relationship_operand) => {
            to_relationship_comparison_expression(
                relationship_operand,
                operator,
                argument,
                metadata,
                session,
                type_mappings,
                type_name,
                source_object_type,
                boolean_expression_type,
                data_connector,
            )
        }
        open_dds::query::Operand::RelationshipAggregate(_) => {
            todo!("RelationshipAggregate is not implemented")
        }
    }
}

fn to_relationship_comparison_expression<'metadata>(
    field: &open_dds::query::RelationshipOperand,
    operator: &ComparisonOperator,
    argument: &open_dds::query::Value,
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &'metadata Qualified<CustomTypeName>,
    source_object_type: &OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
) -> Result<Expression<'metadata>, PlanError> {
    match &field.nested {
        // the nested field here is the target of the relationship
        Some(nested_field) => {
            // Boolean expression type is required to resolve custom operators
            let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
                PlanError::Internal("Custom operators require a boolean expression type".into())
            })?;

            let field_name = open_dds::types::FieldName::new(
                field.target.relationship_name.clone().into_inner(),
            );

            // first try looking for a relationship
            if let Some(relationship_field) = boolean_expression_type
                .fields
                .relationship_fields
                .get(&field_name)
            {
                let target_boolean_expression_type = metadata
                    .boolean_expression_types
                    .objects
                    .get(&relationship_field.boolean_expression_type)
                    .ok_or_else(|| {
                        PlanError::Permission(
                            PermissionError::ObjectBooleanExpressionTypeNotFound {
                                boolean_expression_type_name: relationship_field
                                    .boolean_expression_type
                                    .clone(),
                            },
                        )
                    })?;

                let target_model_object_type = crate::metadata_accessor::get_output_object_type(
                    metadata,
                    &target_boolean_expression_type.object_type,
                    &session.role,
                )?;

                // look up relationship on the source model
                let relationship = source_object_type
                    .relationship_fields
                    .get(&relationship_field.relationship_name)
                    .ok_or_else(|| PermissionError::RelationshipNotFound {
                        object_type_name: target_boolean_expression_type.object_type.clone(),
                        relationship_name: relationship_field.relationship_name.clone(),
                    })?;

                match &relationship.target {
                    metadata_resolve::RelationshipTarget::Command(_) => {
                        todo!("command target not supported")
                    }
                    metadata_resolve::RelationshipTarget::Model(model_target) => {
                        let target_model_source = crate::metadata_accessor::get_model(
                            metadata,
                            &model_target.model_name,
                            &session.role,
                        )?;

                        // reset column path as we're starting from root inside
                        // the relationship
                        let column_path = &[];

                        let inner = to_comparison_expression(
                            nested_field,
                            operator,
                            argument,
                            column_path,
                            metadata,
                            session,
                            &target_model_source.source.type_mappings,
                            &target_boolean_expression_type.object_type,
                            &target_model_object_type,
                            Some(target_boolean_expression_type),
                            data_connector,
                        )?;

                        Ok(crate::build_relationship_comparison_expression(
                            type_mappings,
                            column_path,
                            data_connector,
                            &relationship.relationship_name,
                            &model_target.relationship_type,
                            type_name,
                            &model_target.model_name,
                            target_model_source.source,
                            relationship.target_capabilities.as_ref().ok_or_else(|| {
                                PermissionError::InternalMissingRelationshipCapabilities {
                                    relationship_name: relationship.relationship_name.clone(),
                                    object_type_name: target_boolean_expression_type
                                        .object_type
                                        .clone(),
                                }
                            })?,
                            &model_target.target_typename,
                            &model_target.mappings,
                            inner,
                        )?)
                    }
                }
            } else {
                Err(PlanError::Permission(
                    PermissionError::RelationshipNotFoundInBooleanExpressionType {
                        relationship_name: field.target.relationship_name.clone(),
                        boolean_expression_type_name: boolean_expression_type.name.clone(),
                    },
                ))
            }
        }
        None => {
            todo!("Relationship without field inside")
        }
    }
}

fn to_field_comparison_expression<'metadata>(
    field: &open_dds::query::ObjectFieldOperand,
    operator: &ComparisonOperator,
    argument: &open_dds::query::Value,
    column_path: &[&'_ DataConnectorColumnName],
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &'metadata Qualified<CustomTypeName>,
    source_object_type: &OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
) -> Result<Expression<'metadata>, PlanError> {
    match &field.nested {
        Some(nested_field) => {
            // Boolean expression type is required to resolve custom operators
            let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
                PlanError::Internal("Custom operators require a boolean expression type".into())
            })?;

            if let Some(object_field) = boolean_expression_type
                .fields
                .object_fields
                .get(&field.target.field_name)
            {
                let nested_boolean_expression_type = metadata
                    .boolean_expression_types
                    .objects
                    .get(&object_field.boolean_expression_type_name)
                    .ok_or_else(|| {
                        PlanError::Permission(
                            PermissionError::ObjectBooleanExpressionTypeNotFound {
                                boolean_expression_type_name: object_field
                                    .boolean_expression_type_name
                                    .clone(),
                            },
                        )
                    })?;

                let target_object_type = crate::metadata_accessor::get_output_object_type(
                    metadata,
                    &nested_boolean_expression_type.object_type,
                    &session.role,
                )?;

                let TypeMapping::Object {
                    ndc_object_type_name: _,
                    field_mappings,
                } = type_mappings.get(type_name).ok_or_else(|| {
                    PlanError::Internal(format!("can't find mapping object for type: {type_name}"))
                })?;

                let data_connector_column_name = field_mappings
                    .get(&field.target.field_name)
                    .cloned()
                    .ok_or_else(|| {
                        PlanError::Internal(format!(
                            "couldn't fetch field mapping of field {} in type {}",
                            field.target.field_name, type_name
                        ))
                    })?
                    .column;

                let mut new_column_path = Vec::from(column_path);
                new_column_path.push(&data_connector_column_name);

                to_comparison_expression(
                    nested_field,
                    operator,
                    argument,
                    &new_column_path,
                    metadata,
                    session,
                    type_mappings,
                    &nested_boolean_expression_type.object_type,
                    &target_object_type,
                    Some(nested_boolean_expression_type),
                    data_connector,
                )
            } else {
                Err(PlanError::Permission(
                    PermissionError::FieldNotFoundInBooleanExpressionType {
                        field_name: field.target.field_name.clone(),
                        boolean_expression_type_name: boolean_expression_type.name.clone(),
                    },
                ))
            }
        }
        None => from_object_field(
            metadata,
            session,
            type_mappings,
            type_name,
            source_object_type,
            boolean_expression_type,
            data_connector,
            column_path,
            field,
            operator,
            argument,
        ),
    }
}

fn from_object_field<'metadata, 'other>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &'other Qualified<CustomTypeName>,
    source_object_type: &'other OutputObjectTypeView,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
    column_path: &[&'_ DataConnectorColumnName],
    object_field_operand: &'_ open_dds::query::ObjectFieldOperand,
    operator: &'_ ComparisonOperator,
    argument: &'_ open_dds::query::Value,
) -> Result<Expression<'metadata>, PlanError> {
    let ResolvedColumn {
        column_name: source_column,
        field_path: more_column_path,
        field_mapping,
    } = to_resolved_column(
        &session.role,
        metadata,
        type_mappings,
        type_name,
        source_object_type,
        object_field_operand,
    )?;

    // add field path to existing
    let mut column_path: Vec<_> = column_path.iter().map(|a| (*a).clone()).collect();
    column_path.extend(more_column_path);

    let value = match argument {
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

    // The column name is the root column
    let column_name = column_path.first().map_or(&source_column, |v| v).clone();
    // The field path is the nesting path inside the root column, if any
    let column_path: Vec<_> = column_path
        .into_iter()
        .chain([source_column])
        .skip(1)
        .collect();

    match operator {
        ComparisonOperator::Equals | ComparisonOperator::NotEquals => {
            let data_connector_operator_name = comparison_operators
                .get_eq_operator(ndc_version)
                .ok_or_else(|| {
                    PlanError::Internal(format!(
                        "no equality operator(s) found for type: {type_name:?}"
                    ))
                })?;

            let eq_expr =
                Expression::LocalField(plan_types::LocalFieldComparison::BinaryComparison {
                    column: plan_types::ComparisonTarget::Column {
                        name: column_name,
                        field_path: column_path,
                    },
                    operator: data_connector_operator_name.clone(),
                    value,
                });

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
                        field_path: column_path,
                    },
                    operator: data_connector_operator_name.clone(),
                    value,
                },
            ))
        }
        ComparisonOperator::Custom(custom_operator) => {
            // Boolean expression type is required to resolve custom operators
            let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
                PlanError::Internal("Custom operators require a boolean expression type".into())
            })?;
            let comparison_expression_info = boolean_expression_for_comparison(
                metadata,
                boolean_expression_type,
                object_field_operand,
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

            let expr = Expression::LocalField(plan_types::LocalFieldComparison::BinaryComparison {
                column: plan_types::ComparisonTarget::Column {
                    name: column_name,
                    field_path: column_path,
                },
                operator: data_connector_operators.get(custom_operator).clone(),
                value,
            });

            Ok(expr)
        }
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
