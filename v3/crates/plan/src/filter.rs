use crate::query::process_permissions;
use crate::types::PlanState;
use crate::{ModelView, process_model_predicate};
mod helpers;
use super::column::{ResolvedColumn, to_resolved_column};
use super::types::{BooleanExpressionError, PermissionError, PlanError};
use crate::metadata_accessor::OutputObjectTypeView;
use hasura_authn_core::Session;
pub use helpers::with_nesting_path;
use metadata_resolve::{
    DataConnectorLink, ModelPredicate, ObjectComparisonKind, ObjectTypeWithRelationships,
    Qualified, QualifiedBaseType, ResolvedObjectBooleanExpressionType, TypeMapping,
};
use open_dds::relationships::RelationshipName;
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorName, DataConnectorOperatorName},
    query::{BooleanExpression, ComparisonOperator},
    types::{CustomTypeName, FieldName},
};
use plan_types::{Expression, PredicateQueryTrees, ResolvedFilterExpression, UsagesCounts};
use std::collections::BTreeMap;

// we have to allow equals without a boolean expression for Select One, let's track depth
#[derive(Debug, Clone, Copy, PartialEq)]
enum Nesting {
    No,
    Array,
    NestedField,
    Relationship,
}

pub fn to_filter_expression<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    model_object_type: &'_ OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    expr: &'_ BooleanExpression,
    data_connector: &'metadata DataConnectorLink,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'metadata>, PlanError> {
    to_filter_expression_internal(
        metadata,
        session,
        type_mappings,
        model_object_type,
        boolean_expression_type,
        expr,
        data_connector,
        Nesting::No,
        plan_state,
        usage_counts,
    )
}

fn to_filter_expression_internal<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    model_object_type: &'_ OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    expr: &'_ BooleanExpression,
    data_connector: &'metadata DataConnectorLink,
    nesting: Nesting,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'metadata>, PlanError> {
    match expr {
        BooleanExpression::And(exprs) => Ok(Expression::mk_and(
            exprs
                .iter()
                .map(|expr| {
                    to_filter_expression_internal(
                        metadata,
                        session,
                        type_mappings,
                        model_object_type,
                        boolean_expression_type,
                        expr,
                        data_connector,
                        nesting,
                        plan_state,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, PlanError>>()?,
        )),
        BooleanExpression::Or(exprs) => Ok(Expression::mk_or(
            exprs
                .iter()
                .map(|expr| {
                    to_filter_expression_internal(
                        metadata,
                        session,
                        type_mappings,
                        model_object_type,
                        boolean_expression_type,
                        expr,
                        data_connector,
                        nesting,
                        plan_state,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, PlanError>>()?,
        )),
        BooleanExpression::Not(expr) => Ok(Expression::mk_not(to_filter_expression_internal(
            metadata,
            session,
            type_mappings,
            model_object_type,
            boolean_expression_type,
            expr,
            data_connector,
            nesting,
            plan_state,
            usage_counts,
        )?)),

        BooleanExpression::IsNull(operand) => to_comparison_expression(
            operand,
            Comparison::IsNull,
            &[],
            metadata,
            session,
            type_mappings,
            model_object_type,
            boolean_expression_type,
            data_connector,
            nesting,
            plan_state,
            usage_counts,
        ),

        BooleanExpression::Comparison {
            operand,
            operator,
            argument,
        } => to_comparison_expression(
            operand,
            Comparison::Binary { operator, argument },
            &[],
            metadata,
            session,
            type_mappings,
            model_object_type,
            boolean_expression_type,
            data_connector,
            nesting,
            plan_state,
            usage_counts,
        ),
        BooleanExpression::Relationship {
            relationship_name,
            predicate,
            operand,
        } => to_relationship_expression(
            relationship_name,
            operand.as_ref(),
            predicate,
            metadata,
            session,
            type_mappings,
            model_object_type,
            boolean_expression_type,
            data_connector,
            plan_state,
            usage_counts,
        ),
        BooleanExpression::Exists {
            field_name,
            predicate,
            operand,
        } => to_exists_expression(
            field_name,
            operand.as_ref(),
            predicate,
            metadata,
            session,
            type_mappings,
            model_object_type,
            boolean_expression_type,
            data_connector,
            plan_state,
            usage_counts,
        ),
    }
}

fn to_exists_expression<'metadata>(
    field_name: &FieldName,
    operand: Option<&open_dds::query::Operand>,
    predicate: &open_dds::query::BooleanExpression,
    metadata: &'metadata metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    model_object_type: &'_ OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<&'metadata ResolvedObjectBooleanExpressionType>,
    data_connector: &'metadata DataConnectorLink,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'metadata>, PlanError> {
    // Boolean expression type is required to resolve custom operators
    let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
        PlanError::Internal("Custom operators require a boolean expression type".into())
    })?;

    // we need to navigate the operand up to this exists to find the right boolean
    // expression type to start from
    let source_boolean_expression_type =
        boolean_expression_type_for_path(metadata, boolean_expression_type, operand)?;

    let TypeMapping::Object {
        ndc_object_type_name: _,
        field_mappings,
    } = type_mappings
        .get(&source_boolean_expression_type.object_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "can't find mapping object for type: {}",
                source_boolean_expression_type.object_type
            ))
        })?;

    let data_connector_column_name = field_mappings
        .get(field_name)
        .cloned()
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch field mapping of field {} in type {}",
                field_name, source_boolean_expression_type.object_type
            ))
        })?
        .column;

    // work out path of any nesting before this exists
    let column_path = column_path_for_operand(
        operand,
        metadata,
        session,
        type_mappings,
        model_object_type,
        plan_state,
    )?;

    // `column_path_for_operand` gives us the path in reverse order
    let column_path_slice: Vec<&DataConnectorColumnName> =
        column_path.as_slice().iter().rev().collect();

    let (column, field_path) =
        helpers::with_nesting_path(&data_connector_column_name, &column_path_slice);

    // first try looking for the nested field in the boolean expression type
    let Some(nested_field) = source_boolean_expression_type
        .fields
        .object_fields
        .get(field_name)
    else {
        return Err(PlanError::Permission(
            PermissionError::FieldNotFoundInBooleanExpressionType {
                field_name: field_name.clone(),
                boolean_expression_type_name: source_boolean_expression_type.name.clone(),
            },
        ));
    };

    let target_boolean_expression_type_name = &nested_field.boolean_expression_type_name;

    let target_boolean_expression_type = metadata
        .boolean_expression_types
        .objects
        .get(target_boolean_expression_type_name)
        .ok_or_else(|| {
            PlanError::Permission(PermissionError::ObjectBooleanExpressionTypeNotFound {
                boolean_expression_type_name: target_boolean_expression_type_name.clone(),
            })
        })?;

    let inner_object_type = crate::metadata_accessor::get_output_object_type(
        metadata,
        &target_boolean_expression_type.object_type,
        &session.variables,
        plan_state,
    )?;

    let inner = to_filter_expression_internal(
        metadata,
        session,
        type_mappings,
        &inner_object_type,
        Some(target_boolean_expression_type),
        predicate,
        data_connector,
        Nesting::NestedField,
        plan_state,
        usage_counts,
    )?;

    Ok(Expression::LocalNestedArray {
        column,
        field_path,
        predicate: Box::new(inner),
    })
}

fn to_relationship_expression<'metadata>(
    relationship_name: &RelationshipName,
    operand: Option<&open_dds::query::Operand>,
    predicate: &open_dds::query::BooleanExpression,
    metadata: &'metadata metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    model_object_type: &'_ OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<&'metadata ResolvedObjectBooleanExpressionType>,
    data_connector: &'metadata DataConnectorLink,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'metadata>, PlanError> {
    // Boolean expression type is required to resolve custom operators
    let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
        PlanError::Internal("Custom operators require a boolean expression type".into())
    })?;

    let field_name = open_dds::types::FieldName::new(relationship_name.clone().into_inner());

    // we need to navigate the operand up to this relationship to find the right boolean
    // expression type to start from
    let source_boolean_expression_type =
        boolean_expression_type_for_path(metadata, boolean_expression_type, operand)?;

    // the object type for the left hand side of the relationship
    let source_object_type = crate::metadata_accessor::get_output_object_type(
        metadata,
        &source_boolean_expression_type.object_type,
        &session.variables,
        plan_state,
    )?;

    // first try looking for a relationship
    if let Some(relationship_field) = source_boolean_expression_type
        .fields
        .relationship_fields
        .get(&field_name)
    {
        if let Some(target_boolean_expression_type_name) =
            &relationship_field.boolean_expression_type
        {
            let target_boolean_expression_type = metadata
                .boolean_expression_types
                .objects
                .get(target_boolean_expression_type_name)
                .ok_or_else(|| {
                    PlanError::Permission(PermissionError::ObjectBooleanExpressionTypeNotFound {
                        boolean_expression_type_name: target_boolean_expression_type_name.clone(),
                    })
                })?;

            let target_model_object_type = crate::metadata_accessor::get_output_object_type(
                metadata,
                &target_boolean_expression_type.object_type,
                &session.variables,
                plan_state,
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
                        &session.variables,
                        plan_state,
                    )?;

                    // build expression for any model permissions for the target model
                    let model_expression = model_permission_filter_to_expression(
                        session,
                        &target_model_source,
                        &metadata.object_types,
                        usage_counts,
                    )?;

                    // resolve predicate inside the relationship
                    let inner = to_filter_expression_internal(
                        metadata,
                        session,
                        &target_model_source.source.type_mappings,
                        &target_model_object_type,
                        Some(target_boolean_expression_type),
                        predicate,
                        &target_model_source.source.data_connector,
                        Nesting::Relationship,
                        plan_state,
                        usage_counts,
                    )?;

                    // include any predicates from model permissions
                    let predicate = match model_expression
                        .and_then(Expression::remove_always_true_expression)
                    {
                        Some(model_expression) => {
                            Expression::mk_and([model_expression, inner].to_vec())
                        }
                        None => inner,
                    };

                    // work out path of any nesting before the relationship
                    let column_path = column_path_for_operand(
                        operand,
                        metadata,
                        session,
                        type_mappings,
                        model_object_type,
                        plan_state,
                    )?;

                    return Ok(crate::build_relationship_comparison_expression(
                        type_mappings,
                        column_path,
                        data_connector,
                        &relationship.relationship_name,
                        &model_target.relationship_type,
                        &source_boolean_expression_type.object_type,
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
                        &model_target.mappings,
                        predicate,
                    )?);
                }
            }
        }
    }
    Err(PlanError::Permission(
        PermissionError::RelationshipNotFoundInBooleanExpressionType {
            relationship_name: relationship_name.clone(),
            boolean_expression_type_name: boolean_expression_type.name.clone(),
        },
    ))
}

fn column_path_for_operand(
    operand: Option<&open_dds::query::Operand>,
    metadata: &metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    model_object_type: &'_ OutputObjectTypeView,
    plan_state: &mut PlanState,
) -> Result<Vec<DataConnectorColumnName>, PlanError> {
    // work out path of any nesting before the relationship
    match operand {
        Some(open_dds::query::Operand::Field(object_field_operand)) => {
            let ResolvedColumn {
                column_name,
                field_path,
                field_mapping: _,
            } = to_resolved_column(
                session,
                metadata,
                type_mappings,
                model_object_type,
                object_field_operand,
                plan_state,
            )?;

            Ok(field_path.into_iter().chain([column_name]).collect())
        }
        Some(
            open_dds::query::Operand::RelationshipAggregate(_)
            | open_dds::query::Operand::Relationship(_),
        ) => Err(PlanError::Internal(
            "Operand in a relationship must be of type Field".into(),
        )),
        None => Ok(vec![]),
    }
}

fn boolean_expression_type_for_path<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    boolean_expression_type: &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    operand: Option<&open_dds::query::Operand>,
) -> Result<&'metadata ResolvedObjectBooleanExpressionType, PlanError> {
    match operand {
        None => Ok(boolean_expression_type),
        Some(open_dds::query::Operand::Field(object_field_operand)) => {
            let object_comparison_expression = boolean_expression_type
                .fields
                .object_fields
                .get(&object_field_operand.target.field_name)
                .ok_or_else(|| {
                    PlanError::Permission(PermissionError::FieldNotFoundInBooleanExpressionType {
                        field_name: object_field_operand.target.field_name.clone(),
                        boolean_expression_type_name: boolean_expression_type.name.clone(),
                    })
                })?;

            let inner_boolean_expression_type = metadata
                .boolean_expression_types
                .objects
                .get(&object_comparison_expression.boolean_expression_type_name)
                .ok_or_else(|| {
                    PlanError::Permission(PermissionError::ObjectBooleanExpressionTypeNotFound {
                        boolean_expression_type_name: object_comparison_expression
                            .boolean_expression_type_name
                            .clone(),
                    })
                })?;

            boolean_expression_type_for_path(
                metadata,
                inner_boolean_expression_type,
                object_field_operand.nested.as_deref(),
            )
        }
        Some(
            open_dds::query::Operand::Relationship(_)
            | open_dds::query::Operand::RelationshipAggregate(_),
        ) => Err(PlanError::Internal(
            "Boolean expression operands can only be a Field".into(),
        )),
    }
}

fn to_comparison_expression<'metadata>(
    operand: &open_dds::query::Operand,
    comparison: Comparison<'_>,
    column_path: &[&'_ DataConnectorColumnName],
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_object_type: &'_ OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
    nesting: Nesting,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'metadata>, PlanError> {
    match operand {
        open_dds::query::Operand::Field(object_field_operand) => to_field_comparison_expression(
            object_field_operand,
            comparison,
            column_path,
            metadata,
            session,
            type_mappings,
            source_object_type,
            boolean_expression_type,
            data_connector,
            nesting,
            plan_state,
            usage_counts,
        ),
        open_dds::query::Operand::Relationship(_) => {
            todo!("Relationship is not supported")
        }
        open_dds::query::Operand::RelationshipAggregate(_) => {
            todo!("RelationshipAggregate is not implemented")
        }
    }
}

// Resolve the model permission filter
pub(crate) fn model_permission_filter_to_expression<'metadata>(
    session: &Session,
    model: &crate::metadata_accessor::ModelView<'metadata>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    usage_counts: &mut UsagesCounts,
) -> Result<Option<Expression<'metadata>>, PlanError> {
    if model.permission.filters.is_empty() {
        Ok::<_, PlanError>(None)
    } else {
        Ok(Some(process_permissions(
            &model.source.data_connector,
            &model.source.type_mappings,
            &model.permission.filters,
            &session.variables,
            object_types,
            usage_counts,
        )?))
    }
}

enum Comparison<'a> {
    Binary {
        operator: &'a ComparisonOperator,
        argument: &'a open_dds::query::Value,
    },
    IsNull,
}

fn to_field_comparison_expression<'metadata>(
    field: &open_dds::query::ObjectFieldOperand,
    comparison: Comparison<'_>,
    column_path: &[&'_ DataConnectorColumnName],
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_object_type: &OutputObjectTypeView<'metadata>,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
    nesting: Nesting,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'metadata>, PlanError> {
    let type_name = source_object_type.object_type_name;
    if let Some(nested_field) = &field.nested {
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
                    PlanError::Permission(PermissionError::ObjectBooleanExpressionTypeNotFound {
                        boolean_expression_type_name: object_field
                            .boolean_expression_type_name
                            .clone(),
                    })
                })?;

            let target_object_type = crate::metadata_accessor::get_output_object_type(
                metadata,
                &nested_boolean_expression_type.object_type,
                &session.variables,
                plan_state,
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

            // we may still have `ObjectArray` here if the feature flag is off
            match object_field.field_kind {
                ObjectComparisonKind::Object => {
                    let mut new_column_path = Vec::from(column_path);
                    new_column_path.push(&data_connector_column_name);
                    to_comparison_expression(
                        nested_field,
                        comparison,
                        &new_column_path,
                        metadata,
                        session,
                        type_mappings,
                        &target_object_type,
                        Some(nested_boolean_expression_type),
                        data_connector,
                        Nesting::NestedField,
                        plan_state,
                        usage_counts,
                    )
                }
                ObjectComparisonKind::ObjectArray => {
                    let inner = to_comparison_expression(
                        nested_field,
                        comparison,
                        &[], // nesting "starts again" inside array
                        metadata,
                        session,
                        type_mappings,
                        &target_object_type,
                        Some(nested_boolean_expression_type),
                        data_connector,
                        Nesting::NestedField,
                        plan_state,
                        usage_counts,
                    )?;

                    let (column, field_path) =
                        helpers::with_nesting_path(&data_connector_column_name, column_path);

                    Ok(Expression::LocalNestedArray {
                        column,
                        field_path,
                        predicate: Box::new(inner),
                    })
                }
            }
        } else {
            Err(PlanError::Permission(
                PermissionError::FieldNotFoundInBooleanExpressionType {
                    field_name: field.target.field_name.clone(),
                    boolean_expression_type_name: boolean_expression_type.name.clone(),
                },
            ))
        }
    } else {
        let field_view = source_object_type
            .fields
            .get(&field.target.field_name)
            .ok_or_else(|| {
                PlanError::Internal(format!(
                    "can't find field {} in type {}",
                    field.target.field_name, type_name
                ))
            })?;

        match field_view.field_type.underlying_type {
            QualifiedBaseType::List(_) => {
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

                let (column, field_path) =
                    helpers::with_nesting_path(&data_connector_column_name, column_path);

                let inner = match comparison {
                    Comparison::Binary { operator, argument } => to_scalar_comparison_field(
                        metadata,
                        session,
                        type_mappings,
                        source_object_type,
                        boolean_expression_type,
                        data_connector,
                        &[],
                        field,
                        operator,
                        argument,
                        Nesting::Array,
                        plan_state,
                    )?,
                    Comparison::IsNull => to_is_null_field(
                        metadata,
                        session,
                        type_mappings,
                        source_object_type,
                        column_path,
                        field,
                        plan_state,
                    )?,
                };

                if !data_connector
                    .capabilities
                    .supports_nested_scalar_array_filtering
                {
                    return Err(PermissionError::NestedScalarFilteringNotSupported {
                        data_connector_name: data_connector.name.clone(),
                    }
                    .into());
                }

                Ok(Expression::LocalNestedScalarArray {
                    field_path,
                    column,
                    predicate: Box::new(inner),
                })
            }
            QualifiedBaseType::Named(_) => match comparison {
                Comparison::Binary { operator, argument } => to_scalar_comparison_field(
                    metadata,
                    session,
                    type_mappings,
                    source_object_type,
                    boolean_expression_type,
                    data_connector,
                    column_path,
                    field,
                    operator,
                    argument,
                    nesting,
                    plan_state,
                ),
                Comparison::IsNull => to_is_null_field(
                    metadata,
                    session,
                    type_mappings,
                    source_object_type,
                    column_path,
                    field,
                    plan_state,
                ),
            },
        }
    }
}

fn to_is_null_field<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_object_type: &'_ OutputObjectTypeView,
    column_path: &[&'_ DataConnectorColumnName],
    object_field_operand: &'_ open_dds::query::ObjectFieldOperand,
    plan_state: &mut PlanState,
) -> Result<Expression<'metadata>, PlanError> {
    let ResolvedColumn {
        column_name: source_column,
        field_path: more_column_path,
        field_mapping: _,
    } = to_resolved_column(
        session,
        metadata,
        type_mappings,
        source_object_type,
        object_field_operand,
        plan_state,
    )?;

    // add field path to existing
    let mut column_path: Vec<_> = column_path.iter().map(|a| (*a).clone()).collect();
    column_path.extend(more_column_path);

    // The column name is the root column
    let column_name = column_path.first().map_or(&source_column, |v| v).clone();

    // The field path is the nesting path inside the root column, if any
    let column_path: Vec<_> = column_path
        .into_iter()
        .chain([source_column])
        .skip(1)
        .collect();

    Ok(Expression::LocalField(
        plan_types::LocalFieldComparison::UnaryComparison {
            column: plan_types::ComparisonTarget::Column {
                name: column_name,
                field_path: column_path,
            },
            operator: metadata_resolve::UnaryComparisonOperator::IsNull,
        },
    ))
}

fn to_scalar_comparison_field<'metadata>(
    metadata: &'metadata metadata_resolve::Metadata,
    session: &'_ Session,
    type_mappings: &'metadata BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_object_type: &'_ OutputObjectTypeView,
    boolean_expression_type: Option<
        &'metadata metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
    data_connector: &'metadata DataConnectorLink,
    column_path: &[&'_ DataConnectorColumnName],
    object_field_operand: &'_ open_dds::query::ObjectFieldOperand,
    operator: &'_ ComparisonOperator,
    argument: &'_ open_dds::query::Value,
    nesting: Nesting,
    plan_state: &mut PlanState,
) -> Result<Expression<'metadata>, PlanError> {
    let type_name = source_object_type.object_type_name;

    let ResolvedColumn {
        column_name: source_column,
        field_path: more_column_path,
        field_mapping,
    } = to_resolved_column(
        session,
        metadata,
        type_mappings,
        source_object_type,
        object_field_operand,
        plan_state,
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
            "no comparisons operators found for type: {type_name:?}",
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

            // Select one queries do not need a boolean expression, so we have to allow non-nested
            // Equals comparison without a boolean expression type for compatibility
            if nesting != Nesting::No {
                // Boolean expression type is required to resolve equality
                let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
                    PlanError::Internal("Equality check requires a boolean expression type".into())
                })?;

                operator_reverse_lookup(
                    boolean_expression_type,
                    &object_field_operand.target.field_name,
                    &data_connector.name,
                    data_connector_operator_name,
                )?;
            }

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
                    panic!("invalid pattern match in to_filter_expression_internal: {operator:?}")
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

                            _ => {panic!("invalid pattern match in to_filter_expression_internal: {operator:?}")}
                    }?;

            // Boolean expression type is required to resolve built-in operators
            let boolean_expression_type = boolean_expression_type.ok_or_else(|| {
                BooleanExpressionError::BuiltInOperatorsRequireABooleanExpressionType {
                    object_type_name: source_object_type.object_type_name.clone(),
                }
            })?;

            // ensure we are allowed to access this operator
            operator_reverse_lookup(
                boolean_expression_type,
                &object_field_operand.target.field_name,
                &data_connector.name,
                data_connector_operator_name,
            )?;

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

// if a built-in operator is used, we can look up the operator name
fn operator_reverse_lookup(
    boolean_expression_type: &metadata_resolve::ResolvedObjectBooleanExpressionType,
    field_name: &FieldName,
    data_connector_name: &Qualified<DataConnectorName>,
    operator: &DataConnectorOperatorName,
) -> Result<(), PlanError> {
    let comparison_expression_info = boolean_expression_type
        .fields
        .scalar_fields
        .get(field_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "field {field_name} not found in boolean expression"
            ))
        })?;

    let operator_mapping = comparison_expression_info
        .operator_mapping
        .get(data_connector_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "mappings for data connector {data_connector_name} not found in boolean expression"
            ))
        })?;

    // is there a mapping to this name?
    if operator_mapping
        .0
        .iter()
        .any(|(_, operator_name)| operator_name == operator)
    {
        Ok(())
    } else {
        Err(PlanError::BooleanExpression(
            BooleanExpressionError::ComparisonOperatorNotFound {
                comparison_operator: operator.clone(),
                boolean_expression_type_name: comparison_expression_info
                    .boolean_expression_type_name
                    .clone(),
                data_connector_name: data_connector_name.clone(),
            },
        ))
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
    model_view: &ModelView,
    model_source: &metadata_resolve::ModelSource,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_predicates: &mut PredicateQueryTrees,
    plan_state: &mut PlanState,
    usage_counts: &mut UsagesCounts,
) -> Result<Option<ResolvedFilterExpression>, PlanError> {
    if model_view.permission.filters.is_empty() {
        Ok(None)
    } else {
        let filter = &ModelPredicate::And(
            model_view
                .permission
                .filters
                .iter()
                .map(|filter| (*filter).clone())
                .collect(),
        );
        let filter_ir = process_model_predicate(
            &model_source.data_connector,
            &model_source.type_mappings,
            filter,
            &session.variables,
            object_types,
            usage_counts,
        )?;

        let filter = crate::plan_expression(
            &filter_ir,
            collect_relationships,
            remote_predicates,
            plan_state,
        )?;

        Ok(filter.remove_always_true_expression())
    }
}
