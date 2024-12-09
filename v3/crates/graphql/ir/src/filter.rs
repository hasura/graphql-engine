use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve::{DataConnectorLink, FieldMapping, Qualified};
use serde::Serialize;
use std::collections::BTreeMap;
use std::ops::Deref;

use crate::{error, permissions};
use graphql_schema::{self};
use graphql_schema::{BooleanExpressionAnnotation, InputAnnotation, ObjectFieldKind};
use graphql_schema::{
    FilterRelationshipAnnotation, ObjectBooleanExpressionField, ScalarBooleanExpressionField,
};
use graphql_schema::{LogicalOperatorField, GDS};
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::CustomTypeName,
};
use plan::count_model;
use plan_types::{
    ComparisonTarget, ComparisonValue, Expression, LocalFieldComparison, UsagesCounts,
};

/// Filter expression to be applied on a model/command selection set
#[derive(Debug, Serialize, Clone)]
pub struct FilterExpression<'s> {
    /// Filter obtained from the GraphQL query request
    pub query_filter: QueryFilter<'s>,
    /// Permission filters
    pub permission_filter: Option<Expression<'s>>,
    /// Filter for representing local relationship joins
    pub relationship_join_filter: Option<Expression<'s>>,
}

/// Filter expression derived from GraphQL query.
#[derive(Debug, Serialize, Clone)]
pub struct QueryFilter<'s> {
    /// Filter derived from `where` clause.
    pub where_clause: Option<Expression<'s>>,
    /// Relay global ID or unique field comparisons
    pub additional_filter: Option<Expression<'s>>,
}

/// Generate the IR for GraphQL 'where' boolean expression
pub fn resolve_filter_expression<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, error::Error> {
    resolve_object_boolean_expression(
        fields,
        data_connector_link,
        type_mappings,
        &[],
        session_variables,
        usage_counts,
    )
}

fn resolve_object_boolean_expression<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    column_path: &[&'s DataConnectorColumnName],
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, error::Error> {
    let field_expressions = fields
        .values()
        .map(|field| {
            let field_annotation =
                extract_object_boolean_expression_field_annotation(field.info.generic)?;

            let field_expression = match field_annotation {
                // "_and" field
                ObjectBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::AndOp) => {
                    // The "_and" field value should be a list
                    let and_values = field.value.as_list()?;

                    let and_expressions = and_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_object_boolean_expression(
                                value_object,
                                data_connector_link,
                                type_mappings,
                                column_path,
                                session_variables,
                                usage_counts,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Expression::mk_and(and_expressions)
                }
                // "_or" field
                ObjectBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::OrOp) => {
                    // The "_or" field value should be a list
                    let or_values = field.value.as_list()?;

                    let or_expressions = or_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_object_boolean_expression(
                                value_object,
                                data_connector_link,
                                type_mappings,
                                column_path,
                                session_variables,
                                usage_counts,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Expression::mk_or(or_expressions)
                }
                // "_not" field
                ObjectBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::NotOp) => {
                    // The "_not" field value should be an object
                    let not_value = field.value.as_object()?;

                    let not_filter_expression = resolve_object_boolean_expression(
                        not_value,
                        data_connector_link,
                        type_mappings,
                        column_path,
                        session_variables,
                        usage_counts,
                    )?;
                    Expression::mk_not(not_filter_expression)
                }
                // comparableField field
                ObjectBooleanExpressionField::Field {
                    field_name,
                    object_type,
                    object_field_kind,
                    deprecated: _,
                } => {
                    let FieldMapping { column, .. } = plan::get_field_mapping_of_field_name(
                        type_mappings,
                        object_type,
                        field_name,
                    )?;

                    let field_value = field.value.as_object()?;

                    match object_field_kind {
                        ObjectFieldKind::Object => {
                            // Append the current column to the column_path before descending into the nested object expression
                            let field_path = column_path
                                .iter()
                                .copied()
                                .chain([column])
                                .collect::<Vec<_>>();
                            resolve_object_boolean_expression(
                                field_value,
                                data_connector_link,
                                type_mappings,
                                &field_path,
                                session_variables,
                                usage_counts,
                            )?
                        }
                        ObjectFieldKind::ObjectArray => {
                            let inner_expression = resolve_object_boolean_expression(
                                field_value,
                                data_connector_link,
                                type_mappings,
                                &[], // Reset the column path because we're nesting the expression inside an exists that itself captures the field path
                                session_variables,
                                usage_counts,
                            )?;

                            Expression::LocalNestedArray {
                                column: column.clone(),
                                field_path: column_path.iter().copied().cloned().collect(),
                                predicate: Box::new(inner_expression),
                            }
                        }
                        ObjectFieldKind::Scalar => resolve_scalar_boolean_expression(
                            field_value,
                            data_connector_link,
                            column_path,
                            column,
                        )?,
                    }
                }
                ObjectBooleanExpressionField::RelationshipField(FilterRelationshipAnnotation {
                    relationship_name,
                    relationship_type,
                    source_type,
                    target_source,
                    target_type,
                    target_model_name,
                    mappings,
                    deprecated: _,
                }) => {
                    // Add the target model being used in the usage counts
                    count_model(target_model_name, usage_counts);

                    // Get the filter permissions for the target model
                    let filter_permission = permissions::get_select_filter_predicate(&field.info)?;
                    let filter_predicate = permissions::build_model_permissions_filter_predicate(
                        &target_source.model.data_connector,
                        &target_source.model.type_mappings,
                        filter_permission,
                        session_variables,
                        usage_counts,
                    )?;

                    // This map contains the relationships or the columns of the
                    // relationship that needs to be used for ordering.
                    let filter_object = field.value.as_object()?;

                    // The predicate being applied across the relationship
                    let relationship_predicate = resolve_object_boolean_expression(
                        filter_object,
                        &target_source.model.data_connector,
                        &target_source.model.type_mappings,
                        &[], // We're traversing across the relationship, so we reset the field path
                        session_variables,
                        usage_counts,
                    )?;

                    // Combine the filter predicate and the relationship predicate
                    let predicate = match filter_predicate {
                        Some(filter_predicate) => {
                            Expression::mk_and(vec![filter_predicate, relationship_predicate])
                        }
                        None => relationship_predicate,
                    };

                    // build and return relationshp comparison expression
                    plan::build_relationship_comparison_expression(
                        type_mappings,
                        column_path,
                        data_connector_link,
                        relationship_name,
                        relationship_type,
                        source_type,
                        target_model_name,
                        target_source,
                        target_type,
                        mappings,
                        predicate,
                    )?
                }
            };

            Ok(field_expression)
        })
        .collect::<Result<Vec<Expression>, error::Error>>()?;

    Ok(Expression::mk_and(field_expressions))
}

fn extract_object_boolean_expression_field_annotation(
    annotation: &graphql_schema::Annotation,
) -> Result<&ObjectBooleanExpressionField, error::Error> {
    match annotation {
        graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                object_boolean_expression_field,
            ),
        )) => Ok(object_boolean_expression_field),
        _ => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }
        .into()),
    }
}

fn resolve_scalar_boolean_expression<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    column_path: &[&'s DataConnectorColumnName],
    column: &DataConnectorColumnName,
) -> Result<Expression<'s>, error::Error> {
    let field_expressions = fields
        .values()
        .map(|field| {
            let field_annotation =
                extract_scalar_boolean_expression_field_annotation(field.info.generic)?;

            let field_expression = match field_annotation {
                // "_and" field
                ScalarBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::AndOp) => {
                    // The "_and" field value should be a list
                    let and_values = field.value.as_list()?;

                    let and_expressions = and_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_scalar_boolean_expression(
                                value_object,
                                data_connector_link,
                                column_path,
                                column,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Expression::mk_and(and_expressions)
                }
                // "_or" field
                ScalarBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::OrOp) => {
                    // The "_or" field value should be a list
                    let or_values = field.value.as_list()?;

                    let or_expressions = or_values
                        .iter()
                        .map(|value| {
                            let value_object = value.as_object()?;
                            resolve_scalar_boolean_expression(
                                value_object,
                                data_connector_link,
                                column_path,
                                column,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Expression::mk_or(or_expressions)
                }
                // "_not" field
                ScalarBooleanExpressionField::LogicalOperatorField(LogicalOperatorField::NotOp) => {
                    // The "_not" field value should be an object
                    let not_value = field.value.as_object()?;

                    let not_filter_expression = resolve_scalar_boolean_expression(
                        not_value,
                        data_connector_link,
                        column_path,
                        column,
                    )?;
                    Expression::mk_not(not_filter_expression)
                }
                ScalarBooleanExpressionField::IsNullOperation => {
                    build_is_null_expression(column_path, column, &field.value)?
                }
                ScalarBooleanExpressionField::ComparisonOperation { operator_mapping } => {
                    let operator =
                        operator_mapping
                            .get(&data_connector_link.name)
                            .ok_or_else(|| {
                                error::InternalEngineError::OperatorMappingError(
                                    error::OperatorMappingError::MissingEntryForDataConnector {
                                        column_name: column.clone(),
                                        data_connector_name: data_connector_link.name.clone(),
                                    },
                                )
                            })?;

                    build_binary_comparison_expression(operator, column_path, column, &field.value)
                }
            };

            Ok(field_expression)
        })
        .collect::<Result<Vec<Expression>, error::Error>>()?;

    Ok(Expression::mk_and(field_expressions))
}

fn extract_scalar_boolean_expression_field_annotation(
    annotation: &graphql_schema::Annotation,
) -> Result<&ScalarBooleanExpressionField, error::Error> {
    match annotation {
        graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::ScalarBooleanExpressionField(
                scalar_boolean_expression_field,
            ),
        )) => Ok(scalar_boolean_expression_field),
        _ => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }
        .into()),
    }
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression<'s>(
    column_path: &[&DataConnectorColumnName],
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'s, GDS>,
) -> Result<Expression<'s>, error::Error> {
    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression =
        Expression::LocalField(LocalFieldComparison::UnaryComparison {
            column: ComparisonTarget::Column {
                // The column name is the root column
                name: column_path.first().map_or(column, Deref::deref).clone(),
                // The field path is the nesting path inside the root column, if any
                field_path: column_path
                    .iter()
                    .copied()
                    .chain([column])
                    .skip(1)
                    .cloned()
                    .collect(),
            },
            operator: metadata_resolve::UnaryComparisonOperator::IsNull,
        });
    // Get `_is_null` input value as boolean
    let is_null = value.as_boolean()?;
    if is_null {
        // When _is_null: true. Just return 'IsNull' unary comparison expression.
        Ok(unary_comparison_expression)
    } else {
        // When _is_null: false. Return negated 'IsNull' unary comparison expression by wrapping it in 'Not'.
        Ok(Expression::mk_not(unary_comparison_expression))
    }
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression<'s>(
    operator: &DataConnectorOperatorName,
    column_path: &[&DataConnectorColumnName],
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'s, GDS>,
) -> Expression<'s> {
    Expression::LocalField(LocalFieldComparison::BinaryComparison {
        column: ComparisonTarget::Column {
            // The column name is the root column
            name: column_path.first().map_or(column, Deref::deref).clone(),
            // The field path is the nesting path inside the root column, if any
            field_path: column_path
                .iter()
                .copied()
                .chain([column])
                .skip(1)
                .cloned()
                .collect(),
        },
        operator: operator.clone(),
        value: ComparisonValue::Scalar {
            value: value.as_json(),
        },
    })
}
