use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve::{DataConnectorLink, FieldMapping, Qualified};
use open_dds::models::ModelName;
use open_dds::relationships::{RelationshipName, RelationshipType};
use serde::Serialize;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::ops::Deref;

use crate::model_tracking::count_model;
use crate::{error, permissions};
use graphql_schema::{self};
use graphql_schema::{BooleanExpressionAnnotation, InputAnnotation, ObjectFieldKind};
use graphql_schema::{
    FilterRelationshipAnnotation, ObjectBooleanExpressionField, ScalarBooleanExpressionField,
};
use graphql_schema::{LogicalOperatorField, GDS};
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::{CustomTypeName, FieldName},
};
use plan_types::{
    ComparisonTarget, ComparisonValue, Expression, LocalFieldComparison,
    LocalModelRelationshipInfo, NdcRelationshipName, RelationshipColumnMapping, SourceNdcColumn,
    UsagesCounts,
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
                    let FieldMapping { column, .. } =
                        get_field_mapping_of_field_name(type_mappings, object_type, field_name)?;

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
                    build_relationship_comparison_expression(
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

/// Defines strategies for executing relationship predicates.
enum RelationshipPredicateExecutionStrategy {
    /// Pushes predicate resolution to the NDC (Data Connector).
    /// This is feasible only if the data connector supports the 'relation_comparisons' capability
    /// and is used when both source and target connectors are the same (local relationship).
    NDCPushdown,

    /// Resolves predicates within the Engine itself.
    /// This approach is used when dealing with remote relationships or if the data connector lacks
    /// the 'relation_comparisons' capability. The Engine queries field values from the target model
    /// and constructs the necessary comparison expressions.
    InEngine,
}

/// Determines the strategy for executing relationship predicates based on the connectors and their capabilities.
fn get_relationship_predicate_execution_strategy(
    source_connector: &DataConnectorLink,
    target_connector: &DataConnectorLink,
    target_source_relationship_capabilities: &metadata_resolve::RelationshipCapabilities,
) -> RelationshipPredicateExecutionStrategy {
    // It's a local relationship if the source and target connectors are the same and
    // the connector supports relationships.
    if target_connector.name == source_connector.name
        && target_source_relationship_capabilities
            .supports_relationships
            .as_ref()
            .is_some_and(|r| r.supports_relation_comparisons)
    {
        RelationshipPredicateExecutionStrategy::NDCPushdown
    } else {
        RelationshipPredicateExecutionStrategy::InEngine
    }
}

/// Build a relationship comparison expression from a relationship predicate.
/// This is used for for both query and permission filter predicates.
/// Corresponding inner predicates need to be resolved prior to this function call
/// and passed as `relationship_predicate`.
pub(crate) fn build_relationship_comparison_expression<'s>(
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    column_path: &[&'s DataConnectorColumnName],
    data_connector_link: &'s DataConnectorLink,
    relationship_name: &'s RelationshipName,
    relationship_type: &'s RelationshipType,
    source_type: &'s Qualified<CustomTypeName>,
    target_model_name: &'s Qualified<ModelName>,
    target_source: &'s metadata_resolve::ModelTargetSource,
    target_type: &'s Qualified<CustomTypeName>,
    mappings: &'s Vec<metadata_resolve::RelationshipModelMapping>,
    relationship_predicate: Expression<'s>,
) -> Result<Expression<'s>, error::Error> {
    // Determine whether the relationship is local or remote
    match get_relationship_predicate_execution_strategy(
        data_connector_link,
        &target_source.model.data_connector,
        &target_source.capabilities,
    ) {
        RelationshipPredicateExecutionStrategy::NDCPushdown => {
            let ndc_relationship_name = NdcRelationshipName::new(source_type, relationship_name);

            let local_model_relationship_info = LocalModelRelationshipInfo {
                relationship_name,
                relationship_type,
                source_type,
                source_data_connector: data_connector_link,
                source_type_mappings: type_mappings,
                target_source,
                target_type,
                mappings,
            };

            Ok(Expression::RelationshipLocalComparison {
                field_path: column_path.iter().copied().cloned().collect(),
                relationship: ndc_relationship_name,
                predicate: Box::new(relationship_predicate),
                info: local_model_relationship_info,
            })
        }

        RelationshipPredicateExecutionStrategy::InEngine => {
            // Build a NDC column mapping out of the relationship mappings.
            // This mapping is later used to build the local field comparison expressions
            // using values fetched from remote source
            let mut ndc_column_mapping = Vec::new();
            for relationship_mapping in mappings {
                let source_field = &relationship_mapping.source_field.field_name;
                let FieldMapping {
                    column: source_column,
                    comparison_operators,
                    ..
                } = get_field_mapping_of_field_name(type_mappings, source_type, source_field)?;

                let equal_operators = comparison_operators
                    .as_ref()
                    .map(|ops| Cow::Borrowed(&ops.equality_operators))
                    .unwrap_or_default();

                let eq_operator = equal_operators.first().ok_or_else(|| {
                    error::InternalEngineError::InternalGeneric {
                        description: format!(
                            "Cannot use relationship '{relationship_name}' \
                             in filter predicate. NDC column {source_column} (used by \
                             source field '{source_field}') needs to implement an EQUAL comparison operator"
                        ),
                    }
                })?;

                let source_ndc_column = SourceNdcColumn {
                    column: source_column.clone(),
                    field_path: column_path.iter().copied().cloned().collect(),
                    eq_operator: eq_operator.clone(),
                };

                let target_ndc_column = &relationship_mapping
                    .target_ndc_column
                    .as_ref()
                    .ok_or_else(|| error::InternalEngineError::InternalGeneric {
                        description: "Target NDC column not found".to_string(),
                    })?
                    .column;

                ndc_column_mapping.push(RelationshipColumnMapping {
                    source_ndc_column,
                    target_ndc_column: target_ndc_column.clone(),
                });
            }

            Ok(Expression::RelationshipRemoteComparison {
                relationship: relationship_name.clone(),
                target_model_name,
                target_model_source: target_source.model.clone(),
                ndc_column_mapping,
                predicate: Box::new(relationship_predicate),
            })
        }
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

/// get column name for field name
fn get_field_mapping_of_field_name<'a>(
    type_mappings: &'a BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    field_name: &FieldName,
) -> Result<&'a metadata_resolve::FieldMapping, error::Error> {
    let type_mapping = type_mappings.get(type_name).ok_or_else(|| {
        error::InternalDeveloperError::TypeMappingNotFound {
            type_name: type_name.clone(),
        }
    })?;
    match type_mapping {
        metadata_resolve::TypeMapping::Object { field_mappings, .. } => Ok(field_mappings
            .get(field_name)
            .ok_or_else(|| error::InternalDeveloperError::FieldMappingNotFound {
                type_name: type_name.clone(),
                field_name: field_name.clone(),
            })?),
    }
}
