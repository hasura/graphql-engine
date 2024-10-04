use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve::{DataConnectorLink, FieldMapping, Qualified};
use open_dds::models::ModelName;
use open_dds::relationships::{RelationshipName, RelationshipType};
use serde::Serialize;
use std::collections::BTreeMap;

use crate::error;
use crate::model_tracking::count_model;
use graphql_schema::FilterRelationshipAnnotation;
use graphql_schema::GDS;
use graphql_schema::{self};
use graphql_schema::{
    BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation, ObjectFieldKind,
};
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
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, error::Error> {
    let mut expressions = Vec::new();
    for field in fields.values() {
        let field_filter_expression =
            build_filter_expression(field, data_connector_link, type_mappings, usage_counts)?;
        expressions.push(field_filter_expression);
    }
    Ok(Expression::mk_and(expressions))
}

fn build_filter_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, error::Error> {
    let boolean_expression_annotation = get_boolean_expression_annotation(field.info.generic)?;
    build_filter_expression_from_boolean_expression(
        boolean_expression_annotation,
        field,
        data_connector_link,
        type_mappings,
        &mut vec![],
        usage_counts,
    )
}

fn build_filter_expression_from_boolean_expression<'s>(
    boolean_expression_annotation: &'s BooleanExpressionAnnotation,
    field: &normalized_ast::InputField<'s, GDS>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    field_path: &mut Vec<DataConnectorColumnName>,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, error::Error> {
    match boolean_expression_annotation {
        // "_and"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: graphql_schema::ModelFilterArgument::AndOp,
        } => {
            // The "_and" field value should be a list
            let and_values = field.value.as_list()?;

            let and_expressions = and_values
                .iter()
                .map(|value| {
                    let value_object = value.as_object()?;
                    resolve_filter_object(
                        value_object,
                        data_connector_link,
                        type_mappings,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Expression::mk_and(and_expressions))
        }
        // "_or"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: graphql_schema::ModelFilterArgument::OrOp,
        } => {
            // The "_or" field value should be a list
            let or_values = field.value.as_list()?;

            let or_expressions = or_values
                .iter()
                .map(|value| {
                    let value_object = value.as_object()?;
                    resolve_filter_object(
                        value_object,
                        data_connector_link,
                        type_mappings,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Expression::mk_or(or_expressions))
        }
        // "_not"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: graphql_schema::ModelFilterArgument::NotOp,
        } => {
            // The "_not" field value should be an object
            let not_value = field.value.as_object()?;

            let not_filter_expression =
                resolve_filter_object(not_value, data_connector_link, type_mappings, usage_counts)?;
            Ok(Expression::mk_not(not_filter_expression))
        }
        // The column that we want to use for filtering.
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field:
                graphql_schema::ModelFilterArgument::Field {
                    field_name,
                    object_type,
                    object_field_kind,
                    ..
                },
        } => {
            let FieldMapping { column, .. } =
                get_field_mapping_of_field_name(type_mappings, object_type, field_name)?;

            let inner_is_array_field = matches!(object_field_kind, ObjectFieldKind::Array);

            build_comparison_expression(
                field,
                field_path,
                inner_is_array_field,
                &column,
                data_connector_link,
                type_mappings,
            )
        }
        // Relationship field used for filtering.
        // This relationship can either point to another relationship or a column.
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field:
                graphql_schema::ModelFilterArgument::RelationshipField(FilterRelationshipAnnotation {
                    relationship_name,
                    relationship_type,
                    source_type,
                    target_source,
                    target_type,
                    target_model_name,
                    mappings,
                    deprecated: _,
                }),
        } => {
            // Add the target model being used in the usage counts
            count_model(target_model_name, usage_counts);

            // This map contains the relationships or the columns of the
            // relationship that needs to be used for ordering.
            let filter_object = field.value.as_object()?;

            let mut expressions = Vec::new();

            for field in filter_object.values() {
                let field_filter_expression = build_filter_expression(
                    field,
                    &target_source.model.data_connector,
                    &target_source.model.type_mappings,
                    usage_counts,
                )?;
                expressions.push(field_filter_expression);
            }

            let relationship_predicate = Expression::mk_and(expressions);

            // build and return relationshp comparison expression
            build_relationship_comparison_expression(
                type_mappings,
                field_path,
                data_connector_link,
                relationship_name,
                relationship_type,
                source_type,
                target_model_name,
                target_source,
                target_type,
                mappings,
                relationship_predicate,
            )
        }
        other_boolean_annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
                other_boolean_annotation.clone(),
            )),
        })?,
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
        && target_source_relationship_capabilities.relationship_comparison
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
    field_path: &[DataConnectorColumnName],
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
    // Determinde whether the relationship is local or remote
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

            Ok(Expression::RelationshipNdcPushdown {
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
                    .map(|ops| ops.equality_operators)
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
                    column: source_column,
                    field_path: field_path.to_owned(),
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

            Ok(Expression::RelationshipEngineResolved {
                relationship: relationship_name.clone(),
                target_model_name,
                target_model_source: target_source.model.clone(),
                ndc_column_mapping,
                predicate: Box::new(relationship_predicate),
            })
        }
    }
}

fn build_comparison_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    field_path: &mut Vec<DataConnectorColumnName>,
    is_array_field: bool,
    column: &DataConnectorColumnName,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
) -> Result<Expression<'s>, error::Error> {
    let mut expressions = Vec::new();

    for (_op_name, op_value) in field.value.as_object()? {
        match op_value.info.generic {
            graphql_schema::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::IsNullOperation,
            )) => {
                let expression =
                    build_is_null_expression(column, &op_value.value, field_path.clone())?;
                expressions.push(expression);
            }
            graphql_schema::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ComparisonOperation { operator_mapping },
            )) => {
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

                let expression = build_binary_comparison_expression(
                    operator,
                    column,
                    &op_value.value,
                    field_path,
                );
                expressions.push(expression);
            }
            // Nested field comparison
            graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
                BooleanExpressionAnnotation::BooleanExpressionArgument {
                    field:
                        graphql_schema::ModelFilterArgument::Field {
                            field_name: inner_field_name,
                            object_field_kind,
                            object_type: inner_object_type,
                            ..
                        },
                },
            )) => {
                // get correct inner column name
                let FieldMapping {
                    column: inner_column,
                    ..
                } = get_field_mapping_of_field_name(
                    type_mappings,
                    inner_object_type,
                    inner_field_name,
                )?;

                let inner_is_array_field = matches!(object_field_kind, ObjectFieldKind::Array);

                if is_array_field {
                    // if we're matching in an array field, we look for the inner column directly
                    let inner_expression = build_comparison_expression(
                        op_value,
                        field_path,
                        inner_is_array_field,
                        &inner_column,
                        data_connector_link,
                        type_mappings,
                    )?;

                    // and then wrap it in an `exists`
                    let exists_wrapper = Expression::LocalNestedArray {
                        column: column.clone(),
                        field_path: field_path.clone(),
                        predicate: Box::new(inner_expression),
                    };

                    expressions.push(exists_wrapper);
                } else {
                    // otherwise we add to the field path
                    field_path.push(inner_column.clone());

                    // and look for the main column inside
                    let inner_expression = build_comparison_expression(
                        op_value,
                        field_path,
                        inner_is_array_field,
                        column,
                        data_connector_link,
                        type_mappings,
                    )?;

                    expressions.push(inner_expression);
                }
            }
            // Nested relationship comparison
            graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
                BooleanExpressionAnnotation::BooleanExpressionArgument {
                    field:
                        graphql_schema::ModelFilterArgument::RelationshipField(
                            FilterRelationshipAnnotation {
                                relationship_name, ..
                            },
                        ),
                },
            )) => Err(
                error::InternalDeveloperError::NestedObjectRelationshipInPredicate {
                    relationship_name: relationship_name.clone(),
                },
            )?,

            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(Expression::mk_and(expressions))
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression<'s>(
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'s, GDS>,
    field_path: Vec<DataConnectorColumnName>,
) -> Result<Expression<'s>, error::Error> {
    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression =
        Expression::LocalField(LocalFieldComparison::UnaryComparison {
            column: ComparisonTarget::Column {
                name: column.clone(),
                field_path,
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
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'s, GDS>,
    field_path: &[DataConnectorColumnName],
) -> Expression<'s> {
    Expression::LocalField(LocalFieldComparison::BinaryComparison {
        column: ComparisonTarget::Column {
            name: column.clone(),
            field_path: field_path.to_vec(),
        },
        operator: operator.clone(),
        value: ComparisonValue::Scalar {
            value: value.as_json(),
        },
    })
}

fn resolve_filter_object<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, error::Error> {
    let mut expressions = Vec::new();

    for field in fields.values() {
        expressions.push(build_filter_expression(
            field,
            data_connector_link,
            type_mappings,
            usage_counts,
        )?);
    }
    Ok(Expression::mk_and(expressions))
}

fn get_boolean_expression_annotation(
    annotation: &graphql_schema::Annotation,
) -> Result<&BooleanExpressionAnnotation, error::Error> {
    match annotation {
        graphql_schema::Annotation::Input(InputAnnotation::BooleanExpression(
            boolean_expression_annotation,
        )) => Ok(boolean_expression_annotation),
        _ => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }
        .into()),
    }
}

/// get column name for field name
fn get_field_mapping_of_field_name(
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    field_name: &FieldName,
) -> Result<metadata_resolve::FieldMapping, error::Error> {
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
            })?
            .clone()),
    }
}
