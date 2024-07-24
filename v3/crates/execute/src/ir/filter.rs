use std::collections::BTreeMap;

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve::{DataConnectorLink, FieldMapping, Qualified};
use serde::Serialize;

use crate::ir::error;
use crate::model_tracking::{count_model, UsagesCounts};
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::{CustomTypeName, FieldName},
};
use schema::FilterRelationshipAnnotation;
use schema::GDS;
use schema::{self};
use schema::{BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation};

use super::relationship::LocalModelRelationshipInfo;
use crate::ir::selection_set::NdcRelationshipName;

pub mod expression;

/// Filter expression to be applied on a model/command selection set
#[derive(Debug, Serialize, Clone)]
pub(crate) struct FilterExpression<'s> {
    /// Filter obtained from the GraphQL query request
    pub(crate) query_filter: QueryFilter<'s>,
    /// Permission filters
    pub(crate) permission_filter: Option<expression::Expression<'s>>,
    /// Filter for representing local relationship joins
    pub(crate) relationship_join_filter: Option<expression::Expression<'s>>,
}

/// Filter expression derived from GraphQL query.
#[derive(Debug, Serialize, Clone)]
pub(crate) struct QueryFilter<'s> {
    /// Filter derived from `where` clause.
    pub(crate) where_clause: Option<expression::Expression<'s>>,
    /// Relay global ID or unique field comparisons
    pub(crate) additional_filter: Option<expression::Expression<'s>>,
}

/// Generate the IR for GraphQL 'where' boolean expression
pub(crate) fn resolve_filter_expression<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<expression::Expression<'s>, error::Error> {
    let mut expressions = Vec::new();
    for field in fields.values() {
        let field_filter_expression =
            build_filter_expression(field, data_connector_link, type_mappings, usage_counts)?;
        expressions.push(field_filter_expression);
    }
    Ok(expression::Expression::mk_and(expressions))
}

fn build_filter_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<expression::Expression<'s>, error::Error> {
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
) -> Result<expression::Expression<'s>, error::Error> {
    match boolean_expression_annotation {
        // "_and"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: schema::ModelFilterArgument::AndOp,
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

            Ok(expression::Expression::mk_and(and_expressions))
        }
        // "_or"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: schema::ModelFilterArgument::OrOp,
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

            Ok(expression::Expression::mk_or(or_expressions))
        }
        // "_not"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: schema::ModelFilterArgument::NotOp,
        } => {
            // The "_not" field value should be an object
            let not_value = field.value.as_object()?;

            let not_filter_expression =
                resolve_filter_object(not_value, data_connector_link, type_mappings, usage_counts)?;
            Ok(expression::Expression::mk_not(not_filter_expression))
        }
        // The column that we want to use for filtering.
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field:
                schema::ModelFilterArgument::Field {
                    field_name,
                    object_type,
                },
        } => {
            let FieldMapping { column, .. } =
                get_field_mapping_of_field_name(type_mappings, object_type, field_name)?;

            build_comparison_expression(
                field,
                field_path,
                &column,
                data_connector_link,
                type_mappings,
            )
        }
        // Relationship field used for filtering.
        // This relationship can either point to another relationship or a column.
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field:
                schema::ModelFilterArgument::RelationshipField(FilterRelationshipAnnotation {
                    relationship_name,
                    relationship_type,
                    source_type,
                    target_source,
                    target_type,
                    target_model_name,
                    mappings,
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

            // Using exists clause to build the filter expression for relationship fields.
            let relationship_predicate = expression::Expression::mk_and(expressions);

            // filter expression with relationships is currently only supported for local relationships
            // this is the first point at which we know the source data connector, so we must
            // ensure only a local relationship is used
            match metadata_resolve::relationship_execution_category(
                data_connector_link,
                &target_source.model.data_connector,
                &target_source.capabilities,
            ) {
                metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
                    let mut ndc_column_mapping = Vec::new();
                    for relationship_mapping in mappings {
                        let source_field = &relationship_mapping.source_field.field_name;
                        let FieldMapping {
                            column: source_column,
                            equal_operators,
                            ..
                        } = get_field_mapping_of_field_name(
                            type_mappings,
                            source_type,
                            source_field,
                        )?;

                        let eq_operator = equal_operators.first().ok_or_else(|| {
                            error::InternalEngineError::InternalGeneric {
                                description: format!(
                                    "Cannot proceed with relationship '{relationship_name}' \
                                     in filter predicate: NDC column {source_column}, backing \
                                     source field '{source_field}', need to implement an EQUAL comparison operator"
                                ),
                            }
                        })?;

                        let source_ndc_column = expression::SourceNdcColumn {
                            column: source_column,
                            field_path: field_path.clone(),
                            eq_operator: eq_operator.clone(),
                        };

                        let target_ndc_column = &relationship_mapping
                            .target_ndc_column
                            .as_ref()
                            .ok_or_else(|| error::InternalEngineError::InternalGeneric {
                                description: "Target NDC column not found".to_string(),
                            })?
                            .column;

                        ndc_column_mapping.push(expression::RelationshipColumnMapping {
                            source_ndc_column,
                            target_ndc_column: target_ndc_column.clone(),
                        });
                    }

                    Ok(expression::Expression::RemoteRelationship {
                        relationship: relationship_name.clone(),
                        target_model_name,
                        target_model_source: &target_source.model,
                        ndc_column_mapping,
                        predicate: Box::new(relationship_predicate),
                    })
                }

                metadata_resolve::RelationshipExecutionCategory::Local => {
                    let ndc_relationship_name =
                        NdcRelationshipName::new(source_type, relationship_name)?;

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

                    Ok(expression::Expression::LocalRelationship {
                        relationship: ndc_relationship_name,
                        predicate: Box::new(relationship_predicate),
                        info: local_model_relationship_info,
                    })
                }
            }
        }
        other_boolean_annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: schema::Annotation::Input(InputAnnotation::BooleanExpression(
                other_boolean_annotation.clone(),
            )),
        })?,
    }
}

fn build_comparison_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    field_path: &mut Vec<DataConnectorColumnName>,
    column: &DataConnectorColumnName,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
) -> Result<expression::Expression<'s>, error::Error> {
    let mut expressions = Vec::new();

    for (_op_name, op_value) in field.value.as_object()? {
        match op_value.info.generic {
            schema::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::IsNullOperation,
            )) => {
                let expression =
                    build_is_null_expression(column, &op_value.value, field_path.clone())?;
                expressions.push(expression);
            }
            schema::Annotation::Input(InputAnnotation::Model(
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
            schema::Annotation::Input(InputAnnotation::BooleanExpression(
                BooleanExpressionAnnotation::BooleanExpressionArgument {
                    field:
                        schema::ModelFilterArgument::Field {
                            field_name: inner_field_name,
                            object_type: inner_object_type,
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

                // add it to the path
                field_path.push(inner_column);

                let inner_expression = build_comparison_expression(
                    op_value,
                    field_path,
                    column,
                    data_connector_link,
                    type_mappings,
                )?;

                expressions.push(inner_expression);
            }

            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(expression::Expression::mk_and(expressions))
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression<'s>(
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'s, GDS>,
    field_path: Vec<DataConnectorColumnName>,
) -> Result<expression::Expression<'s>, error::Error> {
    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression =
        expression::Expression::LocalField(expression::LocalFieldComparison::UnaryComparison {
            column: expression::ComparisonTarget::Column {
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
        Ok(expression::Expression::mk_not(unary_comparison_expression))
    }
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression<'s>(
    operator: &DataConnectorOperatorName,
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'s, GDS>,
    field_path: &[DataConnectorColumnName],
) -> expression::Expression<'s> {
    expression::Expression::LocalField(expression::LocalFieldComparison::BinaryComparison {
        column: expression::ComparisonTarget::Column {
            name: column.clone(),
            field_path: field_path.to_vec(),
        },
        operator: operator.clone(),
        value: expression::ComparisonValue::Scalar {
            value: value.as_json(),
        },
    })
}

fn resolve_filter_object<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<expression::Expression<'s>, error::Error> {
    let mut expressions = Vec::new();

    for field in fields.values() {
        expressions.push(build_filter_expression(
            field,
            data_connector_link,
            type_mappings,
            usage_counts,
        )?);
    }
    Ok(expression::Expression::mk_and(expressions))
}

fn get_boolean_expression_annotation(
    annotation: &schema::Annotation,
) -> Result<&BooleanExpressionAnnotation, error::Error> {
    match annotation {
        schema::Annotation::Input(InputAnnotation::BooleanExpression(
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
