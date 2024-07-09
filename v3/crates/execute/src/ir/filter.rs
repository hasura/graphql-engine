use std::collections::BTreeMap;

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve::{DataConnectorLink, FieldMapping, Qualified, UnaryComparisonOperator};
use serde::Serialize;
use serde_json;

use crate::ir::error;
use crate::model_tracking::{count_model, UsagesCounts};
use crate::remote_joins::types::VariableName;
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::{CustomTypeName, FieldName},
};
use schema::FilterRelationshipAnnotation;
use schema::GDS;
use schema::{self};
use schema::{BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation};

use super::relationship::LocalModelRelationshipInfo;
use crate::ir::selection_set::NDCRelationshipName;

#[derive(Debug, Serialize)]
pub(crate) struct ResolvedFilterExpression<'s> {
    pub expression: Option<FilterExpression>,
    // relationships that were used in the filter expression. This is helpful
    // for collecting relatinships and sending collection_relationships
    pub relationships: BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
}

#[derive(Debug, Serialize)]
pub enum FilterExpression {
    And {
        expressions: Vec<FilterExpression>,
    },
    Or {
        expressions: Vec<FilterExpression>,
    },
    Not {
        expression: Box<FilterExpression>,
    },
    UnaryComparisonOperator {
        target: ComparisonTarget,
        operator: UnaryComparisonOperator,
    },
    BinaryComparisonOperator {
        target: ComparisonTarget,
        operator: DataConnectorOperatorName,
        value: ComparisonValue,
    },
    Exists {
        in_collection: ExistsInCollection,
        predicate: Box<FilterExpression>,
    },
}

impl FilterExpression {
    pub fn remove_always_true_expression(self) -> Option<FilterExpression> {
        match &self {
            FilterExpression::And { expressions } if expressions.is_empty() => None,
            FilterExpression::Not { expression } => match expression.as_ref() {
                FilterExpression::Or { expressions } if expressions.is_empty() => None,
                _ => Some(self),
            },
            _ => Some(self),
        }
    }

    /// Creates a 'FilterExpression::And' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_and(expressions: Vec<FilterExpression>) -> FilterExpression {
        // If the `and` only contains one expression, we can unwrap it and get rid of the `and`
        // ie. and([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `and`, we can flatten into a single `and`
        // ie. and([and([x,y]), and([a,b])]) == and([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, FilterExpression::And { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    FilterExpression::And { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            FilterExpression::And {
                expressions: subexprs,
            }
        } else {
            FilterExpression::And { expressions }
        }
    }

    /// Creates a 'FilterExpression::Or' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_or(expressions: Vec<FilterExpression>) -> FilterExpression {
        // If the `or` only contains one expression, we can unwrap it and get rid of the `or`
        // ie. or([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `or`, we can flatten into a single `or`
        // ie. or([or([x,y]), or([a,b])]) == or([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, FilterExpression::Or { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    FilterExpression::Or { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            FilterExpression::Or {
                expressions: subexprs,
            }
        } else {
            FilterExpression::Or { expressions }
        }
    }

    /// Creates a 'FilterExpression::Not' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_not(expression: FilterExpression) -> FilterExpression {
        match expression {
            // Double negations can be removed
            // ie. not(not(x))) == x
            FilterExpression::Not { expression } => *expression,
            _ => FilterExpression::Not {
                expression: Box::new(expression),
            },
        }
    }
}

#[derive(Debug, Serialize)]
pub enum ComparisonTarget {
    Column {
        /// The name of the column
        name: DataConnectorColumnName,
        /// Path to a nested field within an object column
        field_path: Vec<DataConnectorColumnName>,
    },
}

#[derive(Debug, Serialize)]
pub enum ComparisonValue {
    Scalar { value: serde_json::Value },
    Variable { name: VariableName },
}

#[derive(Debug, Serialize)]
pub enum ExistsInCollection {
    Related { relationship: NDCRelationshipName },
}

/// Generates the IR for GraphQL 'where' boolean expression
pub(crate) fn resolve_filter_expression<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<ResolvedFilterExpression<'s>, error::Error> {
    let mut relationships = BTreeMap::new();
    let expressions = fields
        .values()
        .map(|field| {
            build_filter_expression(
                field,
                &mut relationships,
                data_connector_link,
                type_mappings,
                usage_counts,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    let expression = FilterExpression::mk_and(expressions);
    let resolved_filter_expression = ResolvedFilterExpression {
        expression: expression.remove_always_true_expression(),
        relationships,
    };
    Ok(resolved_filter_expression)
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

// Build the NDC filter expression by traversing the relationships when present
fn build_filter_expression<'s>(
    field: &normalized_ast::InputField<'s, GDS>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<FilterExpression, error::Error> {
    let boolean_expression_annotation = get_boolean_expression_annotation(field.info.generic)?;
    build_filter_expression_from_boolean_expression(
        boolean_expression_annotation,
        field,
        relationships,
        data_connector_link,
        type_mappings,
        &mut vec![],
        usage_counts,
    )
}

// build filter expression, specifically matching on BooleanExpressionAnnotation
fn build_filter_expression_from_boolean_expression<'s>(
    boolean_expression_annotation: &'s BooleanExpressionAnnotation,
    field: &normalized_ast::InputField<'s, GDS>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    field_path: &mut Vec<DataConnectorColumnName>,
    usage_counts: &mut UsagesCounts,
) -> Result<FilterExpression, error::Error> {
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
                        relationships,
                        data_connector_link,
                        type_mappings,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(FilterExpression::mk_and(and_expressions))
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
                        relationships,
                        data_connector_link,
                        type_mappings,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(FilterExpression::mk_or(or_expressions))
        }
        // "_not"
        BooleanExpressionAnnotation::BooleanExpressionArgument {
            field: schema::ModelFilterArgument::NotOp,
        } => {
            // The "_not" field value should be an object
            let not_value = field.value.as_object()?;

            let not_filter_expression = resolve_filter_object(
                not_value,
                relationships,
                data_connector_link,
                type_mappings,
                usage_counts,
            )?;
            Ok(FilterExpression::mk_not(not_filter_expression))
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

            // filter expression with relationships is currently only supported for local relationships
            // this is the first point at which we know the source data connector, so we must
            // ensure only a local relationship is used
            match metadata_resolve::relationship_execution_category(
                data_connector_link,
                &target_source.model.data_connector,
                &target_source.capabilities,
            ) {
                metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
                    Err(error::Error::Internal(error::InternalError::Engine(
                        error::InternalEngineError::InternalGeneric {
                            description:
                                "Remote relationships are not supported in boolean expressions"
                                    .to_string(),
                        },
                    )))
                }

                metadata_resolve::RelationshipExecutionCategory::Local => {
                    let ndc_relationship_name =
                        NDCRelationshipName::new(source_type, relationship_name)?;

                    relationships.insert(
                        ndc_relationship_name.clone(),
                        LocalModelRelationshipInfo {
                            relationship_name,
                            relationship_type,
                            source_type,
                            source_data_connector: data_connector_link,
                            source_type_mappings: type_mappings,
                            target_source,
                            target_type,
                            mappings,
                        },
                    );

                    // This map contains the relationships or the columns of the
                    // relationship that needs to be used for ordering.
                    let filter_object = field.value.as_object()?;

                    let expressions = filter_object
                        .values()
                        .map(|field| {
                            build_filter_expression(
                                field,
                                relationships,
                                &target_source.model.data_connector,
                                &target_source.model.type_mappings,
                                usage_counts,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    // Using exists clause to build the filter expression for relationship fields.
                    let exists_filter_clause = FilterExpression::mk_and(expressions);
                    let exists_in_relationship = ExistsInCollection::Related {
                        relationship: ndc_relationship_name,
                    };

                    Ok(FilterExpression::Exists {
                        in_collection: exists_in_relationship,
                        predicate: Box::new(exists_filter_clause),
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
) -> Result<FilterExpression, error::Error> {
    let mut expressions = Vec::new();

    for (_op_name, op_value) in field.value.as_object()? {
        match op_value.info.generic {
            schema::Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::IsNullOperation,
            )) => {
                let expression = build_is_null_expression(column, &op_value.value, field_path)?;
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
    Ok(FilterExpression::mk_and(expressions))
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

/// Generate a filter expression from an input object fields
fn resolve_filter_object<'s>(
    fields: &IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    data_connector_link: &'s DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    usage_counts: &mut UsagesCounts,
) -> Result<FilterExpression, error::Error> {
    let mut expressions = Vec::new();

    for field in fields.values() {
        expressions.push(build_filter_expression(
            field,
            relationships,
            data_connector_link,
            type_mappings,
            usage_counts,
        )?);
    }
    Ok(FilterExpression::mk_and(expressions))
}

/// Generate a binary comparison operator
fn build_binary_comparison_expression(
    operator: &DataConnectorOperatorName,
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'_, GDS>,
    field_path: &[DataConnectorColumnName],
) -> FilterExpression {
    FilterExpression::BinaryComparisonOperator {
        target: ComparisonTarget::Column {
            name: column.clone(),
            field_path: field_path.to_vec(),
        },
        operator: operator.clone(),
        value: ComparisonValue::Scalar {
            value: value.as_json(),
        },
    }
}

/// Resolve `_is_null` GraphQL boolean operator
fn build_is_null_expression(
    column: &DataConnectorColumnName,
    value: &normalized_ast::Value<'_, GDS>,
    field_path: &[DataConnectorColumnName],
) -> Result<FilterExpression, error::Error> {
    // Build an 'IsNull' unary comparison expression
    let unary_comparison_expression = FilterExpression::UnaryComparisonOperator {
        target: ComparisonTarget::Column {
            name: column.clone(),
            field_path: field_path.to_vec(),
        },
        operator: UnaryComparisonOperator::IsNull,
    };
    // Get `_is_null` input value as boolean
    let is_null = value.as_boolean()?;
    if is_null {
        // When _is_null: true. Just return 'IsNull' unary comparison expression.
        Ok(unary_comparison_expression)
    } else {
        // When _is_null: false. Return negated 'IsNull' unary comparison expression by wrapping it in 'Not'.
        Ok(FilterExpression::mk_not(unary_comparison_expression))
    }
}
