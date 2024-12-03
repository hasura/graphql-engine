use crate::error::{FieldError, FieldInternalError, FilterPredicateError};
use indexmap::{IndexMap, IndexSet};
use plan_types::{
    Argument, Field, FieldsSelection, NestedArray, NestedField, NestedObject, OrderByElement,
    OrderByTarget, QueryExecutionPlan, QueryNodeNew, RemotePredicateKey, ResolvedFilterExpression,
};
use std::collections::BTreeMap;

// replace any placeholders in our predicates with
// `ResolvedFilterExpression`s we have calculated from
// our remote predicate execution
pub fn replace_predicates_in_query_execution_plan(
    query_execution_plan: QueryExecutionPlan,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<QueryExecutionPlan, FilterPredicateError> {
    Ok(QueryExecutionPlan {
        query_node: replace_predicates_in_query_node(query_execution_plan.query_node, predicates)?,
        arguments: query_execution_plan
            .arguments
            .into_iter()
            .map(|(argument_name, argument)| {
                Ok((
                    argument_name,
                    replace_predicates_in_argument(argument, predicates)?,
                ))
            })
            .collect::<Result<BTreeMap<_, _>, FilterPredicateError>>()?,
        collection: query_execution_plan.collection,
        collection_relationships: query_execution_plan.collection_relationships,
        data_connector: query_execution_plan.data_connector,
        variables: query_execution_plan.variables,
    })
}

fn replace_predicates_in_argument(
    argument: Argument,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<Argument, FilterPredicateError> {
    Ok(match argument {
        Argument::BooleanExpression { predicate } => Argument::BooleanExpression {
            predicate: replace_predicates_in_filter_expression(predicate, predicates)?,
        },
        Argument::Literal { value } => Argument::Literal { value },
        Argument::Variable { name } => Argument::Variable { name },
    })
}

fn replace_predicates_in_query_node(
    query_node: QueryNodeNew,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<QueryNodeNew, FilterPredicateError> {
    Ok(QueryNodeNew {
        aggregates: query_node.aggregates,
        fields: query_node
            .fields
            .map(|fields_selection| {
                Ok(FieldsSelection {
                    fields: fields_selection
                        .fields
                        .into_iter()
                        .map(|(ndc_alias, field)| {
                            Ok((ndc_alias, replace_predicates_in_field(field, predicates)?))
                        })
                        .collect::<Result<IndexMap<_, _>, FilterPredicateError>>()?,
                })
            })
            .transpose()?,
        limit: query_node.limit,
        offset: query_node.offset,
        order_by: query_node
            .order_by
            .map(|order_by| {
                order_by
                    .into_iter()
                    .map(|order_by_element| {
                        Ok(OrderByElement {
                            order_direction: order_by_element.order_direction,
                            target: replace_predicates_in_order_by_target(
                                order_by_element.target,
                                predicates,
                            )?,
                        })
                    })
                    .collect::<Result<Vec<_>, FilterPredicateError>>()
            })
            .transpose()?,
        predicate: query_node
            .predicate
            .map(|predicate| replace_predicates_in_filter_expression(predicate, predicates))
            .transpose()?,
    })
}

fn replace_predicates_in_order_by_target(
    order_by_target: OrderByTarget<ResolvedFilterExpression>,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<OrderByTarget<ResolvedFilterExpression>, FilterPredicateError> {
    match order_by_target {
        OrderByTarget::Column {
            relationship_path,
            name,
            field_path,
        } => Ok(OrderByTarget::Column {
            relationship_path: relationship_path
                .into_iter()
                .map(|relationship_path_element| {
                    Ok(plan_types::RelationshipPathElement {
                        field_path: relationship_path_element.field_path,
                        relationship_name: relationship_path_element.relationship_name,
                        filter_predicate: relationship_path_element
                            .filter_predicate
                            .map(|pred| replace_predicates_in_filter_expression(pred, predicates))
                            .transpose()?,
                    })
                })
                .collect::<Result<Vec<_>, FilterPredicateError>>()?,
            name,
            field_path,
        }),
    }
}

fn replace_predicates_in_nested_field(
    nested_field: NestedField,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<NestedField, FilterPredicateError> {
    Ok(match nested_field {
        NestedField::Object(nested_object) => NestedField::Object(
            replace_predicates_in_nested_object(nested_object, predicates)?,
        ),
        NestedField::Array(nested_array) => NestedField::Array(replace_predicates_in_nested_array(
            nested_array,
            predicates,
        )?),
    })
}
fn replace_predicates_in_nested_array(
    nested_array: NestedArray,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<NestedArray, FilterPredicateError> {
    let NestedArray { fields } = nested_array;
    Ok(NestedArray {
        fields: Box::new(replace_predicates_in_nested_field(*fields, predicates)?),
    })
}

fn replace_predicates_in_nested_object(
    nested_object: NestedObject,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<NestedObject, FilterPredicateError> {
    let NestedObject { fields } = nested_object;
    Ok(NestedObject {
        fields: fields
            .into_iter()
            .map(|(ndc_field_alias, field)| {
                Ok((
                    ndc_field_alias,
                    replace_predicates_in_field(field, predicates)?,
                ))
            })
            .collect::<Result<IndexMap<_, _>, FilterPredicateError>>()?,
    })
}

fn replace_predicates_in_field(
    field: Field,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<Field, FilterPredicateError> {
    Ok(match field {
        Field::Relationship {
            query_node,
            relationship,
            arguments,
        } => Field::Relationship {
            relationship,
            query_node: Box::new(replace_predicates_in_query_node(*query_node, predicates)?),
            arguments: arguments
                .into_iter()
                .map(|(argument_name, argument)| {
                    Ok((
                        argument_name,
                        replace_predicates_in_argument(argument, predicates)?,
                    ))
                })
                .collect::<Result<BTreeMap<_, _>, FilterPredicateError>>()?,
        },
        Field::Column {
            column,
            fields,
            arguments,
        } => Field::Column {
            column,
            fields: fields
                .map(|nested_field| replace_predicates_in_nested_field(nested_field, predicates))
                .transpose()?,
            arguments: arguments
                .into_iter()
                .map(|(argument_name, argument)| {
                    Ok((
                        argument_name,
                        replace_predicates_in_argument(argument, predicates)?,
                    ))
                })
                .collect::<Result<BTreeMap<_, _>, FilterPredicateError>>()?,
        },
    })
}

fn replace_predicates_in_filter_expression(
    filter_expression: ResolvedFilterExpression,
    predicates: &BTreeMap<RemotePredicateKey, ResolvedFilterExpression>,
) -> Result<ResolvedFilterExpression, FilterPredicateError> {
    Ok(match filter_expression {
        ResolvedFilterExpression::RemoteRelationshipComparison {
            remote_predicate_id,
        } => predicates.get(&remote_predicate_id).cloned().ok_or(
            FilterPredicateError::CouldNotFindRemotePredicate(remote_predicate_id),
        )?,
        ResolvedFilterExpression::And { expressions } => ResolvedFilterExpression::And {
            expressions: expressions
                .into_iter()
                .map(|expression| replace_predicates_in_filter_expression(expression, predicates))
                .collect::<Result<Vec<_>, FilterPredicateError>>()?,
        },
        ResolvedFilterExpression::Or { expressions } => ResolvedFilterExpression::Or {
            expressions: expressions
                .into_iter()
                .map(|expression| replace_predicates_in_filter_expression(expression, predicates))
                .collect::<Result<Vec<_>, FilterPredicateError>>()?,
        },
        ResolvedFilterExpression::Not { expression } => ResolvedFilterExpression::Not {
            expression: Box::new(replace_predicates_in_filter_expression(
                *expression,
                predicates,
            )?),
        },
        ResolvedFilterExpression::LocalFieldComparison(local_field_comparison) => {
            ResolvedFilterExpression::LocalFieldComparison(local_field_comparison)
        }
        ResolvedFilterExpression::LocalNestedArray {
            column,
            field_path,
            predicate,
        } => ResolvedFilterExpression::LocalNestedArray {
            column,
            field_path,
            predicate: Box::new(replace_predicates_in_filter_expression(
                *predicate, predicates,
            )?),
        },
        ResolvedFilterExpression::LocalRelationshipComparison {
            relationship,
            predicate,
        } => ResolvedFilterExpression::LocalRelationshipComparison {
            relationship,
            predicate: Box::new(replace_predicates_in_filter_expression(
                *predicate, predicates,
            )?),
        },
    })
}

/// Utility to store distinct comparisons to avoid duplicate comparison predicates
/// in the remote relationship comparison expression.
struct DistinctComparisons {
    comparisons: IndexSet<ResolvedFilterExpression>,
}

impl DistinctComparisons {
    fn new() -> Self {
        DistinctComparisons {
            comparisons: IndexSet::new(),
        }
    }

    fn push(&mut self, expression: ResolvedFilterExpression) {
        self.comparisons.insert(expression);
    }

    fn into_vec(self) -> Vec<ResolvedFilterExpression> {
        self.comparisons.into_iter().collect()
    }
}

/// Build the column comparison expressions using the equal operator NDC response rows.
///
/// [[(a, a_value_1, b, b_value_1), (a, a_value_2, b, b_value_2)]] --->
/// WHERE (a = a_value_1 AND b = b_value_1) OR (a = a_value_2 AND b = b_value_2)
/// The above filter is semantically equivalent to
/// WHERE (a, b) IN ((a_value_1, b_value_1), (a_value_2, b_value_2))
pub fn build_source_column_comparisons(
    mut rows: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>,
    ndc_column_mapping: &[plan_types::RelationshipColumnMapping],
) -> Result<ResolvedFilterExpression, FieldError> {
    let mut expressions = DistinctComparisons::new();
    for row in &mut rows {
        let mut column_comparisons = Vec::new();
        for column_mapping in ndc_column_mapping {
            let target_column_field =
                ndc_models::FieldName::from(column_mapping.target_ndc_column.as_str());
            // Fetch RHS (target) column value from the row
            let target_value = row.swap_remove(&target_column_field).ok_or_else(
                || FieldInternalError::InternalGeneric{
                        description: format!(
                            "Unable to build remote predicate local comparison. Target field from NDC response not found: {target_column_field}"
                        ),
                }
            )?;

            let plan_types::SourceNdcColumn {
                column: source_column,
                field_path,
                eq_operator,
            } = &column_mapping.source_ndc_column;
            // Generate LHS (source) column comparison with target column value
            column_comparisons.push(ResolvedFilterExpression::LocalFieldComparison(
                plan_types::LocalFieldComparison::BinaryComparison {
                    column: plan_types::ComparisonTarget::Column {
                        name: source_column.clone(),
                        field_path: field_path.clone(),
                    },
                    operator: eq_operator.clone(),
                    value: plan_types::ComparisonValue::Scalar {
                        value: target_value.0,
                    },
                },
            ));
        }
        // combine column comparisons from each row with AND
        // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
        expressions.push(ResolvedFilterExpression::mk_and(column_comparisons));
    }
    // combine all row comparisons with OR
    // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    //     OR (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    Ok(ResolvedFilterExpression::mk_or(expressions.into_vec()))
}
