use std::collections::BTreeMap;

use indexmap::IndexMap;
use open_dds::types::DataConnectorArgumentName;

use super::super::types;
use crate::{
    error::{FieldError, FieldInternalError},
    ir,
    remote_joins::types::VariableName,
};

pub fn make_query_request(
    query_execution_plan: types::ResolvedQueryExecutionPlan,
) -> Result<ndc_models_v01::QueryRequest, FieldError> {
    let query_request = ndc_models_v01::QueryRequest {
        collection: ndc_models_v01::CollectionName::from(query_execution_plan.collection.as_str()),
        query: make_query(query_execution_plan.query_node)?,
        arguments: make_arguments(query_execution_plan.arguments)?,
        collection_relationships: make_collection_relationships(
            query_execution_plan.collection_relationships,
        ),
        variables: make_variables(query_execution_plan.variables),
    };
    Ok(query_request)
}

fn make_variables(
    variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
) -> Option<Vec<BTreeMap<ndc_models_v01::VariableName, serde_json::Value>>> {
    variables.map(|variables| {
        variables
            .into_iter()
            .map(|variables_map| {
                variables_map
                    .into_iter()
                    .map(|(name, value)| {
                        (ndc_models_v01::VariableName::from(name.0.as_str()), value)
                    })
                    .collect()
            })
            .collect()
    })
}

fn make_query(query_node: types::ResolvedQueryNode) -> Result<ndc_models_v01::Query, FieldError> {
    let ndc_predicate = query_node.predicate.map(make_expression).transpose()?;

    let ndc_fields = query_node
        .fields
        .map(|fields| {
            fields
                .into_iter()
                .map(|(name, field)| {
                    Ok((
                        ndc_models_v01::FieldName::from(name.as_str()),
                        make_field(field)?,
                    ))
                })
                .collect::<Result<IndexMap<_, _>, FieldError>>()
        })
        .transpose()?;

    Ok(ndc_models_v01::Query {
        limit: query_node.limit,
        offset: query_node.offset,
        order_by: query_node.order_by.map(make_order_by),
        predicate: ndc_predicate,
        aggregates: query_node.aggregates.map(make_aggregates),
        fields: ndc_fields,
    })
}

fn make_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, types::ResolvedArgument>,
) -> Result<BTreeMap<ndc_models_v01::ArgumentName, ndc_models_v01::Argument>, FieldError> {
    arguments
        .into_iter()
        .map(|(name, argument)| {
            Ok((
                ndc_models_v01::ArgumentName::new(name.into_inner()),
                make_argument(argument)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

fn make_argument(
    argument: types::ResolvedArgument,
) -> Result<ndc_models_v01::Argument, FieldError> {
    match argument {
        types::ResolvedArgument::Literal { value } => {
            Ok(ndc_models_v01::Argument::Literal { value })
        }
        types::ResolvedArgument::Variable { name } => Ok(ndc_models_v01::Argument::Variable {
            name: ndc_models_v01::VariableName::from(name.0.as_str()),
        }),
        types::ResolvedArgument::BooleanExpression { predicate } => {
            let ndc_expression = make_expression(predicate)?;
            Ok(ndc_models_v01::Argument::Literal {
                value: serde_json::to_value(ndc_expression).map_err(|e| {
                    FieldError::InternalError(FieldInternalError::ExpressionSerializationError(e))
                })?,
            })
        }
    }
}

fn make_relationship_arguments_from_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, types::ResolvedArgument>,
) -> Result<BTreeMap<ndc_models_v01::ArgumentName, ndc_models_v01::RelationshipArgument>, FieldError>
{
    arguments
        .into_iter()
        .map(|(name, argument)| {
            Ok((
                ndc_models_v01::ArgumentName::new(name.into_inner()),
                make_relationship_argument_from_argument(argument)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

fn make_relationship_argument_from_argument(
    argument: types::ResolvedArgument,
) -> Result<ndc_models_v01::RelationshipArgument, FieldError> {
    match argument {
        types::ResolvedArgument::Literal { value } => {
            Ok(ndc_models_v01::RelationshipArgument::Literal { value })
        }
        types::ResolvedArgument::Variable { name } => {
            Ok(ndc_models_v01::RelationshipArgument::Variable {
                name: ndc_models_v01::VariableName::from(name.0.as_str()),
            })
        }
        types::ResolvedArgument::BooleanExpression { predicate } => {
            let ndc_expression = make_expression(predicate)?;
            Ok(ndc_models_v01::RelationshipArgument::Literal {
                value: serde_json::to_value(ndc_expression).map_err(|e| {
                    FieldError::InternalError(FieldInternalError::ExpressionSerializationError(e))
                })?,
            })
        }
    }
}

fn make_relationship_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, types::RelationshipArgument>,
) -> BTreeMap<ndc_models_v01::ArgumentName, ndc_models_v01::RelationshipArgument> {
    arguments
        .into_iter()
        .map(|(name, argument)| {
            (
                ndc_models_v01::ArgumentName::new(name.into_inner()),
                make_relationship_argument(argument),
            )
        })
        .collect::<BTreeMap<_, _>>()
}

fn make_relationship_argument(
    argument: types::RelationshipArgument,
) -> ndc_models_v01::RelationshipArgument {
    match argument {
        types::RelationshipArgument::Column { name } => {
            ndc_models_v01::RelationshipArgument::Column {
                name: ndc_models_v01::FieldName::new(name.into_inner()),
            }
        }
    }
}

fn make_mutation_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, types::ResolvedMutationArgument>,
) -> Result<BTreeMap<ndc_models_v01::ArgumentName, serde_json::Value>, FieldError> {
    arguments
        .into_iter()
        .map(|(name, argument)| {
            Ok((
                ndc_models_v01::ArgumentName::new(name.into_inner()),
                make_mutation_argument(argument)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

fn make_mutation_argument(
    argument: types::ResolvedMutationArgument,
) -> Result<serde_json::Value, FieldError> {
    match argument {
        types::ResolvedMutationArgument::Literal { value } => Ok(value),
        types::ResolvedMutationArgument::BooleanExpression { predicate } => {
            let ndc_expression = make_expression(predicate)?;
            Ok(serde_json::to_value(ndc_expression).map_err(|e| {
                FieldError::InternalError(FieldInternalError::ExpressionSerializationError(e))
            })?)
        }
    }
}

fn make_expression(
    predicate: types::ResolvedFilterExpression,
) -> Result<ndc_models_v01::Expression, FieldError> {
    match predicate {
        types::ResolvedFilterExpression::And { expressions } => {
            let mut ndc_expressions = Vec::new();
            for expression in expressions {
                let ndc_expression = make_expression(expression)?;
                ndc_expressions.push(ndc_expression);
            }
            Ok(ndc_models_v01::Expression::And {
                expressions: ndc_expressions,
            })
        }
        types::ResolvedFilterExpression::Or { expressions } => {
            let mut ndc_expressions = Vec::new();
            for expression in expressions {
                let ndc_expression = make_expression(expression)?;
                ndc_expressions.push(ndc_expression);
            }
            Ok(ndc_models_v01::Expression::Or {
                expressions: ndc_expressions,
            })
        }
        types::ResolvedFilterExpression::Not { expression } => {
            let ndc_expression = make_expression(*expression)?;
            Ok(ndc_models_v01::Expression::Not {
                expression: Box::new(ndc_expression),
            })
        }
        types::ResolvedFilterExpression::LocalFieldComparison(
            ir::filter::expression::LocalFieldComparison::BinaryComparison {
                column,
                operator,
                value,
            },
        ) => Ok(ndc_models_v01::Expression::BinaryComparisonOperator {
            column: make_comparison_target(column),
            operator: ndc_models_v01::ComparisonOperatorName::new(operator.into_inner()),
            value: make_comparison_value(value),
        }),
        types::ResolvedFilterExpression::LocalFieldComparison(
            ir::filter::expression::LocalFieldComparison::UnaryComparison { column, operator },
        ) => Ok(ndc_models_v01::Expression::UnaryComparisonOperator {
            column: make_comparison_target(column),
            operator: match operator {
                metadata_resolve::UnaryComparisonOperator::IsNull => {
                    ndc_models_v01::UnaryComparisonOperator::IsNull
                }
            },
        }),
        types::ResolvedFilterExpression::LocalRelationshipComparison {
            relationship,
            predicate,
        } => {
            let ndc_expression = make_expression(*predicate)?;
            Ok(ndc_models_v01::Expression::Exists {
                in_collection: ndc_models_v01::ExistsInCollection::Related {
                    relationship: ndc_models_v01::RelationshipName::from(relationship.as_str()),
                    arguments: BTreeMap::new(),
                },
                predicate: Some(Box::new(ndc_expression)),
            })
        }
    }
}

fn make_comparison_target(
    comparison_target: ir::filter::expression::ComparisonTarget,
) -> ndc_models_v01::ComparisonTarget {
    match comparison_target {
        ir::filter::expression::ComparisonTarget::Column { name, field_path } => {
            ndc_models_v01::ComparisonTarget::Column {
                name: ndc_models_v01::FieldName::new(name.into_inner()),
                field_path: if field_path.is_empty() {
                    None
                } else {
                    Some(
                        field_path
                            .into_iter()
                            .map(|f| ndc_models_v01::FieldName::new(f.into_inner()))
                            .collect(),
                    )
                },
                path: vec![],
            }
        }
    }
}

fn make_comparison_value(
    comparison_value: ir::filter::expression::ComparisonValue,
) -> ndc_models_v01::ComparisonValue {
    match comparison_value {
        ir::filter::expression::ComparisonValue::Scalar { value } => {
            ndc_models_v01::ComparisonValue::Scalar { value }
        }
        ir::filter::expression::ComparisonValue::Variable { name } => {
            ndc_models_v01::ComparisonValue::Variable {
                name: ndc_models_v01::VariableName::from(name.0.as_str()),
            }
        }
    }
}

fn make_field(field: types::ResolvedField) -> Result<ndc_models_v01::Field, FieldError> {
    match field {
        types::ResolvedField::Column {
            column,
            fields,
            arguments,
        } => {
            let nested_fields = fields.map(make_nested_field).transpose()?;

            Ok(ndc_models_v01::Field::Column {
                column: ndc_models_v01::FieldName::new(column.into_inner()),
                fields: nested_fields,
                arguments: make_arguments(arguments)?,
            })
        }
        types::ResolvedField::Relationship {
            query_node,
            relationship,
            arguments,
        } => {
            let query = make_query(*query_node)?;
            Ok(ndc_models_v01::Field::Relationship {
                query: Box::new(query),
                relationship: ndc_models_v01::RelationshipName::from(relationship.as_str()),
                arguments: make_relationship_arguments_from_arguments(arguments)?,
            })
        }
    }
}

fn make_nested_field(
    nested_field: types::ResolvedNestedField,
) -> Result<ndc_models_v01::NestedField, FieldError> {
    match nested_field {
        types::ResolvedNestedField::Object(nested_object) => Ok(
            ndc_models_v01::NestedField::Object(make_nested_object(nested_object)?),
        ),
        types::ResolvedNestedField::Array(nested_array) => Ok(ndc_models_v01::NestedField::Array(
            make_nested_array(nested_array)?,
        )),
    }
}

fn make_nested_object(
    nested_field: types::ResolvedNestedObject,
) -> Result<ndc_models_v01::NestedObject, FieldError> {
    let fields = nested_field
        .fields
        .into_iter()
        .map(|(name, field)| {
            Ok((
                ndc_models_v01::FieldName::from(name.as_str()),
                make_field(field)?,
            ))
        })
        .collect::<Result<IndexMap<_, _>, FieldError>>()?;
    Ok(ndc_models_v01::NestedObject { fields })
}

fn make_nested_array(
    nested_field: types::ResolvedNestedArray,
) -> Result<ndc_models_v01::NestedArray, FieldError> {
    let fields = make_nested_field(*nested_field.fields)?;
    Ok(ndc_models_v01::NestedArray {
        fields: Box::new(fields),
    })
}

fn make_collection_relationships(
    collection_relationships: BTreeMap<ir::selection_set::NdcRelationshipName, types::Relationship>,
) -> BTreeMap<ndc_models_v01::RelationshipName, ndc_models_v01::Relationship> {
    collection_relationships
        .into_iter()
        .map(|(name, relationship)| {
            (
                ndc_models_v01::RelationshipName::from(name.as_str()),
                make_relationship(relationship),
            )
        })
        .collect::<BTreeMap<_, _>>()
}

fn make_relationship(relationship: types::Relationship) -> ndc_models_v01::Relationship {
    ndc_models_v01::Relationship {
        column_mapping: relationship
            .column_mapping
            .into_iter()
            .map(|(s, t)| {
                (
                    ndc_models_v01::FieldName::new(s.into_inner()),
                    ndc_models_v01::FieldName::new(t.into_inner()),
                )
            })
            .collect(),
        relationship_type: match relationship.relationship_type {
            open_dds::relationships::RelationshipType::Object => {
                ndc_models_v01::RelationshipType::Object
            }
            open_dds::relationships::RelationshipType::Array => {
                ndc_models_v01::RelationshipType::Array
            }
        },
        target_collection: ndc_models_v01::CollectionName::new(
            relationship.target_collection.into_inner(),
        ),
        arguments: make_relationship_arguments(relationship.arguments),
    }
}

fn make_order_by(order_by_elements: Vec<ir::order_by::OrderByElement>) -> ndc_models_v01::OrderBy {
    ndc_models_v01::OrderBy {
        elements: order_by_elements
            .into_iter()
            .map(|element| ndc_models_v01::OrderByElement {
                order_direction: match element.order_direction {
                    schema::ModelOrderByDirection::Asc => ndc_models_v01::OrderDirection::Asc,
                    schema::ModelOrderByDirection::Desc => ndc_models_v01::OrderDirection::Desc,
                },
                target: make_order_by_target(element.target),
            })
            .collect(),
    }
}

fn make_order_by_target(target: ir::order_by::OrderByTarget) -> ndc_models_v01::OrderByTarget {
    match target {
        ir::order_by::OrderByTarget::Column {
            name,
            relationship_path,
        } => {
            let mut order_by_element_path = Vec::new();
            // When using a nested relationship column, you'll have to provide all the relationships(paths)
            // NDC has to traverse to access the column. The ordering of that paths is important.
            // The order decides how to access the column.
            //
            // For example, if you have a model called `User` with a relationship column called `Posts`
            // which has a relationship column called `Comments` which has a non-relationship column
            // called `text`, you'll have to provide the following paths to access the `text` column:
            // ["UserPosts", "PostsComments"]
            for path in relationship_path {
                order_by_element_path.push(ndc_models_v01::PathElement {
                    relationship: ndc_models_v01::RelationshipName::from(path.0.as_str()),
                    arguments: BTreeMap::new(),
                    // 'AND' predicate indicates that the column can be accessed
                    // by joining all the relationships paths provided
                    predicate: Some(Box::new(ndc_models_v01::Expression::And {
                        // TODO(naveen): Add expressions here, when we support sorting with predicates.
                        //
                        // There are two types of sorting:
                        //     1. Sorting without predicates
                        //     2. Sorting with predicates
                        //
                        // In the 1st sort, we sort all the elements of the results either in ascending
                        // or descing order based on the order_by argument.
                        //
                        // In the 2nd sort, we want fetch the entire result but only sort a subset
                        // of result and put those sorted set either at the beginning or at the end of the
                        // result.
                        //
                        // Currently we only support the 1st type of sort. Hence we don't have any expressions/predicate.
                        expressions: Vec::new(),
                    })),
                });
            }

            ndc_models_v01::OrderByTarget::Column {
                name: ndc_models_v01::FieldName::new(name.into_inner()),
                path: order_by_element_path,
                field_path: None,
            }
        }
    }
}

/// Translates the internal IR 'AggregateSelectionSet' into an NDC query aggregates selection
fn make_aggregates(
    aggregate_selection_set: ir::aggregates::AggregateSelectionSet,
) -> IndexMap<ndc_models_v01::FieldName, ndc_models_v01::Aggregate> {
    aggregate_selection_set
        .fields
        .into_iter()
        .map(|(field_name, aggregate_selection)| {
            let aggregate = match aggregate_selection {
                ir::aggregates::AggregateFieldSelection::Count { column_path, .. } => {
                    make_count_aggregate(column_path, false)
                }
                ir::aggregates::AggregateFieldSelection::CountDistinct { column_path, .. } => {
                    make_count_aggregate(column_path, true)
                }
                ir::aggregates::AggregateFieldSelection::AggregationFunction {
                    function_name,
                    column_path,
                } => {
                    let nonempty::NonEmpty {
                        head: column,
                        tail: field_path,
                    } = column_path;
                    let nested_field_path = field_path
                        .into_iter()
                        .map(ndc_models_v01::FieldName::from)
                        .collect::<Vec<_>>();
                    ndc_models_v01::Aggregate::SingleColumn {
                        column: ndc_models_v01::FieldName::from(column),
                        field_path: if nested_field_path.is_empty() {
                            None
                        } else {
                            Some(nested_field_path)
                        },
                        function: ndc_models_v01::AggregateFunctionName::from(
                            function_name.0.as_str(),
                        ),
                    }
                }
            };
            (
                ndc_models_v01::FieldName::from(field_name.as_str()),
                aggregate,
            )
        })
        .collect()
}

/// Creates the appropriate NDC count aggregation based on whether we're selecting
/// a column (nested or otherwise) or not
fn make_count_aggregate(column_path: Vec<&str>, distinct: bool) -> ndc_models_v01::Aggregate {
    let mut column_path_iter = column_path.into_iter();
    if let Some(first_path_element) = column_path_iter.next() {
        let remaining_path = column_path_iter
            .map(ndc_models_v01::FieldName::from)
            .collect::<Vec<_>>();
        let nested_field_path = if remaining_path.is_empty() {
            None
        } else {
            Some(remaining_path)
        };
        ndc_models_v01::Aggregate::ColumnCount {
            column: ndc_models_v01::FieldName::from(first_path_element),
            field_path: nested_field_path,
            distinct,
        }
    } else {
        ndc_models_v01::Aggregate::StarCount {}
    }
}

pub fn make_mutation_request(
    mutation_execution_plan: types::ResolvedMutationExecutionPlan,
) -> Result<ndc_models_v01::MutationRequest, FieldError> {
    let mutation_operation = ndc_models_v01::MutationOperation::Procedure {
        name: ndc_models_v01::ProcedureName::new(
            mutation_execution_plan.procedure_name.into_inner(),
        ),
        arguments: make_mutation_arguments(mutation_execution_plan.procedure_arguments)?,
        fields: mutation_execution_plan
            .procedure_fields
            .map(make_nested_field)
            .transpose()?,
    };

    let mutation_request = ndc_models_v01::MutationRequest {
        operations: vec![mutation_operation],
        collection_relationships: make_collection_relationships(
            mutation_execution_plan.collection_relationships,
        ),
    };

    Ok(mutation_request)
}
