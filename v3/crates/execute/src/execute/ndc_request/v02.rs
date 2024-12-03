use std::collections::BTreeMap;

use indexmap::IndexMap;
use ndc_models as ndc_models_v02;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::types::DataConnectorArgumentName;

use crate::error::{FieldError, FieldInternalError};
use plan_types::{
    AggregateFieldSelection, AggregateSelectionSet, Argument, Field, MutationArgument,
    MutationExecutionPlan, NestedArray, NestedField, NestedObject, OrderByDirection,
    OrderByElement, OrderByTarget, QueryExecutionPlan, QueryNodeNew, Relationship,
    RelationshipArgument, ResolvedFilterExpression, VariableName,
};

pub fn make_query_request(
    query_execution_plan: QueryExecutionPlan,
) -> Result<ndc_models_v02::QueryRequest, FieldError> {
    let query_request = ndc_models_v02::QueryRequest {
        collection: ndc_models_v02::CollectionName::from(query_execution_plan.collection.as_str()),
        query: make_query(query_execution_plan.query_node)?,
        arguments: make_arguments(query_execution_plan.arguments)?,
        collection_relationships: make_collection_relationships(
            query_execution_plan.collection_relationships,
        ),
        variables: make_variables(query_execution_plan.variables),
    };
    Ok(query_request)
}

pub fn make_mutation_request(
    mutation_execution_plan: MutationExecutionPlan,
) -> Result<ndc_models_v02::MutationRequest, FieldError> {
    let mutation_operation = ndc_models_v02::MutationOperation::Procedure {
        name: ndc_models_v02::ProcedureName::new(
            mutation_execution_plan.procedure_name.into_inner(),
        ),
        arguments: make_mutation_arguments(mutation_execution_plan.procedure_arguments)?,
        fields: mutation_execution_plan
            .procedure_fields
            .map(make_nested_field)
            .transpose()?,
    };

    let mutation_request = ndc_models_v02::MutationRequest {
        operations: vec![mutation_operation],
        collection_relationships: make_collection_relationships(
            mutation_execution_plan.collection_relationships,
        ),
    };

    Ok(mutation_request)
}

fn make_variables(
    variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
) -> Option<Vec<BTreeMap<ndc_models_v02::VariableName, serde_json::Value>>> {
    variables.map(|variables| {
        variables
            .into_iter()
            .map(|variables_map| {
                variables_map
                    .into_iter()
                    .map(|(name, value)| {
                        (ndc_models_v02::VariableName::from(name.0.as_str()), value)
                    })
                    .collect()
            })
            .collect()
    })
}

fn make_query(query_node: QueryNodeNew) -> Result<ndc_models_v02::Query, FieldError> {
    let ndc_predicate = query_node.predicate.map(make_expression).transpose()?;

    let ndc_fields = query_node
        .fields
        .map(|fields| {
            fields
                .fields
                .into_iter()
                .map(|(name, field)| {
                    Ok((
                        ndc_models_v02::FieldName::from(name.as_str()),
                        make_field(field)?,
                    ))
                })
                .collect::<Result<IndexMap<_, _>, FieldError>>()
        })
        .transpose()?;

    Ok(ndc_models_v02::Query {
        limit: query_node.limit,
        offset: query_node.offset,
        order_by: query_node.order_by.map(make_order_by).transpose()?,
        predicate: ndc_predicate,
        aggregates: query_node.aggregates.map(make_aggregates),
        fields: ndc_fields,
        groups: None,
    })
}

fn make_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, Argument>,
) -> Result<BTreeMap<ndc_models_v02::ArgumentName, ndc_models_v02::Argument>, FieldError> {
    arguments
        .into_iter()
        .map(|(name, argument)| {
            Ok((
                ndc_models_v02::ArgumentName::new(name.into_inner()),
                make_argument(argument)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

fn make_argument(argument: Argument) -> Result<ndc_models_v02::Argument, FieldError> {
    match argument {
        Argument::Literal { value } => Ok(ndc_models_v02::Argument::Literal { value }),
        Argument::Variable { name } => Ok(ndc_models_v02::Argument::Variable {
            name: ndc_models_v02::VariableName::from(name.0.as_str()),
        }),
        Argument::BooleanExpression { predicate } => {
            let ndc_expression = make_expression(predicate)?;
            Ok(ndc_models_v02::Argument::Literal {
                value: serde_json::to_value(ndc_expression).map_err(|e| {
                    FieldError::InternalError(FieldInternalError::ExpressionSerializationError(e))
                })?,
            })
        }
    }
}

fn make_relationship_arguments_from_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, Argument>,
) -> Result<BTreeMap<ndc_models_v02::ArgumentName, ndc_models_v02::RelationshipArgument>, FieldError>
{
    arguments
        .into_iter()
        .map(|(name, argument)| {
            Ok((
                ndc_models_v02::ArgumentName::new(name.into_inner()),
                make_relationship_argument_from_argument(argument)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

fn make_relationship_argument_from_argument(
    argument: Argument,
) -> Result<ndc_models_v02::RelationshipArgument, FieldError> {
    match argument {
        Argument::Literal { value } => Ok(ndc_models_v02::RelationshipArgument::Literal { value }),
        Argument::Variable { name } => Ok(ndc_models_v02::RelationshipArgument::Variable {
            name: ndc_models_v02::VariableName::from(name.0.as_str()),
        }),
        Argument::BooleanExpression { predicate } => {
            let ndc_expression = make_expression(predicate)?;
            Ok(ndc_models_v02::RelationshipArgument::Literal {
                value: serde_json::to_value(ndc_expression).map_err(|e| {
                    FieldError::InternalError(FieldInternalError::ExpressionSerializationError(e))
                })?,
            })
        }
    }
}

fn make_relationship_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, RelationshipArgument>,
) -> BTreeMap<ndc_models_v02::ArgumentName, ndc_models_v02::RelationshipArgument> {
    arguments
        .into_iter()
        .map(|(name, argument)| {
            (
                ndc_models_v02::ArgumentName::new(name.into_inner()),
                make_relationship_argument(argument),
            )
        })
        .collect::<BTreeMap<_, _>>()
}

fn make_relationship_argument(
    argument: RelationshipArgument,
) -> ndc_models_v02::RelationshipArgument {
    match argument {
        RelationshipArgument::Column { name } => ndc_models_v02::RelationshipArgument::Column {
            name: ndc_models_v02::FieldName::new(name.into_inner()),
        },
    }
}

pub fn make_expression(
    predicate: ResolvedFilterExpression,
) -> Result<ndc_models_v02::Expression, FieldError> {
    match predicate {
        ResolvedFilterExpression::And { expressions } => {
            let mut ndc_expressions = Vec::new();

            for expression in expressions {
                let ndc_expression = make_expression(expression)?;
                ndc_expressions.push(ndc_expression);
            }

            Ok(ndc_models_v02::Expression::And {
                expressions: ndc_expressions,
            })
        }
        ResolvedFilterExpression::Or { expressions } => {
            let mut ndc_expressions = Vec::new();

            for expression in expressions {
                let ndc_expression = make_expression(expression)?;
                ndc_expressions.push(ndc_expression);
            }

            Ok(ndc_models_v02::Expression::Or {
                expressions: ndc_expressions,
            })
        }
        ResolvedFilterExpression::Not { expression } => {
            let ndc_expression = make_expression(*expression)?;

            Ok(ndc_models_v02::Expression::Not {
                expression: Box::new(ndc_expression),
            })
        }
        ResolvedFilterExpression::LocalFieldComparison(
            plan_types::LocalFieldComparison::BinaryComparison {
                column,
                operator,
                value,
            },
        ) => Ok(ndc_models_v02::Expression::BinaryComparisonOperator {
            column: make_comparison_target(column),
            operator: ndc_models_v02::ComparisonOperatorName::new(operator.into_inner()),
            value: make_comparison_value(value),
        }),

        ResolvedFilterExpression::LocalNestedArray {
            column,
            field_path,
            predicate,
        } => {
            let ndc_expression = make_expression(*predicate)?;
            let field_name = ndc_models_v02::FieldName::new(column.into_inner());

            Ok(ndc_models_v02::Expression::Exists {
                in_collection: ndc_models_v02::ExistsInCollection::NestedCollection {
                    column_name: field_name,
                    field_path: field_path
                        .into_iter()
                        .map(|f| ndc_models_v02::FieldName::new(f.into_inner()))
                        .collect(),
                    arguments: BTreeMap::new(),
                },
                predicate: Some(Box::new(ndc_expression)),
            })
        }
        ResolvedFilterExpression::LocalFieldComparison(
            plan_types::LocalFieldComparison::UnaryComparison { column, operator },
        ) => Ok(ndc_models_v02::Expression::UnaryComparisonOperator {
            column: make_comparison_target(column),
            operator: match operator {
                metadata_resolve::UnaryComparisonOperator::IsNull => {
                    ndc_models_v02::UnaryComparisonOperator::IsNull
                }
            },
        }),
        ResolvedFilterExpression::LocalRelationshipComparison {
            relationship,
            predicate,
        } => {
            let ndc_expression = make_expression(*predicate)?;
            Ok(ndc_models_v02::Expression::Exists {
                in_collection: ndc_models_v02::ExistsInCollection::Related {
                    field_path: None,
                    relationship: ndc_models_v02::RelationshipName::from(relationship.as_str()),
                    arguments: BTreeMap::new(),
                },
                predicate: Some(Box::new(ndc_expression)),
            })
        }
        // we are generating NDC request for one connector, we can ignore anything remote
        ResolvedFilterExpression::RemoteRelationshipComparison {
            remote_predicate_id: _,
        } => Ok(ndc_models_v02::Expression::And {
            expressions: vec![],
        }),
    }
}

fn make_comparison_target(
    comparison_target: plan_types::ComparisonTarget,
) -> ndc_models_v02::ComparisonTarget {
    match comparison_target {
        plan_types::ComparisonTarget::Column { name, field_path } => {
            ndc_models_v02::ComparisonTarget::Column {
                name: ndc_models_v02::FieldName::new(name.into_inner()),
                arguments: BTreeMap::new(),
                field_path: if field_path.is_empty() {
                    None
                } else {
                    Some(
                        field_path
                            .into_iter()
                            .map(|f| ndc_models_v02::FieldName::new(f.into_inner()))
                            .collect(),
                    )
                },
            }
        }
    }
}

fn make_comparison_value(
    comparison_value: plan_types::ComparisonValue,
) -> ndc_models_v02::ComparisonValue {
    match comparison_value {
        plan_types::ComparisonValue::Scalar { value } => {
            ndc_models_v02::ComparisonValue::Scalar { value }
        }
        plan_types::ComparisonValue::Variable { name } => {
            ndc_models_v02::ComparisonValue::Variable {
                name: ndc_models_v02::VariableName::from(name.0.as_str()),
            }
        }
    }
}

fn make_field(field: Field) -> Result<ndc_models_v02::Field, FieldError> {
    match field {
        Field::Column {
            column,
            fields,
            arguments,
        } => {
            let nested_fields = fields.map(make_nested_field).transpose()?;

            Ok(ndc_models_v02::Field::Column {
                column: ndc_models_v02::FieldName::new(column.into_inner()),
                fields: nested_fields,
                arguments: make_arguments(arguments)?,
            })
        }
        Field::Relationship {
            query_node,
            relationship,
            arguments,
        } => {
            let query = make_query(*query_node)?;
            Ok(ndc_models_v02::Field::Relationship {
                query: Box::new(query),
                relationship: ndc_models_v02::RelationshipName::from(relationship.as_str()),
                arguments: make_relationship_arguments_from_arguments(arguments)?,
            })
        }
    }
}

fn make_nested_field(nested_field: NestedField) -> Result<ndc_models_v02::NestedField, FieldError> {
    match nested_field {
        NestedField::Object(nested_object) => Ok(ndc_models_v02::NestedField::Object(
            make_nested_object(nested_object)?,
        )),
        NestedField::Array(nested_array) => Ok(ndc_models_v02::NestedField::Array(
            make_nested_array(nested_array)?,
        )),
    }
}

fn make_nested_object(
    nested_field: NestedObject,
) -> Result<ndc_models_v02::NestedObject, FieldError> {
    let fields = nested_field
        .fields
        .into_iter()
        .map(|(name, field)| {
            Ok((
                ndc_models_v02::FieldName::from(name.as_str()),
                make_field(field)?,
            ))
        })
        .collect::<Result<IndexMap<_, _>, FieldError>>()?;
    Ok(ndc_models_v02::NestedObject { fields })
}

fn make_nested_array(nested_field: NestedArray) -> Result<ndc_models_v02::NestedArray, FieldError> {
    let fields = make_nested_field(*nested_field.fields)?;
    Ok(ndc_models_v02::NestedArray {
        fields: Box::new(fields),
    })
}

pub fn make_collection_relationships(
    collection_relationships: BTreeMap<plan_types::NdcRelationshipName, Relationship>,
) -> BTreeMap<ndc_models_v02::RelationshipName, ndc_models_v02::Relationship> {
    collection_relationships
        .into_iter()
        .map(|(name, relationship)| {
            (
                ndc_models_v02::RelationshipName::from(name.as_str()),
                make_relationship(relationship),
            )
        })
        .collect::<BTreeMap<_, _>>()
}

fn make_relationship(relationship: Relationship) -> ndc_models_v02::Relationship {
    ndc_models_v02::Relationship {
        column_mapping: relationship
            .column_mapping
            .into_iter()
            .map(|(s, t)| {
                (
                    ndc_models_v02::FieldName::new(s.into_inner()),
                    vec![ndc_models_v02::FieldName::new(t.into_inner())],
                )
            })
            .collect(),
        relationship_type: match relationship.relationship_type {
            open_dds::relationships::RelationshipType::Object => {
                ndc_models_v02::RelationshipType::Object
            }
            open_dds::relationships::RelationshipType::Array => {
                ndc_models_v02::RelationshipType::Array
            }
        },
        target_collection: ndc_models_v02::CollectionName::new(
            relationship.target_collection.into_inner(),
        ),
        arguments: make_relationship_arguments(relationship.arguments),
    }
}

fn make_order_by(
    order_by_elements: Vec<OrderByElement<ResolvedFilterExpression>>,
) -> Result<ndc_models_v02::OrderBy, FieldError> {
    Ok(ndc_models_v02::OrderBy {
        elements: order_by_elements
            .into_iter()
            .map(|element| {
                Ok(ndc_models_v02::OrderByElement {
                    order_direction: match element.order_direction {
                        OrderByDirection::Asc => ndc_models_v02::OrderDirection::Asc,
                        OrderByDirection::Desc => ndc_models_v02::OrderDirection::Desc,
                    },
                    target: make_order_by_target(element.target)?,
                })
            })
            .collect::<Result<Vec<_>, FieldError>>()?,
    })
}

fn make_order_by_target(
    target: OrderByTarget<ResolvedFilterExpression>,
) -> Result<ndc_models_v02::OrderByTarget, FieldError> {
    match target {
        OrderByTarget::Column {
            name,
            field_path,
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
            for path_element in relationship_path {
                order_by_element_path.push(ndc_models_v02::PathElement {
                    field_path: if path_element.field_path.is_empty() {
                        None
                    } else {
                        Some(
                            path_element
                                .field_path
                                .iter()
                                .map(|name| ndc_models_v02::FieldName::from(name.as_str()))
                                .collect(),
                        )
                    },
                    relationship: ndc_models_v02::RelationshipName::from(
                        path_element.relationship_name.as_str(),
                    ),
                    arguments: BTreeMap::new(),
                    predicate: path_element
                        .filter_predicate
                        .map(make_expression)
                        .transpose()?
                        .map(Box::new),
                });
            }

            Ok(ndc_models_v02::OrderByTarget::Column {
                name: ndc_models_v02::FieldName::new(name.into_inner()),
                arguments: BTreeMap::new(),
                path: order_by_element_path,
                field_path: if field_path.is_empty() {
                    None
                } else {
                    Some(
                        field_path
                            .iter()
                            .map(|name| ndc_models_v02::FieldName::from(name.as_str()))
                            .collect(),
                    )
                },
            })
        }
    }
}

/// Translates the internal IR 'AggregateSelectionSet' into an NDC query aggregates selection
fn make_aggregates(
    aggregate_selection_set: AggregateSelectionSet,
) -> IndexMap<ndc_models_v02::FieldName, ndc_models_v02::Aggregate> {
    aggregate_selection_set
        .fields
        .into_iter()
        .map(|(field_name, aggregate_selection)| {
            let aggregate = match aggregate_selection {
                AggregateFieldSelection::Count { column_path, .. } => {
                    make_count_aggregate(column_path, false)
                }
                AggregateFieldSelection::CountDistinct { column_path, .. } => {
                    make_count_aggregate(column_path, true)
                }
                AggregateFieldSelection::AggregationFunction {
                    function_name,
                    column_path,
                } => {
                    let nonempty::NonEmpty {
                        head: column,
                        tail: field_path,
                    } = column_path;
                    let nested_field_path = field_path
                        .into_iter()
                        .map(|column_name| {
                            ndc_models_v02::FieldName::from(column_name.into_inner())
                        })
                        .collect::<Vec<_>>();
                    ndc_models_v02::Aggregate::SingleColumn {
                        column: ndc_models_v02::FieldName::from(column.into_inner()),
                        arguments: BTreeMap::new(),
                        field_path: if nested_field_path.is_empty() {
                            None
                        } else {
                            Some(nested_field_path)
                        },
                        function: ndc_models_v02::AggregateFunctionName::from(
                            function_name.as_str(),
                        ),
                    }
                }
            };
            (
                ndc_models_v02::FieldName::from(field_name.as_str()),
                aggregate,
            )
        })
        .collect()
}

/// Creates the appropriate NDC count aggregation based on whether we're selecting
/// a column (nested or otherwise) or not
fn make_count_aggregate(
    column_path: Vec<DataConnectorColumnName>,
    distinct: bool,
) -> ndc_models_v02::Aggregate {
    let mut column_path_iter = column_path.into_iter();
    if let Some(first_path_element) = column_path_iter.next() {
        let remaining_path = column_path_iter
            .map(|column_name| ndc_models_v02::FieldName::from(column_name.into_inner()))
            .collect::<Vec<_>>();
        let nested_field_path = if remaining_path.is_empty() {
            None
        } else {
            Some(remaining_path)
        };
        ndc_models_v02::Aggregate::ColumnCount {
            column: ndc_models_v02::FieldName::from(first_path_element.into_inner()),
            arguments: BTreeMap::new(),
            field_path: nested_field_path,
            distinct,
        }
    } else {
        ndc_models_v02::Aggregate::StarCount {}
    }
}

fn make_mutation_arguments(
    arguments: BTreeMap<DataConnectorArgumentName, MutationArgument>,
) -> Result<BTreeMap<ndc_models_v02::ArgumentName, serde_json::Value>, FieldError> {
    arguments
        .into_iter()
        .map(|(name, argument)| {
            Ok((
                ndc_models_v02::ArgumentName::new(name.into_inner()),
                make_mutation_argument(argument)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

fn make_mutation_argument(argument: MutationArgument) -> Result<serde_json::Value, FieldError> {
    match argument {
        MutationArgument::Literal { value } => Ok(value),
        MutationArgument::BooleanExpression { predicate } => {
            let ndc_expression = make_expression(predicate)?;
            Ok(serde_json::to_value(ndc_expression).map_err(|e| {
                FieldError::InternalError(FieldInternalError::ExpressionSerializationError(e))
            })?)
        }
    }
}
