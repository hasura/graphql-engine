use indexmap::IndexMap;
use std::collections::BTreeMap;

use ndc_models;
use ndc_models_v01;

use super::NdcDowngradeError;

pub fn upgrade_rowset_to_v02(rowset: ndc_models_v01::RowSet) -> ndc_models::RowSet {
    ndc_models::RowSet {
        aggregates: rowset.aggregates.map(|aggregates| {
            aggregates
                .into_iter()
                .map(|(name, value)| (ndc_models::FieldName::new(name.into_inner()), value))
                .collect()
        }),
        rows: rowset.rows.map(|rows| {
            rows.into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|(name, ndc_models_v01::RowFieldValue(v))| {
                            (
                                ndc_models::FieldName::new(name.into_inner()),
                                ndc_models::RowFieldValue(v),
                            )
                        })
                        .collect()
                })
                .collect()
        }),
        groups: None, // v0.1.x does not have groups
    }
}

pub fn upgrade_mutation_response_to_v02(
    mutation_response: ndc_models_v01::MutationResponse,
) -> ndc_models::MutationResponse {
    ndc_models::MutationResponse {
        operation_results: mutation_response
            .operation_results
            .into_iter()
            .map(upgrade_mutation_operation_result_to_latest)
            .collect(),
    }
}

fn upgrade_mutation_operation_result_to_latest(
    mutation_operation_result: ndc_models_v01::MutationOperationResults,
) -> ndc_models::MutationOperationResults {
    match mutation_operation_result {
        ndc_models_v01::MutationOperationResults::Procedure { result } => {
            ndc_models::MutationOperationResults::Procedure { result }
        }
    }
}

pub fn downgrade_v02_query_request(
    query_request: ndc_models::QueryRequest,
) -> Result<ndc_models_v01::QueryRequest, NdcDowngradeError> {
    Ok(ndc_models_v01::QueryRequest {
        arguments: query_request
            .arguments
            .into_iter()
            .map(|(name, argument)| (name.into_inner().into(), downgrade_v02_argument(argument)))
            .collect(),
        collection: query_request.collection.into_inner().into(),
        query: downgrade_v02_query(query_request.query)?,
        collection_relationships: query_request
            .collection_relationships
            .into_iter()
            .map(|(name, relationship)| {
                (
                    name.into_inner().into(),
                    downgrade_v02_relationship(relationship),
                )
            })
            .collect(),
        variables: downgrade_v02_variables(query_request.variables),
    })
}

pub fn downgrade_v02_variables(
    variables: Option<Vec<BTreeMap<ndc_models::VariableName, serde_json::Value>>>,
) -> Option<Vec<BTreeMap<ndc_models_v01::VariableName, serde_json::Value>>> {
    variables.map(|variables| {
        variables
            .into_iter()
            .map(|variables| {
                variables
                    .into_iter()
                    .map(|(name, value)| {
                        (ndc_models_v01::VariableName::new(name.into_inner()), value)
                    })
                    .collect()
            })
            .collect()
    })
}

fn downgrade_v02_argument(argument: ndc_models::Argument) -> ndc_models_v01::Argument {
    match argument {
        ndc_models::Argument::Variable { name } => ndc_models_v01::Argument::Variable {
            name: name.into_inner().into(),
        },
        ndc_models::Argument::Literal { value } => ndc_models_v01::Argument::Literal { value },
    }
}

fn downgrade_v02_query(
    query: ndc_models::Query,
) -> Result<ndc_models_v01::Query, NdcDowngradeError> {
    Ok(ndc_models_v01::Query {
        aggregates: query.aggregates.map(|aggregates| {
            aggregates
                .into_iter()
                .map(|(name, aggregate)| {
                    (name.into_inner().into(), downgrade_v02_aggregate(aggregate))
                })
                .collect()
        }),
        fields: query
            .fields
            .map(|fields| {
                fields
                    .into_iter()
                    .map(|(name, field)| {
                        Ok((name.into_inner().into(), downgrade_v02_field(field)?))
                    })
                    .collect::<Result<IndexMap<_, _>, _>>()
            })
            .transpose()?,
        limit: query.limit,
        offset: query.offset,
        order_by: query.order_by.map(downgrade_v02_order_by).transpose()?,
        predicate: query.predicate.map(downgrade_v02_expression).transpose()?,
    })
}

fn downgrade_v02_aggregate(aggregate: ndc_models::Aggregate) -> ndc_models_v01::Aggregate {
    match aggregate {
        ndc_models::Aggregate::ColumnCount {
            column,
            field_path,
            distinct,
        } => ndc_models_v01::Aggregate::ColumnCount {
            column: column.into_inner().into(),
            field_path: field_path
                .map(|fp| fp.into_iter().map(|f| f.into_inner().into()).collect()),
            distinct,
        },
        ndc_models::Aggregate::SingleColumn {
            column,
            field_path,
            function,
        } => ndc_models_v01::Aggregate::SingleColumn {
            column: column.into_inner().into(),
            field_path: field_path
                .map(|fp| fp.into_iter().map(|f| f.into_inner().into()).collect()),
            function: function.into_inner().into(),
        },
        ndc_models::Aggregate::StarCount {} => ndc_models_v01::Aggregate::StarCount {},
    }
}

fn downgrade_v02_field(
    field: ndc_models::Field,
) -> Result<ndc_models_v01::Field, NdcDowngradeError> {
    match field {
        ndc_models::Field::Column {
            column,
            fields,
            arguments,
        } => Ok(ndc_models_v01::Field::Column {
            column: column.into_inner().into(),
            fields: fields.map(downgrade_v02_nested_field).transpose()?,
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| {
                    (name.into_inner().into(), downgrade_v02_argument(argument))
                })
                .collect(),
        }),
        ndc_models::Field::Relationship {
            query,
            relationship,
            arguments,
        } => Ok(ndc_models_v01::Field::Relationship {
            query: Box::new(downgrade_v02_query(*query)?),
            relationship: relationship.into_inner().into(),
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| {
                    (
                        name.into_inner().into(),
                        downgrade_v02_relationship_argument(argument),
                    )
                })
                .collect(),
        }),
    }
}

fn downgrade_v02_nested_field(
    nested_field: ndc_models::NestedField,
) -> Result<ndc_models_v01::NestedField, NdcDowngradeError> {
    match nested_field {
        ndc_models::NestedField::Object(nested_object) => Ok(ndc_models_v01::NestedField::Object(
            downgrade_v02_nested_object(nested_object)?,
        )),
        ndc_models::NestedField::Array(nested_array) => Ok(ndc_models_v01::NestedField::Array(
            downgrade_v02_nested_array(nested_array)?,
        )),
        ndc_models::NestedField::Collection(_) => {
            Err(NdcDowngradeError::NestedFieldCollectionsNotSupportedInV01)
        }
    }
}

fn downgrade_v02_nested_object(
    nested_object: ndc_models::NestedObject,
) -> Result<ndc_models_v01::NestedObject, NdcDowngradeError> {
    Ok(ndc_models_v01::NestedObject {
        fields: nested_object
            .fields
            .into_iter()
            .map(|(name, field)| Ok((name.into_inner().into(), downgrade_v02_field(field)?)))
            .collect::<Result<IndexMap<_, _>, _>>()?,
    })
}

fn downgrade_v02_nested_array(
    nested_array: ndc_models::NestedArray,
) -> Result<ndc_models_v01::NestedArray, NdcDowngradeError> {
    Ok(ndc_models_v01::NestedArray {
        fields: Box::new(downgrade_v02_nested_field(*nested_array.fields)?),
    })
}

fn downgrade_v02_relationship_argument(
    relationship_argument: ndc_models::RelationshipArgument,
) -> ndc_models_v01::RelationshipArgument {
    match relationship_argument {
        ndc_models::RelationshipArgument::Variable { name } => {
            ndc_models_v01::RelationshipArgument::Variable {
                name: name.into_inner().into(),
            }
        }
        ndc_models::RelationshipArgument::Literal { value } => {
            ndc_models_v01::RelationshipArgument::Literal { value }
        }
        ndc_models::RelationshipArgument::Column { name } => {
            ndc_models_v01::RelationshipArgument::Column {
                name: name.into_inner().into(),
            }
        }
    }
}

fn downgrade_v02_order_by(
    order_by: ndc_models::OrderBy,
) -> Result<ndc_models_v01::OrderBy, NdcDowngradeError> {
    Ok(ndc_models_v01::OrderBy {
        elements: order_by
            .elements
            .into_iter()
            .map(downgrade_v02_order_by_element)
            .collect::<Result<_, _>>()?,
    })
}

fn downgrade_v02_order_by_element(
    order_by_element: ndc_models::OrderByElement,
) -> Result<ndc_models_v01::OrderByElement, NdcDowngradeError> {
    Ok(ndc_models_v01::OrderByElement {
        order_direction: downgrade_v02_order_direction(order_by_element.order_direction),
        target: downgrade_v02_order_by_target(order_by_element.target)?,
    })
}

fn downgrade_v02_order_direction(
    order_direction: ndc_models::OrderDirection,
) -> ndc_models_v01::OrderDirection {
    match order_direction {
        ndc_models::OrderDirection::Asc => ndc_models_v01::OrderDirection::Asc,
        ndc_models::OrderDirection::Desc => ndc_models_v01::OrderDirection::Desc,
    }
}

fn downgrade_v02_order_by_target(
    target: ndc_models::OrderByTarget,
) -> Result<ndc_models_v01::OrderByTarget, NdcDowngradeError> {
    match target {
        ndc_models::OrderByTarget::Column {
            name,
            field_path,
            path,
        } => Ok(ndc_models_v01::OrderByTarget::Column {
            name: name.into_inner().into(),
            field_path: field_path
                .map(|fp| fp.into_iter().map(|f| f.into_inner().into()).collect()),
            path: path
                .into_iter()
                .map(downgrade_v02_path_element)
                .collect::<Result<_, _>>()?,
        }),
        ndc_models::OrderByTarget::Aggregate { aggregate, path } => match aggregate {
            ndc_models::Aggregate::ColumnCount {
                column: _,
                field_path: _,
                distinct: _,
            } => Err(NdcDowngradeError::OrderByAggregateColumnCountNotSupportedInV01),
            ndc_models::Aggregate::SingleColumn {
                column,
                field_path,
                function,
            } => Ok(ndc_models_v01::OrderByTarget::SingleColumnAggregate {
                column: column.into_inner().into(),
                field_path: field_path
                    .map(|fp| fp.into_iter().map(|f| f.into_inner().into()).collect()),
                function: function.into_inner().into(),
                path: path
                    .into_iter()
                    .map(downgrade_v02_path_element)
                    .collect::<Result<_, _>>()?,
            }),
            ndc_models::Aggregate::StarCount {} => {
                Ok(ndc_models_v01::OrderByTarget::StarCountAggregate {
                    path: path
                        .into_iter()
                        .map(downgrade_v02_path_element)
                        .collect::<Result<_, _>>()?,
                })
            }
        },
    }
}

fn downgrade_v02_path_element(
    path_element: ndc_models::PathElement,
) -> Result<ndc_models_v01::PathElement, NdcDowngradeError> {
    Ok(ndc_models_v01::PathElement {
        relationship: path_element.relationship.into_inner().into(),
        arguments: path_element
            .arguments
            .into_iter()
            .map(|(name, argument)| {
                (
                    name.into_inner().into(),
                    downgrade_v02_relationship_argument(argument),
                )
            })
            .collect(),
        predicate: path_element
            .predicate
            .map(|predicate| Ok(Box::new(downgrade_v02_expression(*predicate)?)))
            .transpose()?,
    })
}

pub fn downgrade_v02_expression(
    predicate: ndc_models::Expression,
) -> Result<ndc_models_v01::Expression, NdcDowngradeError> {
    match predicate {
        ndc_models::Expression::And { expressions } => Ok(ndc_models_v01::Expression::And {
            expressions: expressions
                .into_iter()
                .map(downgrade_v02_expression)
                .collect::<Result<Vec<_>, _>>()?,
        }),
        ndc_models::Expression::Or { expressions } => Ok(ndc_models_v01::Expression::Or {
            expressions: expressions
                .into_iter()
                .map(downgrade_v02_expression)
                .collect::<Result<Vec<_>, _>>()?,
        }),
        ndc_models::Expression::Not { expression } => Ok(ndc_models_v01::Expression::Not {
            expression: Box::new(downgrade_v02_expression(*expression)?),
        }),
        ndc_models::Expression::UnaryComparisonOperator { column, operator } => {
            Ok(ndc_models_v01::Expression::UnaryComparisonOperator {
                column: downgrade_v02_comparison_target(column)?,
                operator: downgrade_v02_unary_comparison_operator(operator),
            })
        }
        ndc_models::Expression::BinaryComparisonOperator {
            column,
            operator,
            value,
        } => Ok(ndc_models_v01::Expression::BinaryComparisonOperator {
            column: downgrade_v02_comparison_target(column)?,
            operator: operator.into_inner().into(),
            value: downgrade_v02_binary_comparison_value(value)?,
        }),
        ndc_models::Expression::Exists {
            in_collection,
            predicate,
        } => Ok(ndc_models_v01::Expression::Exists {
            in_collection: downgrade_v02_exists_in_collection(in_collection),
            predicate: predicate
                .map(|predicate| Ok(Box::new(downgrade_v02_expression(*predicate)?)))
                .transpose()?,
        }),
    }
}

fn downgrade_v02_comparison_target(
    column: ndc_models::ComparisonTarget,
) -> Result<ndc_models_v01::ComparisonTarget, NdcDowngradeError> {
    match column {
        ndc_models::ComparisonTarget::Column { name, field_path } => {
            Ok(ndc_models_v01::ComparisonTarget::Column {
                name: name.into_inner().into(),
                field_path: field_path
                    .map(|fp| fp.into_iter().map(|f| f.into_inner().into()).collect()),
                path: vec![], // This was removed in v0.2.x
            })
        }
        ndc_models::ComparisonTarget::Aggregate {
            aggregate: _,
            path: _,
        } => Err(NdcDowngradeError::AggregateComparisonsNotSupportedInV01),
    }
}

fn downgrade_v02_unary_comparison_operator(
    operator: ndc_models::UnaryComparisonOperator,
) -> ndc_models_v01::UnaryComparisonOperator {
    match operator {
        ndc_models::UnaryComparisonOperator::IsNull => {
            ndc_models_v01::UnaryComparisonOperator::IsNull
        }
    }
}

fn downgrade_v02_binary_comparison_value(
    value: ndc_models::ComparisonValue,
) -> Result<ndc_models_v01::ComparisonValue, NdcDowngradeError> {
    match value {
        ndc_models::ComparisonValue::Column {
            name,
            field_path,
            path,
            scope,
        } => {
            if scope.is_some() {
                Err(NdcDowngradeError::ComparisonValueScopesNotSupportedInV01)
            } else {
                Ok(ndc_models_v01::ComparisonValue::Column {
                    column: ndc_models_v01::ComparisonTarget::Column {
                        name: name.into_inner().into(),
                        field_path: field_path
                            .map(|fp| fp.into_iter().map(|f| f.into_inner().into()).collect()),
                        path: path
                            .into_iter()
                            .map(downgrade_v02_path_element)
                            .collect::<Result<_, _>>()?,
                    },
                })
            }
        }
        ndc_models::ComparisonValue::Scalar { value } => {
            Ok(ndc_models_v01::ComparisonValue::Scalar { value })
        }
        ndc_models::ComparisonValue::Variable { name } => {
            Ok(ndc_models_v01::ComparisonValue::Variable {
                name: name.into_inner().into(),
            })
        }
    }
}

fn downgrade_v02_exists_in_collection(
    exists_in_collection: ndc_models::ExistsInCollection,
) -> ndc_models_v01::ExistsInCollection {
    match exists_in_collection {
        ndc_models::ExistsInCollection::Related {
            relationship,
            arguments,
        } => ndc_models_v01::ExistsInCollection::Related {
            relationship: relationship.into_inner().into(),
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| {
                    (
                        name.into_inner().into(),
                        downgrade_v02_relationship_argument(argument),
                    )
                })
                .collect(),
        },
        ndc_models::ExistsInCollection::Unrelated {
            collection,
            arguments,
        } => ndc_models_v01::ExistsInCollection::Unrelated {
            collection: collection.into_inner().into(),
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| {
                    (
                        name.into_inner().into(),
                        downgrade_v02_relationship_argument(argument),
                    )
                })
                .collect(),
        },
    }
}

fn downgrade_v02_relationship(
    relationship: ndc_models::Relationship,
) -> ndc_models_v01::Relationship {
    ndc_models_v01::Relationship {
        column_mapping: relationship
            .column_mapping
            .into_iter()
            .map(|(k, v)| (k.into_inner().into(), v.into_inner().into()))
            .collect(),
        relationship_type: downgrade_v02_relationship_type(relationship.relationship_type),
        target_collection: relationship.target_collection.into_inner().into(),
        arguments: relationship
            .arguments
            .into_iter()
            .map(|(name, argument)| {
                (
                    name.into_inner().into(),
                    downgrade_v02_relationship_argument(argument),
                )
            })
            .collect(),
    }
}

fn downgrade_v02_relationship_type(
    relationship_type: ndc_models::RelationshipType,
) -> ndc_models_v01::RelationshipType {
    match relationship_type {
        ndc_models::RelationshipType::Object => ndc_models_v01::RelationshipType::Object,
        ndc_models::RelationshipType::Array => ndc_models_v01::RelationshipType::Array,
    }
}

pub fn downgrade_v02_mutation_request(
    mutation_request: ndc_models::MutationRequest,
) -> Result<ndc_models_v01::MutationRequest, NdcDowngradeError> {
    Ok(ndc_models_v01::MutationRequest {
        operations: mutation_request
            .operations
            .into_iter()
            .map(downgrade_v02_mutation_operation)
            .collect::<Result<_, _>>()?,
        collection_relationships: mutation_request
            .collection_relationships
            .into_iter()
            .map(|(name, relationship)| {
                (
                    name.into_inner().into(),
                    downgrade_v02_relationship(relationship),
                )
            })
            .collect(),
    })
}

fn downgrade_v02_mutation_operation(
    mutation_operation: ndc_models::MutationOperation,
) -> Result<ndc_models_v01::MutationOperation, NdcDowngradeError> {
    Ok(match mutation_operation {
        ndc_models::MutationOperation::Procedure {
            name,
            arguments,
            fields,
        } => ndc_models_v01::MutationOperation::Procedure {
            name: name.into_inner().into(),
            arguments: arguments
                .into_iter()
                .map(|(name, arg)| (name.into_inner().into(), arg))
                .collect(),
            fields: fields.map(downgrade_v02_nested_field).transpose()?,
        },
    })
}
