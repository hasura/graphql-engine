use ndc_models;
use ndc_models_v01;

use super::NdcDowngradeError;

pub fn upgrade_rowset_to_v02(rowset: ndc_models_v01::RowSet) -> ndc_models::RowSet {
    ndc_models::RowSet {
        aggregates: rowset.aggregates,
        rows: rowset.rows.map(|rows| {
            rows.into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|(k, ndc_models_v01::RowFieldValue(v))| {
                            (k, ndc_models::RowFieldValue(v))
                        })
                        .collect()
                })
                .collect()
        }),
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
            .map(|(name, argument)| (name, downgrade_v02_argument(argument)))
            .collect(),
        collection: query_request.collection,
        query: downgrade_v02_query(query_request.query),
        collection_relationships: query_request
            .collection_relationships
            .into_iter()
            .map(|(name, relationship)| (name, downgrade_v02_relationship(relationship)))
            .collect(),
        variables: query_request.variables,
    })
}

fn downgrade_v02_argument(argument: ndc_models::Argument) -> ndc_models_v01::Argument {
    match argument {
        ndc_models::Argument::Variable { name } => ndc_models_v01::Argument::Variable { name },
        ndc_models::Argument::Literal { value } => ndc_models_v01::Argument::Literal { value },
    }
}

fn downgrade_v02_query(query: ndc_models::Query) -> ndc_models_v01::Query {
    ndc_models_v01::Query {
        aggregates: query.aggregates.map(|aggregates| {
            aggregates
                .into_iter()
                .map(|(name, aggregate)| (name, downgrade_v02_aggregate(aggregate)))
                .collect()
        }),
        fields: query.fields.map(|fields| {
            fields
                .into_iter()
                .map(|(name, field)| (name, downgrade_v02_field(field)))
                .collect()
        }),
        limit: query.limit,
        offset: query.offset,
        order_by: query.order_by.map(downgrade_v02_order_by),
        predicate: query.predicate.map(downgrade_v02_predicate),
    }
}

fn downgrade_v02_aggregate(aggregate: ndc_models::Aggregate) -> ndc_models_v01::Aggregate {
    match aggregate {
        ndc_models::Aggregate::ColumnCount {
            column,
            field_path,
            distinct,
        } => ndc_models_v01::Aggregate::ColumnCount {
            column,
            field_path,
            distinct,
        },
        ndc_models::Aggregate::SingleColumn {
            column,
            field_path,
            function,
        } => ndc_models_v01::Aggregate::SingleColumn {
            column,
            field_path,
            function,
        },
        ndc_models::Aggregate::StarCount {} => ndc_models_v01::Aggregate::StarCount {},
    }
}

fn downgrade_v02_field(field: ndc_models::Field) -> ndc_models_v01::Field {
    match field {
        ndc_models::Field::Column {
            column,
            fields,
            arguments,
        } => ndc_models_v01::Field::Column {
            column,
            fields: fields.map(downgrade_v02_nested_field),
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| (name, downgrade_v02_argument(argument)))
                .collect(),
        },
        ndc_models::Field::Relationship {
            query,
            relationship,
            arguments,
        } => ndc_models_v01::Field::Relationship {
            query: Box::new(downgrade_v02_query(*query)),
            relationship,
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| (name, downgrade_v02_relationship_argument(argument)))
                .collect(),
        },
    }
}

fn downgrade_v02_nested_field(
    nested_field: ndc_models::NestedField,
) -> ndc_models_v01::NestedField {
    match nested_field {
        ndc_models::NestedField::Object(nested_object) => {
            ndc_models_v01::NestedField::Object(downgrade_v02_nested_object(nested_object))
        }
        ndc_models::NestedField::Array(nested_array) => {
            ndc_models_v01::NestedField::Array(downgrade_v02_nested_array(nested_array))
        }
    }
}

fn downgrade_v02_nested_object(
    nested_object: ndc_models::NestedObject,
) -> ndc_models_v01::NestedObject {
    ndc_models_v01::NestedObject {
        fields: nested_object
            .fields
            .into_iter()
            .map(|(name, field)| (name, downgrade_v02_field(field)))
            .collect(),
    }
}

fn downgrade_v02_nested_array(
    nested_array: ndc_models::NestedArray,
) -> ndc_models_v01::NestedArray {
    ndc_models_v01::NestedArray {
        fields: Box::new(downgrade_v02_nested_field(*nested_array.fields)),
    }
}

fn downgrade_v02_relationship_argument(
    relationship_argument: ndc_models::RelationshipArgument,
) -> ndc_models_v01::RelationshipArgument {
    match relationship_argument {
        ndc_models::RelationshipArgument::Variable { name } => {
            ndc_models_v01::RelationshipArgument::Variable { name }
        }
        ndc_models::RelationshipArgument::Literal { value } => {
            ndc_models_v01::RelationshipArgument::Literal { value }
        }
        ndc_models::RelationshipArgument::Column { name } => {
            ndc_models_v01::RelationshipArgument::Column { name }
        }
    }
}

fn downgrade_v02_order_by(order_by: ndc_models::OrderBy) -> ndc_models_v01::OrderBy {
    ndc_models_v01::OrderBy {
        elements: order_by
            .elements
            .into_iter()
            .map(downgrade_v02_order_by_element)
            .collect(),
    }
}

fn downgrade_v02_order_by_element(
    order_by_element: ndc_models::OrderByElement,
) -> ndc_models_v01::OrderByElement {
    ndc_models_v01::OrderByElement {
        order_direction: downgrade_v02_order_direction(order_by_element.order_direction),
        target: downgrade_v02_order_by_target(order_by_element.target),
    }
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
) -> ndc_models_v01::OrderByTarget {
    match target {
        ndc_models::OrderByTarget::Column {
            name,
            field_path,
            path,
        } => ndc_models_v01::OrderByTarget::Column {
            name,
            field_path,
            path: path.into_iter().map(downgrade_v02_path_element).collect(),
        },
        ndc_models::OrderByTarget::SingleColumnAggregate {
            column,
            field_path,
            function,
            path,
        } => ndc_models_v01::OrderByTarget::SingleColumnAggregate {
            column,
            field_path,
            function,
            path: path.into_iter().map(downgrade_v02_path_element).collect(),
        },
        ndc_models::OrderByTarget::StarCountAggregate { path } => {
            ndc_models_v01::OrderByTarget::StarCountAggregate {
                path: path.into_iter().map(downgrade_v02_path_element).collect(),
            }
        }
    }
}

fn downgrade_v02_path_element(
    path_element: ndc_models::PathElement,
) -> ndc_models_v01::PathElement {
    ndc_models_v01::PathElement {
        relationship: path_element.relationship,
        arguments: path_element
            .arguments
            .into_iter()
            .map(|(name, argument)| (name, downgrade_v02_relationship_argument(argument)))
            .collect(),
        predicate: path_element
            .predicate
            .map(|predicate| Box::new(downgrade_v02_predicate(*predicate))),
    }
}

fn downgrade_v02_predicate(predicate: ndc_models::Expression) -> ndc_models_v01::Expression {
    match predicate {
        ndc_models::Expression::And { expressions } => ndc_models_v01::Expression::And {
            expressions: expressions
                .into_iter()
                .map(downgrade_v02_predicate)
                .collect(),
        },
        ndc_models::Expression::Or { expressions } => ndc_models_v01::Expression::Or {
            expressions: expressions
                .into_iter()
                .map(downgrade_v02_predicate)
                .collect(),
        },
        ndc_models::Expression::Not { expression } => ndc_models_v01::Expression::Not {
            expression: Box::new(downgrade_v02_predicate(*expression)),
        },
        ndc_models::Expression::UnaryComparisonOperator { column, operator } => {
            ndc_models_v01::Expression::UnaryComparisonOperator {
                column: downgrade_v02_comparison_target(column),
                operator: downgrade_v02_unary_comparison_operator(operator),
            }
        }
        ndc_models::Expression::BinaryComparisonOperator {
            column,
            operator,
            value,
        } => ndc_models_v01::Expression::BinaryComparisonOperator {
            column: downgrade_v02_comparison_target(column),
            operator,
            value: downgrade_v02_binary_comparison_value(value),
        },
        ndc_models::Expression::Exists {
            in_collection,
            predicate,
        } => ndc_models_v01::Expression::Exists {
            in_collection: downgrade_v02_exists_in_collection(in_collection),
            predicate: predicate.map(|predicate| Box::new(downgrade_v02_predicate(*predicate))),
        },
    }
}

fn downgrade_v02_comparison_target(
    column: ndc_models::ComparisonTarget,
) -> ndc_models_v01::ComparisonTarget {
    match column {
        ndc_models::ComparisonTarget::Column {
            name,
            field_path,
            path,
        } => ndc_models_v01::ComparisonTarget::Column {
            name,
            field_path,
            path: path.into_iter().map(downgrade_v02_path_element).collect(),
        },
        ndc_models::ComparisonTarget::RootCollectionColumn { name, field_path } => {
            ndc_models_v01::ComparisonTarget::RootCollectionColumn { name, field_path }
        }
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
) -> ndc_models_v01::ComparisonValue {
    match value {
        ndc_models::ComparisonValue::Column { column } => ndc_models_v01::ComparisonValue::Column {
            column: downgrade_v02_comparison_target(column),
        },
        ndc_models::ComparisonValue::Scalar { value } => {
            ndc_models_v01::ComparisonValue::Scalar { value }
        }
        ndc_models::ComparisonValue::Variable { name } => {
            ndc_models_v01::ComparisonValue::Variable { name }
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
            relationship,
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| (name, downgrade_v02_relationship_argument(argument)))
                .collect(),
        },
        ndc_models::ExistsInCollection::Unrelated {
            collection,
            arguments,
        } => ndc_models_v01::ExistsInCollection::Unrelated {
            collection,
            arguments: arguments
                .into_iter()
                .map(|(name, argument)| (name, downgrade_v02_relationship_argument(argument)))
                .collect(),
        },
    }
}

fn downgrade_v02_relationship(
    relationship: ndc_models::Relationship,
) -> ndc_models_v01::Relationship {
    ndc_models_v01::Relationship {
        column_mapping: relationship.column_mapping,
        relationship_type: downgrade_v02_relationship_type(relationship.relationship_type),
        target_collection: relationship.target_collection,
        arguments: relationship
            .arguments
            .into_iter()
            .map(|(name, argument)| (name, downgrade_v02_relationship_argument(argument)))
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
            .collect(),
        collection_relationships: mutation_request
            .collection_relationships
            .into_iter()
            .map(|(name, relationship)| (name, downgrade_v02_relationship(relationship)))
            .collect(),
    })
}

fn downgrade_v02_mutation_operation(
    mutation_operation: ndc_models::MutationOperation,
) -> ndc_models_v01::MutationOperation {
    match mutation_operation {
        ndc_models::MutationOperation::Procedure {
            name,
            arguments,
            fields,
        } => ndc_models_v01::MutationOperation::Procedure {
            name,
            arguments,
            fields: fields.map(downgrade_v02_nested_field),
        },
    }
}
