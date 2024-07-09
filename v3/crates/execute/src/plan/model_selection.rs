//! NDC query generation from 'ModelSelection' IR

use std::collections::BTreeMap;

use indexmap::IndexMap;

use super::common;
use super::error;
use super::relationships;
use super::selection_set;
use crate::ir::aggregates::{AggregateFieldSelection, AggregateSelectionSet};
use crate::ir::model_selection::ModelSelection;
use crate::ir::order_by;
use crate::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};

/// Create an NDC `Query` based on the internal IR `ModelSelection` settings
pub(crate) fn ndc_query<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc_models::Query, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let (ndc_fields, join_locations) = ir
        .selection
        .as_ref()
        .map(|selection| -> Result<_, error::Error> {
            let (ndc_fields, join_locations) =
                selection_set::process_selection_set_ir(selection, join_id_counter)?;
            Ok((Some(ndc_fields), join_locations))
        })
        .transpose()?
        .unwrap_or_else(|| (None, JoinLocations::new()));

    let aggregates = ir.aggregate_selection.as_ref().map(ndc_aggregates);

    let ndc_query = ndc_models::Query {
        aggregates,
        fields: ndc_fields,
        limit: ir.limit,
        offset: ir.offset,
        order_by: ir
            .order_by
            .as_ref()
            .map(|x| ndc_order_by(&x.order_by_elements)),
        predicate: ir
            .filter_clause
            .expression
            .as_ref()
            .map(common::ndc_expression),
    };

    Ok((ndc_query, join_locations))
}

/// Translates the internal IR 'AggregateSelectionSet' into an NDC query aggregates selection
fn ndc_aggregates(
    aggregate_selection_set: &AggregateSelectionSet,
) -> IndexMap<ndc_models::FieldName, ndc_models::Aggregate> {
    aggregate_selection_set
        .fields
        .iter()
        .map(|(field_name, aggregate_selection)| {
            let aggregate = match aggregate_selection {
                AggregateFieldSelection::Count { column_path, .. } => {
                    ndc_count_aggregate(column_path, false)
                }
                AggregateFieldSelection::CountDistinct { column_path, .. } => {
                    ndc_count_aggregate(column_path, true)
                }
                AggregateFieldSelection::AggregationFunction {
                    function_name,
                    column_path,
                    ..
                } => {
                    let nonempty::NonEmpty {
                        head: column,
                        tail: field_path,
                    } = column_path;
                    let nested_field_path = field_path
                        .iter()
                        .map(|p| ndc_models::FieldName::from(*p))
                        .collect::<Vec<_>>();
                    ndc_models::Aggregate::SingleColumn {
                        column: ndc_models::FieldName::from(*column),
                        field_path: if nested_field_path.is_empty() {
                            None
                        } else {
                            Some(nested_field_path)
                        },
                        function: ndc_models::AggregateFunctionName::from(function_name.0.as_str()),
                    }
                }
            };
            (ndc_models::FieldName::from(field_name.as_str()), aggregate)
        })
        .collect()
}

/// Creates the appropriate NDC count aggregation based on whether we're selecting
/// a column (nested or otherwise) or not
fn ndc_count_aggregate(column_path: &[&str], distinct: bool) -> ndc_models::Aggregate {
    let mut column_path_iter = column_path.iter();
    if let Some(first_path_element) = column_path_iter.next() {
        let remaining_path = column_path_iter
            .map(|p| ndc_models::FieldName::from(*p))
            .collect::<Vec<_>>();
        let nested_field_path = if remaining_path.is_empty() {
            None
        } else {
            Some(remaining_path)
        };
        ndc_models::Aggregate::ColumnCount {
            column: ndc_models::FieldName::from(*first_path_element),
            field_path: nested_field_path,
            distinct,
        }
    } else {
        ndc_models::Aggregate::StarCount {}
    }
}

/// Convert the internal IR (`ModelSelection`) into NDC IR (`ndc::models::QueryRequest`)
pub(crate) fn ndc_ir<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc_models::QueryRequest, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let mut collection_relationships = BTreeMap::new();
    relationships::collect_relationships(ir, &mut collection_relationships)?;

    let (query, join_locations) = ndc_query(ir, join_id_counter)?;
    let query_request = ndc_models::QueryRequest {
        query,
        collection: ndc_models::CollectionName::from(ir.collection.as_str()),
        arguments: common::ndc_arguments(&ir.arguments)?,
        collection_relationships,
        variables: None,
    };
    Ok((query_request, join_locations))
}

fn ndc_order_by(order_by_elements: &[order_by::OrderByElement]) -> ndc_models::OrderBy {
    ndc_models::OrderBy {
        elements: order_by_elements
            .iter()
            .map(|element| ndc_models::OrderByElement {
                order_direction: match element.order_direction {
                    schema::ModelOrderByDirection::Asc => ndc_models::OrderDirection::Asc,
                    schema::ModelOrderByDirection::Desc => ndc_models::OrderDirection::Desc,
                },
                target: ndc_order_by_target(&element.target),
            })
            .collect(),
    }
}

fn ndc_order_by_target(target: &order_by::OrderByTarget) -> ndc_models::OrderByTarget {
    match target {
        order_by::OrderByTarget::Column {
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
                order_by_element_path.push(ndc_models::PathElement {
                    relationship: ndc_models::RelationshipName::from(path.0.as_str()),
                    arguments: BTreeMap::new(),
                    // 'AND' predicate indicates that the column can be accessed
                    // by joining all the relationships paths provided
                    predicate: Some(Box::new(ndc_models::Expression::And {
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

            ndc_models::OrderByTarget::Column {
                name: ndc_models::FieldName::from(name.as_str()),
                path: order_by_element_path,
                field_path: None,
            }
        }
    }
}
