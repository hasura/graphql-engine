//! NDC query generation from 'ModelSelection' IR

use std::collections::BTreeMap;

use indexmap::IndexMap;

use super::relationships;
use super::selection_set;
use crate::ir::aggregates::{AggregateFieldSelection, AggregateSelectionSet};
use crate::ir::model_selection::ModelSelection;
use crate::plan::error;
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
        order_by: ir.order_by.as_ref().map(|x| x.order_by.clone()),
        predicate: ir.filter_clause.expression.clone(),
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
        arguments: ir
            .arguments
            .iter()
            .map(|(k, v)| (ndc_models::ArgumentName::from(k.0.as_str()), v.clone()))
            .collect(),
        collection_relationships,
        variables: None,
    };
    Ok((query_request, join_locations))
}
