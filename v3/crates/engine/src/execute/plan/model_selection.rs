//! NDC query generation from 'ModelSelection' IR

use std::collections::BTreeMap;

use ndc_models;

use super::relationships;
use super::selection_set;
use crate::execute::error;
use crate::execute::ir::model_selection::ModelSelection;
use crate::execute::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};

pub(crate) fn ndc_query<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc_models::Query, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let (ndc_fields, join_locations) =
        selection_set::process_selection_set_ir(&ir.selection, join_id_counter)?;
    let ndc_query = ndc_models::Query {
        aggregates: None,
        fields: Some(ndc_fields),
        limit: ir.limit,
        offset: ir.offset,
        order_by: ir.order_by.as_ref().map(|x| x.order_by.clone()),
        predicate: ir.filter_clause.expression.clone(),
    };
    Ok((ndc_query, join_locations))
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
        collection: ir.collection.clone(),
        arguments: ir.arguments.clone(),
        collection_relationships,
        variables: None,
    };
    Ok((query_request, join_locations))
}
