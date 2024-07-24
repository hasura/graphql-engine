//! NDC query generation from 'ModelSelection' IR

use std::collections::BTreeMap;

use super::arguments;
use super::error;
use super::filter;
use super::relationships;
use super::selection_set;
use super::types;
use crate::ir::model_selection::ModelSelection;
use crate::ir::selection_set::NdcRelationshipName;
use crate::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};

/// Create an NDC `Query` based on the internal IR `ModelSelection` settings
// #[async_recursion]
pub(crate) fn plan_query_node<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
    relationships: &mut BTreeMap<NdcRelationshipName, types::Relationship>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        types::UnresolvedQueryNode<'s>,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut query_fields = None;
    let mut join_locations = JoinLocations::new();
    if let Some(selection) = &ir.selection {
        let (fields, locations) = selection_set::plan_selection_set(
            selection,
            join_id_counter,
            ir.data_connector.capabilities.supported_ndc_version,
            relationships,
        )?;
        query_fields = Some(fields);
        join_locations = locations;
    }

    let predicate = filter::plan_filter_expression(&ir.filter_clause, relationships)?;
    let query_node = types::QueryNode {
        limit: ir.limit,
        offset: ir.offset,
        order_by: ir.order_by.as_ref().map(|o| o.order_by_elements.clone()),
        predicate,
        aggregates: ir.aggregate_selection.clone(),
        fields: query_fields,
    };

    Ok((query_node, join_locations))
}

/// Generate query execution plan from internal IR (`ModelSelection`)
pub(crate) fn plan_query_execution<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        types::UnresolvedQueryExecutionPlan<'s>,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut collection_relationships = BTreeMap::new();
    relationships::collect_relationships(ir, &mut collection_relationships)?;

    let (query, join_locations) =
        plan_query_node(ir, &mut collection_relationships, join_id_counter)?;
    let execution_node = types::UnresolvedQueryExecutionPlan {
        query_node: query,
        collection: ir.collection.clone(),
        arguments: arguments::plan_arguments(&ir.arguments, &mut collection_relationships)?,
        collection_relationships,
        variables: None,
        data_connector: ir.data_connector,
    };
    Ok((execution_node, join_locations))
}
