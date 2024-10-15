//! NDC query generation from 'ModelSelection' IR

use std::collections::BTreeMap;

use super::arguments;
use super::error;
use super::filter;
use super::query;
use super::relationships;
use super::selection_set;
use crate::remote_joins::types::JoinLocations;
use graphql_ir::ModelSelection;
use plan_types::NdcRelationshipName;

/// Create an NDC `Query` based on the internal IR `ModelSelection` settings
// #[async_recursion]
pub(crate) fn plan_query_node<'s>(
    ir: &ModelSelection<'s>,
    relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
) -> Result<(query::UnresolvedQueryNode<'s>, JoinLocations<'s>), error::Error> {
    let mut query_fields = None;
    let mut join_locations = JoinLocations::new();
    if let Some(selection) = &ir.selection {
        let (fields, locations) = selection_set::plan_selection_set(
            selection,
            ir.data_connector.capabilities.supported_ndc_version,
            relationships,
        )?;
        query_fields = Some(fields);
        join_locations = locations;
    }

    let predicate = filter::plan_filter_expression(&ir.filter_clause, relationships)?;
    let query_node = query::QueryNode {
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
pub(crate) fn plan_query_execution<'s>(
    ir: &ModelSelection<'s>,
) -> Result<(query::UnresolvedQueryExecutionPlan<'s>, JoinLocations<'s>), error::Error> {
    let mut collection_relationships = BTreeMap::new();
    let (query, join_locations) = plan_query_node(ir, &mut collection_relationships)?;
    // collection relationships from order_by clause
    relationships::collect_relationships_from_order_by(ir, &mut collection_relationships)?;
    let execution_node = query::UnresolvedQueryExecutionPlan {
        query_node: query,
        collection: ir.collection.clone(),
        arguments: arguments::plan_arguments(&ir.arguments, &mut collection_relationships)?,
        collection_relationships,
        variables: None,
        data_connector: ir.data_connector.clone(),
    };
    Ok((execution_node, join_locations))
}
