//! NDC query generation from 'ModelSelection' IR

use super::arguments;
use super::error;
use super::filter;
use super::order_by;
use super::relationships;
use super::selection_set;
use crate::plan::Plan;
use crate::ModelSelection;
use plan_types::{
    ExecutionTree, FieldsSelection, JoinLocations, NdcRelationshipName, PredicateQueryTrees,
    QueryExecutionPlan, QueryNodeNew, Relationship, UniqueNumber,
};
use std::collections::BTreeMap;

/// Create an NDC `Query` based on the internal IR `ModelSelection` settings
// #[async_recursion]
pub(crate) fn plan_query_node(
    ir: &ModelSelection<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<QueryNodeNew>, error::Error> {
    let mut query_fields = None;
    let mut join_locations = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();
    if let Some(selection) = &ir.selection {
        let Plan {
            inner: fields,
            join_locations: locations,
            remote_predicates: selection_set_remote_predicates,
        } = selection_set::plan_selection_set(
            selection,
            ir.data_connector.capabilities.supported_ndc_version,
            relationships,
            unique_number,
        )?;

        query_fields = Some(fields);
        join_locations = locations;
        remote_predicates = selection_set_remote_predicates;
    }

    let (predicate, filter_remote_predicates) =
        filter::plan_filter_expression(&ir.filter_clause, relationships, unique_number)?;

    remote_predicates.0.extend(filter_remote_predicates.0);

    let (order_by, order_by_remote_predicates) = ir
        .order_by
        .as_ref()
        .map(|order_by| order_by::plan_order_by(order_by, relationships, unique_number))
        .transpose()?
        .map_or_else(
            || (None, PredicateQueryTrees::new()),
            |(order_by, predicate)| (Some(order_by), predicate),
        );

    remote_predicates.0.extend(order_by_remote_predicates.0);

    let query_node = QueryNodeNew {
        limit: ir.limit,
        offset: ir.offset,
        order_by,
        predicate,
        aggregates: ir.aggregate_selection.clone(),
        fields: query_fields.map(|fields| FieldsSelection { fields }),
    };

    Ok(Plan {
        inner: query_node,
        join_locations,
        remote_predicates,
    })
}

/// Generate query execution plan from internal IR (`ModelSelection`)
pub(crate) fn plan_query_execution(
    ir: &ModelSelection<'_>,
    unique_number: &mut UniqueNumber,
) -> Result<ExecutionTree, error::Error> {
    let mut collection_relationships = BTreeMap::new();
    let Plan {
        inner: query,
        join_locations: remote_join_executions,
        mut remote_predicates,
    } = plan_query_node(ir, &mut collection_relationships, unique_number)?;

    // collection relationships from order_by clause
    relationships::collect_relationships_from_order_by(ir, &mut collection_relationships)?;

    let (arguments, argument_remote_predicates) =
        arguments::plan_arguments(&ir.arguments, &mut collection_relationships, unique_number)?;

    remote_predicates.0.extend(argument_remote_predicates.0);

    let query_execution_plan = QueryExecutionPlan {
        query_node: query,
        collection: ir.collection.clone(),
        arguments,
        collection_relationships,
        variables: None,
        data_connector: ir.data_connector.clone(),
    };
    Ok(ExecutionTree {
        query_execution_plan,
        remote_join_executions,
        remote_predicates,
    })
}
