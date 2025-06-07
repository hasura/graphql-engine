//! IR for the 'model_selection' type - selecting fields from a model
use super::{filter, order_by, selection_set};
use crate::{error, flags::GraphqlIrFlags};
use graphql_schema::GDS;
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::normalized_ast;
use metadata_resolve::{ObjectTypeWithRelationships, Qualified};
use open_dds::{
    data_connector::CollectionName,
    models::ModelName,
    types::{CustomTypeName, DataConnectorArgumentName},
};
use plan::UnresolvedArgument;
use plan::count_model;
use plan_types::{UsagesCounts, VariableName};
use serde::Serialize;
use std::collections::BTreeMap;
use std::sync::Arc;

/// IR fragment for any 'select' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelection<'s> {
    // The data connector backing this model.
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,

    // Source collection in the data connector for this model
    pub collection: &'s CollectionName,

    // Arguments for the NDC collection
    pub arguments: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>,

    // The boolean expression that would fetch a single row from this model
    pub filter_clause: filter::FilterExpression<'s>,

    // Limit
    pub limit: Option<u32>,

    // Offset
    pub offset: Option<u32>,

    // Order by
    pub order_by: Option<order_by::OrderBy<'s>>,

    // Fields requested from the model
    pub selection: Option<selection_set::ResultSelectionSet<'s>>,

    // Aggregates requested of the model
    pub aggregate_selection: Option<plan_types::AggregateSelectionSet>,

    /// Variable arguments to be used for remote joins
    pub variable_arguments: BTreeMap<DataConnectorArgumentName, VariableName>,
}

/// Generates the IR fragment for selecting from a model.
#[allow(clippy::too_many_arguments)]
pub fn model_selection_open_dd_ir(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    model_name: &Qualified<ModelName>,
    models: &IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    type_mappings: &BTreeMap<
        metadata_resolve::Qualified<CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    model_arguments: Option<IndexMap<open_dds::query::ArgumentName, open_dds::query::Value>>,
    where_clause: Option<open_dds::query::BooleanExpression>,
    order_by: Vec<open_dds::query::OrderByElement>,
    limit: Option<usize>,
    offset: Option<usize>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::ModelSelection, error::Error> {
    let selection = selection_set::generate_selection_set_open_dd_ir(
        selection_set,
        metadata_resolve::FieldNestedness::NotNested,
        models,
        type_mappings,
        object_types,
        session_variables,
        request_headers,
        flags,
        usage_counts,
    )?;

    let filter = where_clause;

    let target = open_dds::query::ModelTarget {
        subgraph: model_name.subgraph.clone(),
        model_name: model_name.name.clone(),
        offset,
        order_by,
        arguments: model_arguments.unwrap_or_default(), // Permission presets are handled during planning
        filter,
        limit,
    };

    Ok(open_dds::query::ModelSelection { selection, target })
}

/// Generates the IR fragment for selecting from a model.
#[allow(clippy::too_many_arguments)]
pub fn model_aggregate_selection_open_dd_ir(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    model_name: &Qualified<ModelName>,
    model_arguments: Option<IndexMap<open_dds::query::ArgumentName, open_dds::query::Value>>,
    where_clause: Option<open_dds::query::BooleanExpression>,
    order_by: Vec<open_dds::query::OrderByElement>,
    limit: Option<usize>,
    offset: Option<usize>,
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::ModelAggregateSelection, error::Error> {
    count_model(model_name, usage_counts);
    let selection = selection_set::generate_aggregate_selection_set_open_dd_ir(selection_set)?;

    let filter = where_clause;

    let target = open_dds::query::ModelTarget {
        subgraph: model_name.subgraph.clone(),
        model_name: model_name.name.clone(),
        offset,
        order_by,
        arguments: model_arguments.unwrap_or_default(), // Permission presets are handled during planning
        filter,
        limit,
    };

    Ok(open_dds::query::ModelAggregateSelection { selection, target })
}
