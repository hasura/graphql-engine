//! IR for the 'model_selection' type - selecting fields from a model

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast;
use metadata_resolve::QualifiedTypeName;
use ndc_models;
use open_dds::types::CustomTypeName;
use serde::Serialize;
use std::collections::BTreeMap;

use super::aggregates;
use super::filter::ResolvedFilterExpression;
use super::order_by::ResolvedOrderBy;
use super::permissions;
use super::selection_set;
use crate::ir::error;
use crate::model_tracking::UsagesCounts;
use metadata_resolve;
use metadata_resolve::{ConnectorArgumentName, Qualified};
use schema::GDS;

/// IR fragment for any 'select' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelection<'s> {
    // The data connector backing this model.
    pub data_connector: &'s metadata_resolve::DataConnectorLink,

    // Source collection in the data connector for this model
    pub(crate) collection: &'s String,

    // Arguments for the NDC collection
    pub(crate) arguments: BTreeMap<ConnectorArgumentName, ndc_models::Argument>,

    // The boolean expression that would fetch a single row from this model
    pub(crate) filter_clause: ResolvedFilterExpression<'s>,

    // Limit
    pub(crate) limit: Option<u32>,

    // Offset
    pub(crate) offset: Option<u32>,

    // Order by
    pub(crate) order_by: Option<ResolvedOrderBy<'s>>,

    // Fields requested from the model
    pub(crate) selection: Option<selection_set::ResultSelectionSet<'s>>,

    // Aggregates requested of the model
    pub(crate) aggregate_selection: Option<aggregates::AggregateSelectionSet<'s>>,
}

/// Generates the IR fragment for selecting from a model.
#[allow(clippy::too_many_arguments)]
pub(crate) fn model_selection_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_type: &Qualified<CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    arguments: BTreeMap<ConnectorArgumentName, ndc_models::Argument>,
    filter_clauses: ResolvedFilterExpression<'s>,
    permissions_predicate: &'s metadata_resolve::FilterPermission,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    let filter_clauses = apply_permissions_predicate(
        filter_clauses,
        permissions_predicate,
        session_variables,
        usage_counts,
    )?;

    let field_mappings = get_field_mappings_for_object_type(model_source, data_type)?;
    let selection = selection_set::generate_selection_set_ir(
        selection_set,
        &model_source.data_connector,
        &model_source.type_mappings,
        field_mappings,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    Ok(ModelSelection {
        data_connector: &model_source.data_connector,
        collection: &model_source.collection,
        arguments,
        filter_clause: filter_clauses,
        limit,
        offset,
        order_by,
        selection: Some(selection),
        aggregate_selection: None,
    })
}

fn apply_permissions_predicate<'s>(
    mut filter_clauses: ResolvedFilterExpression<'s>,
    permissions_predicate: &'s metadata_resolve::FilterPermission,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<ResolvedFilterExpression<'s>, error::Error> {
    match permissions_predicate {
        metadata_resolve::FilterPermission::AllowAll => {}
        metadata_resolve::FilterPermission::Filter(predicate) => {
            let mut permissions_predicate_relationships = BTreeMap::new();
            let processed_model_predicate = permissions::process_model_predicate(
                predicate,
                session_variables,
                &mut permissions_predicate_relationships,
                usage_counts,
            )?;
            filter_clauses.expression = match filter_clauses.expression {
                Some(existing) => Some(ndc_models::Expression::And {
                    expressions: vec![existing, processed_model_predicate],
                }),
                None => Some(processed_model_predicate),
            };
            for (rel_name, rel_info) in permissions_predicate_relationships {
                filter_clauses.relationships.insert(rel_name, rel_info);
            }
        }
    };

    Ok(filter_clauses)
}

/// Generates the IR fragment for selecting an aggregate of a model.
#[allow(clippy::too_many_arguments)]
pub(crate) fn model_aggregate_selection_ir<'s>(
    aggregate_selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_type: &Qualified<CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    arguments: BTreeMap<ConnectorArgumentName, ndc_models::Argument>,
    filter_clauses: ResolvedFilterExpression<'s>,
    permissions_predicate: &'s metadata_resolve::FilterPermission,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    let filter_clauses = apply_permissions_predicate(
        filter_clauses,
        permissions_predicate,
        session_variables,
        usage_counts,
    )?;

    let field_mappings = get_field_mappings_for_object_type(model_source, data_type)?;
    let aggregate_selection = aggregates::generate_aggregate_selection_set_ir(
        aggregate_selection_set,
        &model_source.data_connector,
        &model_source.type_mappings,
        field_mappings,
        &QualifiedTypeName::Custom(data_type.clone()),
    )?;

    Ok(ModelSelection {
        data_connector: &model_source.data_connector,
        collection: &model_source.collection,
        arguments,
        filter_clause: filter_clauses,
        limit,
        offset,
        order_by,
        selection: None,
        aggregate_selection: Some(aggregate_selection),
    })
}

fn get_field_mappings_for_object_type<'s>(
    model_source: &'s metadata_resolve::ModelSource,
    data_type: &Qualified<CustomTypeName>,
) -> Result<&'s BTreeMap<open_dds::types::FieldName, metadata_resolve::FieldMapping>, error::Error>
{
    model_source
        .type_mappings
        .get(data_type)
        .map(|type_mapping| {
            let metadata_resolve::TypeMapping::Object { field_mappings, .. } = type_mapping;
            field_mappings
        })
        .ok_or_else(|| {
            error::InternalEngineError::InternalGeneric {
                description: format!("type '{data_type}' not found in model source type_mappings"),
            }
            .into()
        })
}
