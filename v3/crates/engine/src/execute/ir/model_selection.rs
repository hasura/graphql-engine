//! IR for the 'model_selection' type - selecting fields from a model

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast;
use ndc_models;
use open_dds::types::CustomTypeName;
use serde::Serialize;
use std::collections::BTreeMap;

use super::filter::ResolvedFilterExpression;
use super::order_by::ResolvedOrderBy;
use super::permissions;
use super::selection_set;
use crate::execute::error;
use crate::execute::model_tracking::UsagesCounts;
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph::Qualified;
use crate::schema::GDS;

/// IR fragment for any 'select' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelection<'s> {
    // The data connector backing this model.
    pub data_connector: &'s resolved::data_connector::DataConnectorLink,

    // Source collection in the data connector for this model
    pub(crate) collection: &'s String,

    // Arguments for the NDC collection
    pub(crate) arguments: BTreeMap<String, ndc_models::Argument>,

    // The boolean expression that would fetch a single row from this model
    pub(crate) filter_clause: ResolvedFilterExpression<'s>,

    // Limit
    pub(crate) limit: Option<u32>,

    // Offset
    pub(crate) offset: Option<u32>,

    // Order by
    pub(crate) order_by: Option<ResolvedOrderBy<'s>>,

    // Fields requested from the model
    pub(crate) selection: selection_set::ResultSelectionSet<'s>,
}

/// Generates the IR fragment for selecting from a model.
#[allow(clippy::too_many_arguments)]
pub(crate) fn model_selection_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_type: &Qualified<CustomTypeName>,
    model_source: &'s resolved::model::ModelSource,
    arguments: BTreeMap<String, ndc_models::Argument>,
    mut filter_clauses: ResolvedFilterExpression<'s>,
    permissions_predicate: &'s resolved::model::FilterPermission,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    match permissions_predicate {
        resolved::model::FilterPermission::AllowAll => {}
        resolved::model::FilterPermission::Filter(predicate) => {
            let permissions_predicate_relationship_paths = Vec::new();
            let mut permissions_predicate_relationships = BTreeMap::new();
            let processed_model_perdicate = permissions::process_model_predicate(
                predicate,
                session_variables,
                permissions_predicate_relationship_paths,
                &mut permissions_predicate_relationships,
                usage_counts,
            )?;
            filter_clauses.expression = match filter_clauses.expression {
                Some(existing) => Some(ndc_models::Expression::And {
                    expressions: vec![existing, processed_model_perdicate],
                }),
                None => Some(processed_model_perdicate),
            };
            for (rel_name, rel_info) in permissions_predicate_relationships {
                filter_clauses.relationships.insert(rel_name, rel_info);
            }
        }
    };
    let field_mappings = model_source
        .type_mappings
        .get(data_type)
        .map(|type_mapping| {
            let resolved::types::TypeMapping::Object { field_mappings, .. } = type_mapping;
            field_mappings
        })
        .ok_or_else(|| error::InternalEngineError::InternalGeneric {
            description: format!("type '{data_type}' not found in model source type_mappings"),
        })?;
    let selection = selection_set::generate_selection_set_ir(
        selection_set,
        &model_source.data_connector,
        &model_source.type_mappings,
        field_mappings,
        session_variables,
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
        selection,
    })
}
