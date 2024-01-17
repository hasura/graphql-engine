//! IR for the 'model_selection' type - selecting fields from a model

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast;
use ndc_client as ndc;
use open_dds::types::CustomTypeName;
use serde::Serialize;
use std::collections::BTreeMap;

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
    pub data_connector: &'s resolved::data_connector::DataConnector,

    // Source collection in the data connector for this model
    pub(crate) collection: &'s String,

    // Arguments for the NDC collection
    pub(crate) arguments: BTreeMap<String, ndc::models::Argument>,

    // The boolean expression that would fetch a single row from this model
    pub(crate) filter_clause: Vec<ndc::models::Expression>,

    // Limit
    pub(crate) limit: Option<u32>,

    // Offset
    pub(crate) offset: Option<u32>,

    // Order by
    pub(crate) order_by: Option<ndc::models::OrderBy>,

    // Fields requested from the model
    pub(crate) selection: selection_set::ResultSelectionSet<'s>,
}

/// Generates the IR fragment for selecting from a model.
#[allow(clippy::too_many_arguments)]
pub(crate) fn model_selection_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_type: &Qualified<CustomTypeName>,
    model_source: &'s resolved::model::ModelSource,
    arguments: BTreeMap<String, ndc::models::Argument>,
    mut filter_clauses: Vec<ndc::models::Expression>,
    permissions_predicate: &resolved::model::FilterPermission,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ndc::models::OrderBy>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    match permissions_predicate {
        resolved::model::FilterPermission::AllowAll => {}
        resolved::model::FilterPermission::Filter(predicate) => {
            filter_clauses.push(permissions::process_model_predicate(
                predicate,
                session_variables,
            )?);
        }
    };
    let field_mappings = model_source
        .type_mappings
        .get(data_type)
        .map(|type_mapping| {
            let resolved::types::TypeMapping::Object { field_mappings } = type_mapping;
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
