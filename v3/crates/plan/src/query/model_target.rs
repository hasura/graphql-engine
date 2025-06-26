use super::arguments::{
    add_missing_nullable_arguments, get_unresolved_arguments, resolve_arguments,
};
use super::process_argument_presets_for_model;
use super::types::NDCQuery;
use crate::filter::{resolve_model_permission_filter, to_filter_expression};
use crate::metadata_accessor::OutputObjectTypeView;
use crate::order_by::to_resolved_order_by_element;
use crate::types::{PlanError, PlanState};
use hasura_authn_core::Session;
use open_dds::query::ModelTarget;
use plan_types::{PredicateQueryTrees, Relationship, ResolvedFilterExpression};
use std::collections::BTreeMap;

pub fn model_target_to_ndc_query(
    model_target: &ModelTarget,
    session: &Session,
    metadata: &metadata_resolve::Metadata,
    request_headers: &reqwest::header::HeaderMap,
    // The following are things we could compute, but we have them on hand
    // at all call sites anyway:
    model: &metadata_resolve::ModelWithPermissions,
    model_source: &metadata_resolve::ModelSource,
    model_object_type: &OutputObjectTypeView,
    remote_predicates: &mut PredicateQueryTrees,
    plan_state: &mut PlanState,
) -> Result<NDCQuery, PlanError> {
    let mut usage_counts = plan_types::UsagesCounts::default();
    let mut relationships: BTreeMap<plan_types::NdcRelationshipName, Relationship> =
        BTreeMap::new();

    // Permission filter
    let permission_filter = resolve_model_permission_filter(
        session,
        model,
        model_source,
        &metadata.object_types,
        &mut relationships,
        remote_predicates,
        plan_state,
        &mut usage_counts,
    )?;

    let unresolved_arguments = get_unresolved_arguments(
        &model_target.arguments,
        &model.arguments,
        &model_source.argument_mappings,
        metadata,
        session,
        &model_source.type_mappings,
        &model_source.data_connector,
        plan_state,
        &mut usage_counts,
    )?;

    // add any preset arguments from model permissions
    let unresolved_arguments = process_argument_presets_for_model(
        unresolved_arguments,
        model,
        &metadata.object_types,
        session,
        request_headers,
        &mut usage_counts,
    )?;

    // add in any missing arguments as nulls
    let unresolved_arguments = add_missing_nullable_arguments(
        unresolved_arguments,
        &model.arguments,
        &model_source.argument_mappings,
        &metadata.runtime_flags,
    )?;

    let resolved_arguments = resolve_arguments(
        unresolved_arguments,
        &mut relationships,
        remote_predicates,
        plan_state,
    )?;

    let model_filter = match &model_target.filter {
        Some(expr) => {
            let expression = to_filter_expression(
                metadata,
                session,
                &model_source.type_mappings,
                model_object_type,
                model
                    .filter_expression_type
                    .as_ref()
                    .map(std::convert::AsRef::as_ref),
                expr,
                &model_source.data_connector,
                plan_state,
                &mut usage_counts,
            )?;

            let resolved_filter_expression = crate::plan_expression(
                &expression,
                &mut relationships,
                remote_predicates,
                plan_state,
            )?;

            resolved_filter_expression.remove_always_true_expression()
        }
        _ => None,
    };

    let filter = match (model_filter, permission_filter) {
        (None, filter) | (filter, None) => filter,
        (Some(filter), Some(permission_filter)) => Some(ResolvedFilterExpression::mk_and(vec![
            permission_filter,
            filter,
        ])),
    }
    .and_then(ResolvedFilterExpression::remove_always_true_expression);

    let order_by_elements = model_target
        .order_by
        .iter()
        .map(|element| {
            to_resolved_order_by_element(
                metadata,
                session,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                &model_source.data_connector,
                element,
                &mut relationships,
                remote_predicates,
                plan_state,
                &mut usage_counts,
            )
        })
        .collect::<Result<Vec<_>, PlanError>>()?;

    let limit = model_target
        .limit
        .map(u32::try_from)
        .transpose()
        .map_err(|_| PlanError::Internal("limit out of range".into()))?;

    let offset: Option<u32> = model_target
        .offset
        .map(u32::try_from)
        .transpose()
        .map_err(|_| PlanError::Internal("offset out of range".into()))?;

    let query = NDCQuery {
        arguments: resolved_arguments,
        collection_name: model_source.collection.clone(),
        collection_relationships: relationships,
        data_connector: model_source.data_connector.clone(),
        filter,
        limit,
        offset,
        order_by: order_by_elements,
    };

    Ok(query)
}
