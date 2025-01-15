use super::arguments::process_arguments;
use super::filter::resolve_filter_expression;
use super::permissions::process_model_predicate;
use super::types::NDCQuery;
use crate::filter::to_resolved_filter_expr;
use crate::order_by::to_resolved_order_by_element;
use crate::types::PlanError;
use std::collections::BTreeMap;

use hasura_authn_core::Session;
use metadata_resolve::FilterPermission;
use open_dds::query::ModelTarget;
use plan_types::{Relationship, ResolvedFilterExpression, UniqueNumber};

pub fn model_target_to_ndc_query(
    model_target: &ModelTarget,
    session: &Session,
    metadata: &metadata_resolve::Metadata,
    request_headers: &reqwest::header::HeaderMap,
    // The following are things we could compute, but we have them on hand
    // at all call sites anyway:
    model: &metadata_resolve::ModelWithArgumentPresets,
    model_source: &metadata_resolve::ModelSource,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    unique_number: &mut UniqueNumber,
) -> Result<NDCQuery, PlanError> {
    let qualified_model_name = metadata_resolve::Qualified::new(
        model_target.subgraph.clone(),
        model_target.model_name.clone(),
    );

    let model_select_permission = model.select_permissions.get(&session.role).ok_or_else(|| {
        PlanError::Permission(format!(
            "role {} does not have select permission for model {}",
            session.role, qualified_model_name
        ))
    })?;

    let mut usage_counts = plan_types::UsagesCounts::default();
    let mut relationships: BTreeMap<plan_types::NdcRelationshipName, Relationship> =
        BTreeMap::new();

    let permission_filter = match &model_select_permission.filter {
        FilterPermission::AllowAll => Ok::<_, PlanError>(None),
        FilterPermission::Filter(filter) => {
            let filter_ir = process_model_predicate(
                &model_source.data_connector,
                &model_source.type_mappings,
                filter,
                &session.variables,
                &mut usage_counts,
            )
            .map_err(|e| {
                PlanError::Internal(format!("error when processing model predicate: {e}"))
            })?;

            let (filter, _remote_predicates) =
                resolve_filter_expression(&filter_ir, &mut relationships, unique_number)?;

            Ok(Some(filter))
        }
    }?;

    // resolve arguments, adding in presets
    let resolved_arguments = process_arguments(
        &model_target.arguments,
        &model.argument_presets,
        &model.model.arguments,
        &model_source.argument_mappings,
        &model_source.data_connector,
        &model_source.type_mappings,
        &model_source.data_connector_link_argument_presets,
        session,
        request_headers,
        metadata,
        &mut usage_counts,
        &mut relationships,
        unique_number,
    )?;

    // if we are going to filter our results, we must have a `BooleanExpressionType` defined for
    // our model
    // todo: what to do for SELECT ONE filters, we shouldn't need a bool exp for that?
    let model_filter = match &model_target.filter {
        Some(expr) => {
            let boolean_expression_type =
                model.filter_expression_type.as_ref().ok_or_else(|| {
                    PlanError::Internal(format!(
                        "Cannot filter model {} as no boolean expression is defined for it",
                        model.model.name
                    ))
                })?;

            Ok(Some(to_resolved_filter_expr(
                metadata,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                boolean_expression_type,
                expr,
                &model_source.data_connector,
            )?))
        }
        _ => Ok(None),
    }?;

    let filter = match (model_filter, permission_filter) {
        (None, filter) | (filter, None) => filter,
        (Some(filter), Some(permission_filter)) => Some(ResolvedFilterExpression::mk_and(vec![
            filter,
            permission_filter,
        ])),
    };

    let order_by_elements = model_target
        .order_by
        .iter()
        .map(|element| {
            to_resolved_order_by_element(
                metadata,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                element,
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
