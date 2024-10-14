use super::types::NDCQuery;
use crate::filter::to_resolved_filter_expr;
use crate::order_by::to_resolved_order_by_element;
use crate::types::PlanError;
use std::collections::BTreeMap;

use execute::{plan::ResolvedFilterExpression, HttpContext};
use hasura_authn_core::Session;
use metadata_resolve::FilterPermission;
use open_dds::query::ModelTarget;

pub async fn model_target_to_ndc_query(
    model_target: &ModelTarget,
    session: &Session,
    http_context: &HttpContext,
    metadata: &metadata_resolve::Metadata,
    // The following are things we could compute, but we have them on hand
    // at all call sites anyway:
    model: &metadata_resolve::ModelWithPermissions,
    model_source: &metadata_resolve::ModelSource,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
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
    let mut relationships: BTreeMap<plan_types::NdcRelationshipName, execute::plan::Relationship> =
        BTreeMap::new();

    let permission_filter = match &model_select_permission.filter {
        FilterPermission::AllowAll => Ok::<_, PlanError>(None),
        FilterPermission::Filter(filter) => {
            let filter_ir = graphql_ir::process_model_predicate(
                &model_source.data_connector,
                &model_source.type_mappings,
                filter,
                &session.variables,
                &mut usage_counts,
            )
            .map_err(|e| {
                PlanError::Internal(format!("error when processing model predicate: {e}"))
            })?;

            let filter_plan = execute::plan::plan_expression(&filter_ir, &mut relationships)
                .map_err(|e| {
                    PlanError::Internal(format!("error constructing permission filter plan: {e}"))
                })?;

            // TODO: this thing has to change, need to be pushed into the
            // execution plan. We shouldn't be running this in the planning phase
            let resolve_context =
                execute::plan::ResolveFilterExpressionContext::new_allow_in_engine_resolution(
                    http_context,
                );

            // TODO: this resolve step should go away once we've split `execute` into actual 'plan' and
            // 'run' steps, then none of this planning will need to be async. (currently the async
            // is in case we're running remote predicates, which should not be a planning concern).
            let filter = execute::plan::resolve_expression(filter_plan, &resolve_context)
                .await
                .map_err(|e| {
                    PlanError::Internal(format!("error resolving permission filter plan: {e}"))
                })?;
            Ok(Some(filter))
        }
    }?;

    let mut ndc_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &model_target.arguments {
        let ndc_argument_name = model_source.argument_mappings.get(argument_name).ok_or_else(|| PlanError::Internal(format!("couldn't fetch argument mapping for argument {argument_name} of model {qualified_model_name}")))?;
        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(_) => {
                return Err(PlanError::Internal(format!("unexpected boolean expression as value for argument {argument_name} of model {qualified_model_name}")));
            }
            open_dds::query::Value::Literal(value) => value,
        };
        ndc_arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
    }

    let model_filter = model_target
        .filter
        .as_ref()
        .map(|expr| {
            to_resolved_filter_expr(
                metadata,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                expr,
            )
        })
        .transpose()?;

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
        arguments: ndc_arguments,
        collection_name: model_source.collection.clone(),
        collection_relationships: relationships,
        data_connector: model_source.data_connector.clone(),
        filter,
        limit,
        offset,
        order_by: graphql_ir::ResolvedOrderBy {
            order_by_elements,
            relationships: BTreeMap::new(),
        },
    };

    Ok(query)
}
