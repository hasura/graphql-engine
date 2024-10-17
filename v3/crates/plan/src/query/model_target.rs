use super::boolean_expression;
use super::types::NDCQuery;
use crate::filter::to_resolved_filter_expr;
use crate::order_by::to_resolved_order_by_element;
use crate::types::PlanError;
use std::collections::BTreeMap;

use execute::{plan::ResolvedFilterExpression, HttpContext};
use hasura_authn_core::Session;
use metadata_resolve::FilterPermission;
use open_dds::query::ModelTarget;

// Turn a `plan_types::Expression` into `execute::ResolvedFilterExpression`
// Currently this works by running all the remote predicates, soon it won't need to
async fn resolve_filter_expression(
    filter_ir: &plan_types::Expression<'_>,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, execute::plan::Relationship>,
    http_context: &HttpContext,
) -> Result<ResolvedFilterExpression, PlanError> {
    let filter_plan = execute::plan::plan_expression(filter_ir, relationships).map_err(|e| {
        PlanError::Internal(format!("error constructing permission filter plan: {e}"))
    })?;

    // TODO: this thing has to change, need to be pushed into the
    // execution plan. We shouldn't be running this in the planning phase
    let resolve_context =
        execute::plan::ResolveFilterExpressionContext::new_allow_in_engine_resolution(http_context);

    // TODO: this resolve step should go away once we've split `execute` into actual 'plan' and
    // 'run' steps, then none of this planning will need to be async. (currently the async
    // is in case we're running remote predicates, which should not be a planning concern).
    execute::plan::resolve_expression(filter_plan, &resolve_context)
        .await
        .map_err(|e| PlanError::Internal(format!("error resolving permission filter plan: {e}")))
}

pub async fn model_target_to_ndc_query(
    model_target: &ModelTarget,
    session: &Session,
    http_context: &HttpContext,
    metadata: &metadata_resolve::Metadata,
    request_headers: &reqwest::header::HeaderMap,
    // The following are things we could compute, but we have them on hand
    // at all call sites anyway:
    model: &metadata_resolve::ModelWithArgumentPresets,
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

            let filter =
                resolve_filter_expression(&filter_ir, &mut relationships, http_context).await?;

            Ok(Some(filter))
        }
    }?;

    let mut graphql_ir_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &model_target.arguments {
        let ndc_argument_name = model_source.argument_mappings.get(argument_name).ok_or_else(|| PlanError::Internal(format!("couldn't fetch argument mapping for argument {argument_name} of model {qualified_model_name}")))?;
        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(bool_exp) => {
                // this implementation is incomplete
                // and should be filled out once we implement user-defined filters here
                let predicate =
                    boolean_expression::open_dd_boolean_expression_to_plan_types_expression(
                        bool_exp,
                        &model_source.type_mappings,
                        &model.model.data_type,
                    )?;

                graphql_ir::Argument::BooleanExpression { predicate }
            }
            open_dds::query::Value::Literal(value) => graphql_ir::Argument::Literal {
                value: value.clone(),
            },
        };
        graphql_ir_arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
    }

    let argument_presets_for_role = model.argument_presets.get(&session.role).unwrap();

    // add any preset arguments from model permissions
    let arguments_with_presets = graphql_ir::process_argument_presets(
        &model_source.data_connector,
        &model_source.type_mappings,
        Some(argument_presets_for_role),
        &model_source.data_connector_link_argument_presets,
        &session.variables,
        request_headers,
        graphql_ir_arguments,
        &mut usage_counts,
    )
    .map_err(|e| PlanError::Internal(e.to_string()))?;

    // now we turn the GraphQL IR `Arguments` type into the `execute` "resolved" argument type
    // by resolving any `Expression` types inside
    // this currently involves executing any remote predicates, but should not in future
    let mut resolved_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &arguments_with_presets {
        let resolved_argument_value = match argument_value {
            graphql_ir::Argument::BooleanExpression { predicate } => {
                let resolved_filter_expression =
                    resolve_filter_expression(predicate, &mut relationships, http_context).await?;

                execute::plan::Argument::BooleanExpression {
                    predicate: resolved_filter_expression,
                }
            }
            graphql_ir::Argument::Literal { value } => execute::plan::Argument::Literal {
                value: value.clone(),
            },
        };
        resolved_arguments.insert(argument_name.clone(), resolved_argument_value.clone());
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
        arguments: resolved_arguments,
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
