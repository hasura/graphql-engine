use super::boolean_expression;
use super::types::NDCQuery;
use crate::filter::to_resolved_filter_expr;
use crate::order_by::to_resolved_order_by_element;
use crate::types::PlanError;
use std::collections::BTreeMap;

use hasura_authn_core::Session;
use metadata_resolve::{FilterPermission, ModelExpressionType};
use open_dds::query::ModelTarget;
use plan_types::{
    Argument, PredicateQueryTrees, Relationship, ResolvedFilterExpression, UniqueNumber,
};

// Turn a `plan_types::Expression` into `execute::ResolvedFilterExpression`
// Currently this works by running all the remote predicates, soon it won't need to
fn resolve_filter_expression(
    filter_ir: &plan_types::Expression<'_>,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<(ResolvedFilterExpression, PredicateQueryTrees), PlanError> {
    let mut remote_predicates = plan_types::PredicateQueryTrees::new();

    let resolved_filter_expression = graphql_ir::plan_expression(
        filter_ir,
        relationships,
        &mut remote_predicates,
        unique_number,
    )
    .map_err(|e| PlanError::Internal(format!("error constructing permission filter plan: {e}")))?;

    Ok((resolved_filter_expression, remote_predicates))
}

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

            let (filter, _remote_predicates) =
                resolve_filter_expression(&filter_ir, &mut relationships, unique_number)?;

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
                let (resolved_filter_expression, _remote_predicates) =
                    resolve_filter_expression(predicate, &mut relationships, unique_number)?;

                Argument::BooleanExpression {
                    predicate: resolved_filter_expression,
                }
            }
            graphql_ir::Argument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
        };
        resolved_arguments.insert(argument_name.clone(), resolved_argument_value.clone());
    }

    // if we are going to filter our results, we must have a `BooleanExpressionType` defined for
    // our model
    let model_filter = match &model_target.filter {
        Some(expr) => {
            let boolean_expression_type = match &model.filter_expression_type {
                Some(ModelExpressionType::BooleanExpressionType(boolean_expression_type)) => {
                    Ok(boolean_expression_type)
                }
                _ => Err(PlanError::Internal(format!(
                    "Cannot filter model {} as no boolean expression is defined for it",
                    model.model.name
                ))),
            }?;

            Ok(Some(to_resolved_filter_expr(
                metadata,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                boolean_expression_type,
                expr,
                &model_source.data_connector.name,
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
