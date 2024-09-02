use super::types;
use super::HttpContext;
use crate::error;
use crate::explain::fetch_explain_from_data_connector;
use crate::plan;
use crate::plan::field::{UnresolvedField, UnresolvedNestedField};
use crate::plan::query::UnresolvedQueryNode;
use async_recursion::async_recursion;
use indexmap::IndexMap;
use ir::NdcFieldAlias;
use std::collections::BTreeMap;

#[async_recursion]
pub(crate) async fn explain_query_predicate_node<'s>(
    expose_internal_errors: &crate::ExposeInternalErrors,
    http_context: &HttpContext,
    node: &UnresolvedQueryNode<'s>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    // Generate explain steps for involved remote relationships in the predicate
    if let Some(filter_predicate) = &node.predicate {
        explain_query_predicate(
            expose_internal_errors,
            http_context,
            filter_predicate,
            steps,
        )
        .await?;
    }

    // Generate explain steps for relationship field queries
    explain_query_predicate_fields(
        expose_internal_errors,
        http_context,
        node.fields.as_ref(),
        steps,
    )
    .await?;
    Ok(())
}

async fn explain_query_predicate_fields<'s, 'a>(
    expose_internal_errors: &crate::ExposeInternalErrors,
    http_context: &HttpContext,
    fields: Option<&'a IndexMap<NdcFieldAlias, UnresolvedField<'s>>>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    if let Some(fields) = fields {
        for field in fields.values() {
            match field {
                UnresolvedField::Column { fields, .. } => {
                    explain_query_predicate_nested_field(
                        expose_internal_errors,
                        http_context,
                        fields.as_ref(),
                        steps,
                    )
                    .await?;
                }
                UnresolvedField::Relationship { query_node, .. } => {
                    explain_query_predicate_node(
                        expose_internal_errors,
                        http_context,
                        query_node,
                        steps,
                    )
                    .await?;
                }
            }
        }
    }
    Ok(())
}

#[async_recursion]
pub(crate) async fn explain_query_predicate_nested_field<'s, 'a>(
    expose_internal_errors: &crate::ExposeInternalErrors,
    http_context: &HttpContext,
    nested_field: Option<&'a UnresolvedNestedField<'s>>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    if let Some(nested_field) = nested_field {
        match nested_field {
            UnresolvedNestedField::Object(nested_object) => {
                explain_query_predicate_fields(
                    expose_internal_errors,
                    http_context,
                    Some(&nested_object.fields),
                    steps,
                )
                .await?;
            }
            UnresolvedNestedField::Array(array_fields) => {
                explain_query_predicate_nested_field(
                    expose_internal_errors,
                    http_context,
                    Some(&array_fields.fields),
                    steps,
                )
                .await?;
            }
        }
    }
    Ok(())
}

#[async_recursion]
async fn explain_query_predicate<'s>(
    expose_internal_errors: &crate::ExposeInternalErrors,
    http_context: &HttpContext,
    predicate: &ir::Expression<'s>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    match predicate {
        ir::Expression::And { expressions } => {
            for expression in expressions {
                explain_query_predicate(expose_internal_errors, http_context, expression, steps)
                    .await?;
            }
            Ok(())
        }
        ir::Expression::Or { expressions } => {
            for expression in expressions {
                explain_query_predicate(expose_internal_errors, http_context, expression, steps)
                    .await?;
            }
            Ok(())
        }
        ir::Expression::Not { expression } => {
            explain_query_predicate(expose_internal_errors, http_context, expression, steps).await
        }
        ir::Expression::LocalField { .. }
        | ir::Expression::RelationshipNdcPushdown { .. }
        | ir::Expression::LocalNestedArray { .. } => Ok(()),

        // Needs to be resolved in the Engine by making a request to the target data connector,
        // for which we need to explain the corresponding NDC request.
        ir::Expression::RelationshipEngineResolved {
            relationship: _,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            let (remote_query_node, collection_relationships) =
                plan::filter::plan_remote_predicate(ndc_column_mapping, predicate)
                    .map_err(|e| error::RequestError::ExplainError(e.to_string()))?;

            explain_query_predicate_node(
                expose_internal_errors,
                http_context,
                &remote_query_node,
                steps,
            )
            .await?;

            let resolved_query_node = remote_query_node
                .resolve(http_context)
                .await
                .map_err(|e| error::RequestError::ExplainError(e.to_string()))?;

            let query_execution_plan = plan::query::ResolvedQueryExecutionPlan {
                query_node: resolved_query_node,
                collection: target_model_source.collection.clone(),
                arguments: BTreeMap::new(),
                collection_relationships: collection_relationships.clone(),
                variables: None,
                data_connector: &target_model_source.data_connector,
            };

            let ndc_query_request = plan::ndc_request::make_ndc_query_request(query_execution_plan)
                .map_err(|e| error::RequestError::ExplainError(e.to_string()))?;

            let ndc_request = types::NDCRequest::Query(ndc_query_request);
            let data_connector_explain = fetch_explain_from_data_connector(
                *expose_internal_errors,
                http_context,
                &ndc_request,
                &target_model_source.data_connector,
            )
            .await;

            steps.push(types::Step::ModelSelect(types::ModelSelectIR {
                model_name: target_model_name.to_string(),
                ndc_request,
                ndc_explain: data_connector_explain,
            }));
            Ok(())
        }
    }
}
