use super::types;
use super::HttpContext;
use crate::error;
use crate::explain::fetch_explain_from_data_connector;
use crate::plan::ndc_request;
use crate::plan::types::{Field, FilterExpression, NestedField, QueryNode};
use async_recursion::async_recursion;
use indexmap::IndexMap;
use std::collections::BTreeMap;

#[async_recursion]
pub(crate) async fn explain_query_predicate_node<'s>(
    expose_internal_errors: &crate::ExposeInternalErrors,
    http_context: &HttpContext,
    node: &QueryNode<'s>,
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
    fields: Option<&'a IndexMap<ndc_models::FieldName, Field<'s>>>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    if let Some(fields) = fields {
        for field in fields.values() {
            match field {
                Field::Column { fields, .. } => {
                    explain_query_predicate_nested_field(
                        expose_internal_errors,
                        http_context,
                        fields.as_ref(),
                        steps,
                    )
                    .await?;
                }
                Field::Relationship { query_node, .. } => {
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
    nested_field: Option<&'a NestedField<'s>>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    if let Some(nested_field) = nested_field {
        match nested_field {
            NestedField::Object(nested_object) => {
                explain_query_predicate_fields(
                    expose_internal_errors,
                    http_context,
                    Some(&nested_object.fields),
                    steps,
                )
                .await?;
            }
            NestedField::Array(array_fields) => {
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
    predicate: &FilterExpression<'s>,
    steps: &mut Vec<types::Step>,
) -> Result<(), error::RequestError> {
    match predicate {
        FilterExpression::And { expressions } => {
            for expression in expressions {
                explain_query_predicate(expose_internal_errors, http_context, expression, steps)
                    .await?;
            }
            Ok(())
        }
        FilterExpression::Or { expressions } => {
            for expression in expressions {
                explain_query_predicate(expose_internal_errors, http_context, expression, steps)
                    .await?;
            }
            Ok(())
        }
        FilterExpression::Not { expression } => {
            explain_query_predicate(expose_internal_errors, http_context, expression, steps).await
        }
        FilterExpression::NDCComparison { .. }
        | FilterExpression::LocalRelationshipComparison { .. } => Ok(()),
        FilterExpression::RemoteRelationshipComparison {
            relationship_name: _,
            model_name,
            ndc_column_mapping: _,
            remote_collection,
            remote_query_node,
            collection_relationships,
            data_connector,
        } => {
            explain_query_predicate_node(
                expose_internal_errors,
                http_context,
                remote_query_node,
                steps,
            )
            .await?;
            let query = remote_query_node
                .clone()
                .resolve(http_context)
                .await
                .map_err(|e| error::RequestError::ExplainError(e.to_string()))?;
            let query_request = ndc_models::QueryRequest {
                collection: remote_collection.clone(),
                query,
                arguments: BTreeMap::new(),
                collection_relationships: collection_relationships.clone(),
                variables: None,
            };

            let ndc_query_request =
                ndc_request::make_ndc_query_request(query_request, data_connector)
                    .map_err(|e| error::RequestError::ExplainError(e.to_string()))?;
            let ndc_request = types::NDCRequest::Query(ndc_query_request);
            let data_connector_explain = fetch_explain_from_data_connector(
                *expose_internal_errors,
                http_context,
                &ndc_request,
                data_connector,
            )
            .await;

            steps.push(types::Step::ModelSelect(types::ModelSelectIR {
                model_name: model_name.clone(),
                ndc_request,
                ndc_explain: data_connector_explain,
            }));
            Ok(())
        }
    }
}
