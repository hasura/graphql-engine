use async_recursion::async_recursion;
use indexmap::{IndexMap, IndexSet};
use std::collections::BTreeMap;
use tracing_util::SpanVisibility;

use super::error as plan_error;
use super::field;
use super::query;
use super::relationships::process_model_relationship_definition;
use crate::{error, ndc};
use engine_types::HttpContext;
use plan_types::{
    NdcFieldAlias, NdcRelationshipName, PredicateQueryTrees, Relationship, ResolvedFilterExpression,
};

/// Plan the filter expression IR.
/// This function will take the filter expression IR and convert it into a planned filter expression
/// that can be converted the NDC filter expression.
/// This will record the relationships that are used in the filter expression.
pub(crate) fn plan_filter_expression<'s>(
    graphql_ir::FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter,
    }: &graphql_ir::FilterExpression<'s>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<Option<plan_types::Expression<'s>>, plan_error::Error> {
    let mut expressions = Vec::new();
    let mut remote_predicates = PredicateQueryTrees::new();

    if let Some(filter) = permission_filter {
        expressions.push(plan_expression(
            filter,
            relationships,
            &mut remote_predicates,
        )?);
    }

    if let Some(filter) = relationship_join_filter {
        expressions.push(plan_expression(
            filter,
            relationships,
            &mut remote_predicates,
        )?);
    }

    if let Some(filter) = &query_filter.additional_filter {
        expressions.push(plan_expression(
            filter,
            relationships,
            &mut remote_predicates,
        )?);
    }

    if let Some(query_where_expression) = &query_filter.where_clause {
        let planned_expression = plan_expression(
            query_where_expression,
            relationships,
            &mut remote_predicates,
        )?;
        expressions.push(planned_expression);
    }

    Ok(plan_types::Expression::mk_and(expressions).remove_always_true_expression())
}

/// Plan the expression IR type.
pub fn plan_expression<'s, 'a>(
    expression: &'a plan_types::Expression<'s>,
    relationships: &'a mut BTreeMap<NdcRelationshipName, Relationship>,
    _remote_predicates: &'a mut PredicateQueryTrees,
) -> Result<plan_types::Expression<'s>, plan_error::Error> {
    match expression {
        plan_types::Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(and_expression, relationships, _remote_predicates)?;
                results.push(result);
            }
            Ok(plan_types::Expression::mk_and(results))
        }
        plan_types::Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(or_expression, relationships, _remote_predicates)?;
                results.push(result);
            }
            Ok(plan_types::Expression::mk_or(results))
        }
        plan_types::Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(not_expression, relationships, _remote_predicates)?;
            Ok(plan_types::Expression::mk_not(result))
        }
        plan_types::Expression::LocalField(local_field_comparison) => Ok(
            plan_types::Expression::LocalField(local_field_comparison.clone()),
        ),
        plan_types::Expression::LocalNestedArray {
            predicate,
            field_path,
            column,
        } => {
            let resolved_predicate = plan_expression(predicate, relationships, _remote_predicates)?;
            Ok(plan_types::Expression::LocalNestedArray {
                column: column.clone(),
                field_path: field_path.clone(),
                predicate: Box::new(resolved_predicate),
            })
        }
        plan_types::Expression::RelationshipLocalComparison {
            relationship,
            predicate,
            info,
        } => {
            let relationship_filter =
                plan_expression(predicate, relationships, _remote_predicates)?;
            relationships.insert(
                relationship.clone(),
                process_model_relationship_definition(info)?,
            );

            Ok(plan_types::Expression::RelationshipLocalComparison {
                relationship: relationship.clone(),
                predicate: Box::new(relationship_filter),
                info: info.clone(),
            })
        }
        plan_types::Expression::RelationshipRemoteComparison {
            relationship,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            // TODO: Plan the remote relationship comparison expression and populate the remote_predicates

            // This needs to be resolved in engine itself, further planning is deferred until it is resolved
            Ok(plan_types::Expression::RelationshipRemoteComparison {
                relationship: relationship.clone(),
                target_model_name,
                target_model_source: target_model_source.clone(),
                ndc_column_mapping: ndc_column_mapping.clone(),
                predicate: predicate.clone(),
            })
        }
    }
}

/// Generate comparison expression plan for remote relationshp predicate.
pub fn plan_remote_predicate<'s, 'a>(
    ndc_column_mapping: &'a [plan_types::RelationshipColumnMapping],
    predicate: &'a plan_types::Expression<'s>,
) -> Result<
    (
        query::UnresolvedQueryNode<'s>,
        PredicateQueryTrees,
        BTreeMap<NdcRelationshipName, Relationship>,
    ),
    plan_error::Error,
> {
    let mut relationships = BTreeMap::new();
    let mut remote_predicates = PredicateQueryTrees::new();
    let planned_predicate = plan_expression(predicate, &mut relationships, &mut remote_predicates)?;

    let query_node = query::QueryNode {
        limit: None,
        offset: None,
        order_by: None,
        predicate: Some(planned_predicate),
        aggregates: None,
        fields: Some(build_ndc_query_fields(ndc_column_mapping)),
    };

    Ok((query_node, remote_predicates, relationships))
}

/// Generate the NDC query fields with the mapped NDC columns in a remote relationship.
/// These field values are fetched from the remote data connector.
fn build_ndc_query_fields<'s>(
    ndc_column_mapping: &[plan_types::RelationshipColumnMapping],
) -> IndexMap<NdcFieldAlias, field::Field<'s>> {
    let mut fields = IndexMap::new();
    for mapping in ndc_column_mapping {
        let field = field::Field::Column {
            column: mapping.target_ndc_column.clone(),
            fields: None,
            arguments: BTreeMap::new(),
        };
        fields.insert(
            NdcFieldAlias::from(mapping.target_ndc_column.as_str()),
            field,
        );
    }
    fields
}

/// Context required to resolve the filter expressions
pub enum ResolveFilterExpressionContext<'req> {
    /// Allow only expressions that can be pushed down to NDC.
    /// Reject relationships that require resolution in engine such as remote relationship comparison
    /// and local relationships without NDC 'relation_comparisoins' capability.
    OnlyNdcPushdown,
    /// Allow predicate resolution in engine for relationships.
    /// Requires the HTTP context to fetch the relationship data.
    AllowInEngineResolution { http_context: &'req HttpContext },
}

impl<'req> ResolveFilterExpressionContext<'req> {
    /// Create a new context to allow predicate resolution in engine
    pub fn new_allow_in_engine_resolution(http_context: &'req HttpContext) -> Self {
        ResolveFilterExpressionContext::AllowInEngineResolution { http_context }
    }

    /// Create a new context to reject predicate resolution in engine
    pub fn new_only_allow_ndc_pushdown_expressions() -> Self {
        ResolveFilterExpressionContext::OnlyNdcPushdown
    }
}

/// Resolve the filter expression plan and generate NDC expression.
#[async_recursion]
pub async fn resolve_expression<'s>(
    expression: plan_types::Expression<'s>,
    resolve_context: &ResolveFilterExpressionContext,
) -> Result<plan_types::ResolvedFilterExpression, error::FieldError>
where
    's: 'async_recursion,
{
    match expression {
        plan_types::Expression::And { expressions } => {
            let mut resolved_expressions: Vec<ResolvedFilterExpression> = Vec::new();
            for subexpression in expressions {
                let resolved_expression =
                    resolve_expression(subexpression, resolve_context).await?;
                resolved_expressions.push(resolved_expression);
            }
            Ok(plan_types::ResolvedFilterExpression::And {
                expressions: resolved_expressions,
            })
        }
        plan_types::Expression::Or { expressions } => {
            let mut resolved_expressions = Vec::new();
            for subexpression in expressions {
                let resolve_expression = resolve_expression(subexpression, resolve_context).await?;
                resolved_expressions.push(resolve_expression);
            }
            Ok(plan_types::ResolvedFilterExpression::Or {
                expressions: resolved_expressions,
            })
        }
        plan_types::Expression::Not {
            expression: subexpression,
        } => {
            let resolved_expression = resolve_expression(*subexpression, resolve_context).await?;
            Ok(plan_types::ResolvedFilterExpression::Not {
                expression: Box::new(resolved_expression),
            })
        }
        plan_types::Expression::LocalField(local_field_comparison) => Ok(
            ResolvedFilterExpression::LocalFieldComparison(local_field_comparison),
        ),
        plan_types::Expression::RelationshipLocalComparison {
            relationship,
            predicate,
            info: _,
        } => {
            let resolved_expression = resolve_expression(*predicate, resolve_context).await?;
            Ok(
                plan_types::ResolvedFilterExpression::LocalRelationshipComparison {
                    relationship,
                    predicate: Box::new(resolved_expression),
                },
            )
        }
        plan_types::Expression::LocalNestedArray {
            column,
            field_path,
            predicate,
        } => {
            let resolved_expression = resolve_expression(*predicate, resolve_context).await?;
            Ok(plan_types::ResolvedFilterExpression::LocalNestedArray {
                column,
                field_path,
                predicate: Box::new(resolved_expression),
            })
        }
        plan_types::Expression::RelationshipRemoteComparison {
            relationship,
            target_model_name: _,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            let http_context = match resolve_context {
                ResolveFilterExpressionContext::AllowInEngineResolution { http_context } => {
                    http_context
                }
                ResolveFilterExpressionContext::OnlyNdcPushdown => {
                    return Err(error::FieldError::RelationshipPredicatesNotSupported {
                        name: relationship.clone(),
                    })
                }
            };
            let tracer = tracing_util::global_tracer();
            tracer
                .in_span_async(
                    "resolve_in_engine_relationship_predicate",
                    format!("Resolve relationship comparison expression: {relationship}"),
                    SpanVisibility::User,
                    || {
                        Box::pin(async {
                            let (remote_query_node, _, collection_relationships) =
                                super::filter::plan_remote_predicate(
                                    &ndc_column_mapping,
                                    &predicate,
                                )
                                .map_err(|e| {
                                    error::FilterPredicateError::RemoteRelationshipPlanError(
                                        Box::new(e),
                                    )
                                })?;

                            let query_execution_plan = plan_types::QueryExecutionPlan {
                                query_node: remote_query_node.resolve(resolve_context).await?,
                                collection: target_model_source.collection.clone(),
                                arguments: BTreeMap::new(),
                                collection_relationships,
                                variables: None,
                                data_connector: target_model_source.data_connector.clone(),
                            };

                            let ndc_query_request =
                                crate::make_ndc_query_request(query_execution_plan)?;

                            // Generate LHS mapping NDC columns values from the remote data connector
                            // using the RHS NDC columns.
                            let connector_result = ndc::fetch_from_data_connector(
                                http_context,
                                &ndc_query_request,
                                &target_model_source.data_connector,
                                None,
                            )
                            .await
                            .map_err(error::FilterPredicateError::RemoteRelationshipNDCRequest)?;

                            // Assume a single row set is returned
                            let single_rowset = crate::process_response::get_single_rowset(
                                connector_result.as_latest_rowsets(),
                            )
                            .map_err(|e| {
                                error::FilterPredicateError::NotASingleRowSet(e.to_string())
                            })?;

                            let column_comparison = build_source_column_comparisons(
                                single_rowset.rows.unwrap_or_else(Vec::new),
                                &ndc_column_mapping,
                            )?;

                            Ok(column_comparison)
                        })
                    },
                )
                .await
        }
    }
}

/// Utility to store distinct comparisons to avoid duplicate comparison predicates
/// in the remote relationship comparison expression.
struct DistinctComparisons {
    comparisons: IndexSet<ResolvedFilterExpression>,
}

impl DistinctComparisons {
    fn new() -> Self {
        DistinctComparisons {
            comparisons: IndexSet::new(),
        }
    }

    fn push(&mut self, expression: ResolvedFilterExpression) {
        self.comparisons.insert(expression);
    }

    fn into_vec(self) -> Vec<ResolvedFilterExpression> {
        self.comparisons.into_iter().collect()
    }
}

/// Build the column comparison expressions using the equal operator NDC response rows.
///
/// [[(a, a_value_1, b, b_value_1), (a, a_value_2, b, b_value_2)]] --->
/// WHERE (a = a_value_1 AND b = b_value_1) OR (a = a_value_2 AND b = b_value_2)
/// The above filter is semantically equivalent to
/// WHERE (a, b) IN ((a_value_1, b_value_1), (a_value_2, b_value_2))
fn build_source_column_comparisons(
    mut rows: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>,
    ndc_column_mapping: &[plan_types::RelationshipColumnMapping],
) -> Result<ResolvedFilterExpression, error::FieldError> {
    let mut expressions = DistinctComparisons::new();
    for row in &mut rows {
        let mut column_comparisons = Vec::new();
        for column_mapping in ndc_column_mapping {
            let target_column_field =
                ndc_models::FieldName::from(column_mapping.target_ndc_column.as_str());
            // Fetch RHS (target) column value from the row
            let target_value = row.swap_remove(&target_column_field).ok_or_else(
                || error::FieldInternalError::InternalGeneric{
                        description: format!(
                            "Unable to build remote predicate local comparison. Target field from NDC response not found: {target_column_field}"
                        ),
                }
            )?;

            let plan_types::SourceNdcColumn {
                column: source_column,
                field_path,
                eq_operator,
            } = &column_mapping.source_ndc_column;
            // Generate LHS (source) column comparison with target column value
            column_comparisons.push(ResolvedFilterExpression::LocalFieldComparison(
                plan_types::LocalFieldComparison::BinaryComparison {
                    column: plan_types::ComparisonTarget::Column {
                        name: source_column.clone(),
                        field_path: field_path.clone(),
                    },
                    operator: eq_operator.clone(),
                    value: plan_types::ComparisonValue::Scalar {
                        value: target_value.0,
                    },
                },
            ));
        }
        // combine column comparisons from each row with AND
        // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
        expressions.push(ResolvedFilterExpression::mk_and(column_comparisons));
    }
    // combine all row comparisons with OR
    // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    //     OR (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    Ok(ResolvedFilterExpression::mk_or(expressions.into_vec()))
}
