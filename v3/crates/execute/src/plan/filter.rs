use async_recursion::async_recursion;
use indexmap::{IndexMap, IndexSet};
use std::collections::BTreeMap;
use tracing_util::SpanVisibility;

use super::error as plan_error;
use super::field;
use super::ndc_request;
use super::query;
use super::relationships::{self, process_model_relationship_definition};
use crate::{error, ndc, HttpContext};
use graphql_ir::NdcRelationshipName;
use open_dds::data_connector::DataConnectorColumnName;
use plan_types::NdcFieldAlias;

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
    relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
) -> Result<Option<graphql_ir::Expression<'s>>, plan_error::Error> {
    let mut expressions = Vec::new();

    if let Some(filter) = permission_filter {
        expressions.push(plan_expression(filter, relationships)?);
    }

    if let Some(filter) = relationship_join_filter {
        expressions.push(plan_expression(filter, relationships)?);
    }

    if let Some(filter) = &query_filter.additional_filter {
        expressions.push(plan_expression(filter, relationships)?);
    }

    if let Some(query_where_expression) = &query_filter.where_clause {
        let planned_expression = plan_expression(query_where_expression, relationships)?;
        expressions.push(planned_expression);
    }

    Ok(graphql_ir::Expression::mk_and(expressions).remove_always_true_expression())
}

/// Plan the expression IR type.
pub fn plan_expression<'s, 'a>(
    expression: &'a graphql_ir::Expression<'s>,
    relationships: &'a mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
) -> Result<graphql_ir::Expression<'s>, plan_error::Error> {
    match expression {
        graphql_ir::Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(and_expression, relationships)?;
                results.push(result);
            }
            Ok(graphql_ir::Expression::mk_and(results))
        }
        graphql_ir::Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(or_expression, relationships)?;
                results.push(result);
            }
            Ok(graphql_ir::Expression::mk_or(results))
        }
        graphql_ir::Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(not_expression, relationships)?;
            Ok(graphql_ir::Expression::mk_not(result))
        }
        graphql_ir::Expression::LocalField(local_field_comparison) => Ok(
            graphql_ir::Expression::LocalField(local_field_comparison.clone()),
        ),
        graphql_ir::Expression::LocalNestedArray {
            predicate,
            field_path,
            column,
        } => {
            let resolved_predicate = plan_expression(predicate, relationships)?;
            Ok(graphql_ir::Expression::LocalNestedArray {
                column: column.clone(),
                field_path: field_path.clone(),
                predicate: Box::new(resolved_predicate),
            })
        }
        graphql_ir::Expression::RelationshipNdcPushdown {
            relationship,
            predicate,
            info,
        } => {
            let relationship_filter = plan_expression(predicate, relationships)?;
            relationships.insert(
                relationship.clone(),
                process_model_relationship_definition(info)?,
            );

            Ok(graphql_ir::Expression::RelationshipNdcPushdown {
                relationship: relationship.clone(),
                predicate: Box::new(relationship_filter),
                info: info.clone(),
            })
        }
        graphql_ir::Expression::RelationshipEngineResolved {
            relationship,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            // This needs to be resolved in engine itself, further planning is deferred until it is resolved
            Ok(graphql_ir::Expression::RelationshipEngineResolved {
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
    ndc_column_mapping: &'a [graphql_ir::RelationshipColumnMapping],
    predicate: &'a graphql_ir::Expression<'s>,
) -> Result<
    (
        query::UnresolvedQueryNode<'s>,
        BTreeMap<NdcRelationshipName, relationships::Relationship>,
    ),
    plan_error::Error,
> {
    let mut relationships = BTreeMap::new();
    let planned_predicate = plan_expression(predicate, &mut relationships)?;

    let query_node = query::QueryNode {
        limit: None,
        offset: None,
        order_by: None,
        predicate: Some(planned_predicate),
        aggregates: None,
        fields: Some(build_ndc_query_fields(ndc_column_mapping)),
    };

    Ok((query_node, relationships))
}

/// Generate the NDC query fields with the mapped NDC columns in a remote relationship.
/// These field values are fetched from the remote data connector.
fn build_ndc_query_fields<'s>(
    ndc_column_mapping: &[graphql_ir::RelationshipColumnMapping],
) -> IndexMap<NdcFieldAlias, field::Field<graphql_ir::Expression<'s>>> {
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

/// Filter expression plan to be resolved
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedFilterExpression {
    And {
        expressions: Vec<ResolvedFilterExpression>,
    },
    Or {
        expressions: Vec<ResolvedFilterExpression>,
    },
    Not {
        expression: Box<ResolvedFilterExpression>,
    },
    LocalFieldComparison(graphql_ir::LocalFieldComparison),
    LocalNestedArray {
        column: DataConnectorColumnName,
        field_path: Vec<DataConnectorColumnName>,
        predicate: Box<ResolvedFilterExpression>,
    },
    LocalRelationshipComparison {
        relationship: NdcRelationshipName,
        predicate: Box<ResolvedFilterExpression>,
    },
}

impl ResolvedFilterExpression {
    pub fn remove_always_true_expression(self) -> Option<ResolvedFilterExpression> {
        match &self {
            ResolvedFilterExpression::And { expressions } if expressions.is_empty() => None,
            ResolvedFilterExpression::Not { expression } => match expression.as_ref() {
                ResolvedFilterExpression::Or { expressions } if expressions.is_empty() => None,
                _ => Some(self),
            },
            _ => Some(self),
        }
    }

    /// Creates a 'FilterExpression::And' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_and(expressions: Vec<ResolvedFilterExpression>) -> ResolvedFilterExpression {
        // If the `and` only contains one expression, we can unwrap it and get rid of the `and`
        // ie. and([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `and`, we can flatten into a single `and`
        // ie. and([and([x,y]), and([a,b])]) == and([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, ResolvedFilterExpression::And { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    ResolvedFilterExpression::And { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            ResolvedFilterExpression::And {
                expressions: subexprs,
            }
        } else {
            ResolvedFilterExpression::And { expressions }
        }
    }

    /// Creates a 'FilterExpression::Or' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_or(expressions: Vec<ResolvedFilterExpression>) -> ResolvedFilterExpression {
        // If the `or` only contains one expression, we can unwrap it and get rid of the `or`
        // ie. or([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `or`, we can flatten into a single `or`
        // ie. or([or([x,y]), or([a,b])]) == or([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, ResolvedFilterExpression::Or { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    ResolvedFilterExpression::Or { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            ResolvedFilterExpression::Or {
                expressions: subexprs,
            }
        } else {
            ResolvedFilterExpression::Or { expressions }
        }
    }

    /// Creates a 'FilterExpression::Not' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_not(expression: ResolvedFilterExpression) -> ResolvedFilterExpression {
        match expression {
            // Double negations can be removed
            // ie. not(not(x))) == x
            ResolvedFilterExpression::Not { expression } => *expression,
            _ => ResolvedFilterExpression::Not {
                expression: Box::new(expression),
            },
        }
    }
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
    expression: graphql_ir::Expression<'s>,
    resolve_context: &ResolveFilterExpressionContext,
) -> Result<ResolvedFilterExpression, error::FieldError>
where
    's: 'async_recursion,
{
    match expression {
        graphql_ir::Expression::And { expressions } => {
            let mut resolved_expressions: Vec<ResolvedFilterExpression> = Vec::new();
            for subexpression in expressions {
                let resolved_expression =
                    resolve_expression(subexpression, resolve_context).await?;
                resolved_expressions.push(resolved_expression);
            }
            Ok(ResolvedFilterExpression::And {
                expressions: resolved_expressions,
            })
        }
        graphql_ir::Expression::Or { expressions } => {
            let mut resolved_expressions = Vec::new();
            for subexpression in expressions {
                let resolve_expression = resolve_expression(subexpression, resolve_context).await?;
                resolved_expressions.push(resolve_expression);
            }
            Ok(ResolvedFilterExpression::Or {
                expressions: resolved_expressions,
            })
        }
        graphql_ir::Expression::Not {
            expression: subexpression,
        } => {
            let resolved_expression = resolve_expression(*subexpression, resolve_context).await?;
            Ok(ResolvedFilterExpression::Not {
                expression: Box::new(resolved_expression),
            })
        }
        graphql_ir::Expression::LocalField(local_field_comparison) => Ok(
            ResolvedFilterExpression::LocalFieldComparison(local_field_comparison),
        ),
        graphql_ir::Expression::RelationshipNdcPushdown {
            relationship,
            predicate,
            info: _,
        } => {
            let resolved_expression = resolve_expression(*predicate, resolve_context).await?;
            Ok(ResolvedFilterExpression::LocalRelationshipComparison {
                relationship,
                predicate: Box::new(resolved_expression),
            })
        }
        graphql_ir::Expression::LocalNestedArray {
            column,
            field_path,
            predicate,
        } => {
            let resolved_expression = resolve_expression(*predicate, resolve_context).await?;
            Ok(ResolvedFilterExpression::LocalNestedArray {
                column,
                field_path,
                predicate: Box::new(resolved_expression),
            })
        }
        graphql_ir::Expression::RelationshipEngineResolved {
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
                            let (remote_query_node, collection_relationships) =
                                super::filter::plan_remote_predicate(
                                    &ndc_column_mapping,
                                    &predicate,
                                )
                                .map_err(|e| {
                                    error::FilterPredicateError::RemoteRelationshipPlanError(
                                        Box::new(e),
                                    )
                                })?;

                            let query_execution_plan = query::QueryExecutionPlan {
                                query_node: remote_query_node.resolve(resolve_context).await?,
                                collection: target_model_source.collection.clone(),
                                arguments: BTreeMap::new(),
                                collection_relationships,
                                variables: None,
                                data_connector: &target_model_source.data_connector,
                            };

                            let ndc_query_request =
                                ndc_request::make_ndc_query_request(query_execution_plan)?;

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
    ndc_column_mapping: &[graphql_ir::RelationshipColumnMapping],
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

            let graphql_ir::SourceNdcColumn {
                column: source_column,
                field_path,
                eq_operator,
            } = &column_mapping.source_ndc_column;
            // Generate LHS (source) column comparison with target column value
            column_comparisons.push(ResolvedFilterExpression::LocalFieldComparison(
                graphql_ir::LocalFieldComparison::BinaryComparison {
                    column: graphql_ir::ComparisonTarget::Column {
                        name: source_column.clone(),
                        field_path: field_path.clone(),
                    },
                    operator: eq_operator.clone(),
                    value: graphql_ir::ComparisonValue::Scalar {
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
