use std::collections::BTreeMap;

use super::error as plan_error;
use super::relationships::process_model_relationship_definition;
use crate::FilterExpression;
use indexmap::IndexMap;
use plan_types::{
    ExecutionTree, Field, FieldsSelection, JoinLocations, NdcFieldAlias, NdcRelationshipName,
    PredicateQueryTree, PredicateQueryTrees, QueryExecutionPlan, QueryNodeNew, Relationship,
    ResolvedFilterExpression, UniqueNumber,
};

/// Plan the filter expression IR.
/// This function will take the filter expression IR and convert it into a planned filter expression
/// that can be converted the NDC filter expression.
/// This will record the relationships that are used in the filter expression.
pub(crate) fn plan_filter_expression(
    FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter,
    }: &FilterExpression<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<(Option<ResolvedFilterExpression>, PredicateQueryTrees), plan_error::Error> {
    let mut expressions = Vec::new();
    let mut remote_predicates = PredicateQueryTrees::new();

    if let Some(filter) = permission_filter {
        expressions.push(plan_expression(
            filter,
            relationships,
            &mut remote_predicates,
            unique_number,
        )?);
    }

    if let Some(filter) = relationship_join_filter {
        expressions.push(plan_expression(
            filter,
            relationships,
            &mut remote_predicates,
            unique_number,
        )?);
    }

    if let Some(filter) = &query_filter.additional_filter {
        expressions.push(plan_expression(
            filter,
            relationships,
            &mut remote_predicates,
            unique_number,
        )?);
    }

    if let Some(query_where_expression) = &query_filter.where_clause {
        let planned_expression = plan_expression(
            query_where_expression,
            relationships,
            &mut remote_predicates,
            unique_number,
        )?;
        expressions.push(planned_expression);
    }

    Ok((
        ResolvedFilterExpression::mk_and(expressions).remove_always_true_expression(),
        remote_predicates,
    ))
}

/// Plan the expression IR type.
pub fn plan_expression<'a>(
    expression: &'a plan_types::Expression<'_>,
    relationships: &'a mut BTreeMap<NdcRelationshipName, Relationship>,
    remote_predicates: &'a mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<ResolvedFilterExpression, plan_error::Error> {
    match expression {
        plan_types::Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(
                    and_expression,
                    relationships,
                    remote_predicates,
                    unique_number,
                )?;
                results.push(result);
            }
            Ok(ResolvedFilterExpression::mk_and(results))
        }
        plan_types::Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(
                    or_expression,
                    relationships,
                    remote_predicates,
                    unique_number,
                )?;
                results.push(result);
            }
            Ok(ResolvedFilterExpression::mk_or(results))
        }
        plan_types::Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(
                not_expression,
                relationships,
                remote_predicates,
                unique_number,
            )?;
            Ok(ResolvedFilterExpression::mk_not(result))
        }
        plan_types::Expression::LocalField(local_field_comparison) => Ok(
            ResolvedFilterExpression::LocalFieldComparison(local_field_comparison.clone()),
        ),
        plan_types::Expression::LocalNestedArray {
            predicate,
            field_path,
            column,
        } => {
            let resolved_predicate =
                plan_expression(predicate, relationships, remote_predicates, unique_number)?;
            Ok(ResolvedFilterExpression::LocalNestedArray {
                column: column.clone(),
                field_path: field_path.clone(),
                predicate: Box::new(resolved_predicate),
            })
        }
        plan_types::Expression::RelationshipLocalComparison {
            field_path,
            relationship,
            predicate,
            info,
        } => {
            let relationship_filter =
                plan_expression(predicate, relationships, remote_predicates, unique_number)?;
            relationships.insert(
                relationship.clone(),
                process_model_relationship_definition(info)?,
            );

            Ok(ResolvedFilterExpression::LocalRelationshipComparison {
                field_path: field_path.clone(),
                relationship: relationship.clone(),
                predicate: Box::new(relationship_filter),
            })
        }
        plan_types::Expression::RelationshipRemoteComparison {
            relationship: _,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            let (remote_query_node, rest_predicate_trees, collection_relationships) =
                plan_remote_predicate(ndc_column_mapping, predicate, unique_number)?;

            let query_execution_plan: QueryExecutionPlan = QueryExecutionPlan {
                query_node: remote_query_node,
                collection: target_model_source.collection.clone(),
                arguments: BTreeMap::new(),
                collection_relationships,
                variables: None,
                data_connector: target_model_source.data_connector.clone(),
            };

            let predicate_query_tree = PredicateQueryTree {
                ndc_column_mapping: ndc_column_mapping.clone(),
                target_model_name: (*target_model_name).clone(),
                query: ExecutionTree {
                    query_execution_plan,
                    remote_predicates: PredicateQueryTrees::new(),
                    remote_join_executions: JoinLocations::new(),
                },
                children: rest_predicate_trees,
            };

            let remote_predicate_id = remote_predicates.insert(unique_number, predicate_query_tree);

            Ok(ResolvedFilterExpression::RemoteRelationshipComparison {
                remote_predicate_id,
            })
        }
    }
}

/// Generate comparison expression plan for remote relationshp predicate.
pub fn plan_remote_predicate<'a>(
    ndc_column_mapping: &'a [plan_types::RelationshipColumnMapping],
    predicate: &'a plan_types::Expression<'_>,
    unique_number: &mut UniqueNumber,
) -> Result<
    (
        QueryNodeNew,
        PredicateQueryTrees,
        BTreeMap<NdcRelationshipName, Relationship>,
    ),
    plan_error::Error,
> {
    let mut relationships = BTreeMap::new();
    let mut remote_predicates = PredicateQueryTrees::new();
    let planned_predicate = plan_expression(
        predicate,
        &mut relationships,
        &mut remote_predicates,
        unique_number,
    )?;

    let query_node = QueryNodeNew {
        limit: None,
        offset: None,
        order_by: None,
        predicate: Some(planned_predicate),
        aggregates: None,
        fields: Some(FieldsSelection {
            fields: build_ndc_query_fields(ndc_column_mapping),
        }),
    };

    Ok((query_node, remote_predicates, relationships))
}

/// Generate the NDC query fields with the mapped NDC columns in a remote relationship.
/// These field values are fetched from the remote data connector.
fn build_ndc_query_fields(
    ndc_column_mapping: &[plan_types::RelationshipColumnMapping],
) -> IndexMap<NdcFieldAlias, Field> {
    let mut fields = IndexMap::new();
    for mapping in ndc_column_mapping {
        let field = Field::Column {
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
