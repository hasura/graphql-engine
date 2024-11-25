use std::collections::BTreeMap;

use super::error as plan_error;
use super::relationships::process_model_relationship_definition;
use crate::FilterExpression;
use plan_types::{
    NdcRelationshipName, PredicateQueryTrees, Relationship, ResolvedFilterExpression,
};

/// Plan the filter expression IR.
/// This function will take the filter expression IR and convert it into a planned filter expression
/// that can be converted the NDC filter expression.
/// This will record the relationships that are used in the filter expression.
/// TODO: combine this with resolve step as they are no longer separate
pub(crate) fn plan_filter_expression(
    FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter,
    }: &FilterExpression<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<Option<ResolvedFilterExpression>, plan_error::Error> {
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

    Ok(ResolvedFilterExpression::mk_and(expressions).remove_always_true_expression())
}

/// Plan the expression IR type.
pub fn plan_expression<'a>(
    expression: &'a plan_types::Expression<'_>,
    relationships: &'a mut BTreeMap<NdcRelationshipName, Relationship>,
    _remote_predicates: &'a mut PredicateQueryTrees,
) -> Result<ResolvedFilterExpression, plan_error::Error> {
    match expression {
        plan_types::Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(and_expression, relationships, _remote_predicates)?;
                results.push(result);
            }
            Ok(ResolvedFilterExpression::mk_and(results))
        }
        plan_types::Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(or_expression, relationships, _remote_predicates)?;
                results.push(result);
            }
            Ok(ResolvedFilterExpression::mk_or(results))
        }
        plan_types::Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(not_expression, relationships, _remote_predicates)?;
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
            let resolved_predicate = plan_expression(predicate, relationships, _remote_predicates)?;
            Ok(ResolvedFilterExpression::LocalNestedArray {
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

            Ok(ResolvedFilterExpression::LocalRelationshipComparison {
                relationship: relationship.clone(),
                predicate: Box::new(relationship_filter),
            })
        }
        plan_types::Expression::RelationshipRemoteComparison {
            relationship: _,
            target_model_name: _,
            target_model_source: _,
            ndc_column_mapping: _,
            predicate: _,
        } => {
            // TODO: Plan the remote relationship comparison expression and populate the remote_predicates

            // this is where the action will happen
            todo!("need to generate remote predicate plan");
            /*
            // This needs to be resolved in engine itself, further planning is deferred until it is resolved
            Ok(ResolvedFilterExpression::RemoteRelationshipComparison {
                relationship: relationship.clone(),
                target_model_name,
                target_model_source: target_model_source.clone(),
                ndc_column_mapping: ndc_column_mapping.clone(),
                predicate: predicate.clone(),
            })*/
        }
    }
}
