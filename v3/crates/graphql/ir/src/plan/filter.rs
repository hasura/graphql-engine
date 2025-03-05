use super::error as plan_error;
use crate::FilterExpression;
use plan::plan_expression;
use plan_types::{
    NdcRelationshipName, PredicateQueryTrees, Relationship, ResolvedFilterExpression, UniqueNumber,
};
use std::collections::BTreeMap;

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
        expressions.push(
            plan_expression(filter, relationships, &mut remote_predicates, unique_number).map_err(
                |plan_error| {
                    plan_error::Error::Internal(plan_error::InternalError::InternalGeneric {
                        description: plan_error.to_string(),
                    })
                },
            )?,
        );
    }

    if let Some(filter) = &query_filter.additional_filter {
        expressions.push(
            plan_expression(filter, relationships, &mut remote_predicates, unique_number).map_err(
                |plan_error| {
                    plan_error::Error::Internal(plan_error::InternalError::InternalGeneric {
                        description: plan_error.to_string(),
                    })
                },
            )?,
        );
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

    if let Some(filter) = relationship_join_filter {
        expressions.push(
            plan_expression(filter, relationships, &mut remote_predicates, unique_number).map_err(
                |plan_error| {
                    plan_error::Error::Internal(plan_error::InternalError::InternalGeneric {
                        description: plan_error.to_string(),
                    })
                },
            )?,
        );
    }

    let resolved_filter_expression =
        ResolvedFilterExpression::mk_and(expressions).remove_always_true_expression();

    Ok((resolved_filter_expression, remote_predicates))
}
