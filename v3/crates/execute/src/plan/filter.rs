use indexmap::IndexMap;
use std::collections::BTreeMap;

use super::error;
use super::relationships::process_model_relationship_definition;
use super::types;
use crate::ir::filter;
use crate::ir::selection_set::{NdcFieldAlias, NdcRelationshipName};

/// Plan the filter expression IR.
/// This function will take the filter expression IR and convert it into a planned filter expression
/// that can be converted the NDC filter expression.
/// This will record the relationships that are used in the filter expression.
pub(crate) fn plan_filter_expression<'s>(
    filter::FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter,
    }: &filter::FilterExpression<'s>,
    relationships: &mut BTreeMap<NdcRelationshipName, types::Relationship>,
) -> Result<Option<filter::expression::Expression<'s>>, error::Error> {
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

    Ok(filter::expression::Expression::mk_and(expressions).remove_always_true_expression())
}

/// Plan the expression IR type.
pub fn plan_expression<'s, 'a>(
    expression: &'a filter::expression::Expression<'s>,
    relationships: &'a mut BTreeMap<NdcRelationshipName, types::Relationship>,
) -> Result<filter::expression::Expression<'s>, error::Error> {
    match expression {
        filter::expression::Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(and_expression, relationships)?;
                results.push(result);
            }
            Ok(filter::expression::Expression::mk_and(results))
        }
        filter::expression::Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(or_expression, relationships)?;
                results.push(result);
            }
            Ok(filter::expression::Expression::mk_or(results))
        }
        filter::expression::Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(not_expression, relationships)?;
            Ok(filter::expression::Expression::mk_not(result))
        }
        filter::expression::Expression::LocalField(local_field_comparison) => Ok(
            filter::expression::Expression::LocalField(local_field_comparison.clone()),
        ),
        filter::expression::Expression::LocalRelationship {
            relationship,
            predicate,
            info,
        } => {
            let relationship_filter = plan_expression(predicate, relationships)?;
            relationships.insert(
                relationship.clone(),
                process_model_relationship_definition(info)?,
            );

            Ok(filter::expression::Expression::LocalRelationship {
                relationship: relationship.clone(),
                predicate: Box::new(relationship_filter),
                info: info.clone(),
            })
        }
        filter::expression::Expression::RemoteRelationship {
            relationship,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            // This is a remote relationship, further planning is deferred until it is resolved
            Ok(filter::expression::Expression::RemoteRelationship {
                relationship: relationship.clone(),
                target_model_name,
                target_model_source,
                ndc_column_mapping: ndc_column_mapping.clone(),
                predicate: predicate.clone(),
            })
        }
    }
}

/// Generate comparison expression plan for remote relationshp predicate.
pub fn plan_remote_predicate<'s, 'a>(
    ndc_column_mapping: &'a [filter::expression::RelationshipColumnMapping],
    predicate: &'a filter::expression::Expression<'s>,
) -> Result<
    (
        types::UnresolvedQueryNode<'s>,
        BTreeMap<NdcRelationshipName, types::Relationship>,
    ),
    error::Error,
> {
    let mut relationships = BTreeMap::new();
    let planned_predicate = plan_expression(predicate, &mut relationships)?;

    let query_node = types::QueryNode {
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
    ndc_column_mapping: &[filter::expression::RelationshipColumnMapping],
) -> IndexMap<NdcFieldAlias, types::Field<'s, filter::expression::Expression<'s>>> {
    let mut fields = IndexMap::new();
    for mapping in ndc_column_mapping {
        let field = types::Field::Column {
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
