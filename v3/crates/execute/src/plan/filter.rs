use indexmap::IndexMap;
use std::collections::BTreeMap;

use super::error;
use super::relationships::process_model_relationship_definition;
use super::types;
use crate::ir::filter;

/// Plan the filter expression IR.
/// This function will take the filter expression IR and convert it into a planned filter expression
/// that can be converted the NDC filter expression.
/// This will record the relationships that are used in the filter expression.
pub(crate) fn plan_filter_expression<'s, 'a>(
    filter::FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter,
    }: &'a filter::FilterExpression<'s>,
    relationships: &'a mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
) -> Result<Option<types::FilterExpression<'s>>, error::Error> {
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

    Ok(types::FilterExpression::mk_and(expressions).remove_always_true_expression())
}

/// Plan the expression IR type.
pub fn plan_expression<'s, 'a>(
    expression: &'a filter::expression::Expression<'s>,
    relationships: &'a mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
) -> Result<types::FilterExpression<'s>, error::Error> {
    match expression {
        filter::expression::Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(and_expression, relationships)?;
                results.push(result);
            }
            Ok(types::FilterExpression::mk_and(results))
        }
        filter::expression::Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(or_expression, relationships)?;
                results.push(result);
            }
            Ok(types::FilterExpression::mk_or(results))
        }
        filter::expression::Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(not_expression, relationships)?;
            Ok(types::FilterExpression::mk_not(result))
        }
        filter::expression::Expression::LocalField(local_field_comparison) => {
            match local_field_comparison {
                filter::expression::LocalFieldComparison::UnaryComparison { column, operator } => {
                    let ndc_expression = ndc_models::Expression::UnaryComparisonOperator {
                        column: column.clone(),
                        operator: *operator,
                    };
                    Ok(types::FilterExpression::NDCComparison { ndc_expression })
                }
                filter::expression::LocalFieldComparison::BinaryComparison {
                    column,
                    operator,
                    value,
                } => {
                    let ndc_expression = ndc_models::Expression::BinaryComparisonOperator {
                        column: column.clone(),
                        operator: operator.clone(),
                        value: value.clone(),
                    };
                    Ok(types::FilterExpression::NDCComparison { ndc_expression })
                }
            }
        }
        filter::expression::Expression::LocalRelationship {
            relationship,
            arguments,
            predicate,
            info,
        } => {
            let relationship_filter = plan_expression(predicate, relationships)?;
            let relationship_name = ndc_models::RelationshipName::from(relationship.as_str());
            relationships.insert(
                relationship_name.clone(),
                process_model_relationship_definition(info)?,
            );

            let exists_in_relationship = ndc_models::ExistsInCollection::Related {
                relationship: relationship_name,
                arguments: arguments.clone(),
            };
            Ok(types::FilterExpression::LocalRelationshipComparison {
                exists_in_collection: exists_in_relationship,
                predicate: Box::new(relationship_filter),
            })
        }
        filter::expression::Expression::RemoteRelationship {
            relationship,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            let (remote_query_node, collection_relationships) =
                plan_remote_predicate(ndc_column_mapping, predicate)?;
            Ok(types::FilterExpression::RemoteRelationshipComparison {
                relationship_name: relationship.clone(),
                model_name: target_model_name.to_string(),
                ndc_column_mapping: ndc_column_mapping.clone(),
                remote_collection: ndc_models::CollectionName::from(
                    target_model_source.collection.as_str(),
                ),
                remote_query_node: Box::new(remote_query_node),
                collection_relationships,
                data_connector: &target_model_source.data_connector,
            })
        }
    }
}

/// Generate comparison expression plan for remote relationshp predicate.
fn plan_remote_predicate<'s, 'a>(
    ndc_column_mapping: &'a [filter::expression::RelationshipColumnMapping],
    predicate: &'a filter::expression::Expression<'s>,
) -> Result<
    (
        types::QueryNode<'s>,
        BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
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
        groups: None,
    };

    Ok((query_node, relationships))
}

/// Generate the NDC query fields with the mapped NDC columns in a remote relationship.
/// These field values are fetched from the remote data connector.
fn build_ndc_query_fields<'s>(
    ndc_column_mapping: &[filter::expression::RelationshipColumnMapping],
) -> IndexMap<ndc_models::FieldName, types::Field<'s>> {
    let mut fields = IndexMap::new();
    for mapping in ndc_column_mapping {
        let target_column_field = ndc_models::FieldName::from(mapping.target_ndc_column.as_str());
        let field = types::Field::Column {
            column: target_column_field.clone(),
            fields: None,
            arguments: BTreeMap::new(),
        };
        fields.insert(target_column_field, field);
    }
    fields
}
