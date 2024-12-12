use std::collections::BTreeMap;

use super::error as plan_error;
use crate::order_by::OrderBy;
use plan::{plan_expression, process_model_relationship_definition};
use plan_types::{self, NdcRelationshipName, PredicateQueryTrees, Relationship, UniqueNumber};

pub fn plan_order_by(
    order_by: &OrderBy<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<
    (
        Vec<plan_types::OrderByElement<plan_types::ResolvedFilterExpression>>,
        PredicateQueryTrees,
    ),
    plan_error::Error,
> {
    let mut remote_predicates = PredicateQueryTrees::new();

    for (relationship, info) in &order_by.relationships {
        relationships.insert(
            relationship.clone(),
            process_model_relationship_definition(info).map_err(|plan_error| {
                plan_error::Error::Internal(plan_error::InternalError::InternalGeneric {
                    description: plan_error.to_string(),
                })
            })?,
        );
    }

    let order_by_elements = order_by
        .order_by_elements
        .iter()
        .map(|element| {
            Ok(plan_types::OrderByElement {
                order_direction: element.order_direction.clone(),
                target: plan_order_by_target(
                    &element.target,
                    relationships,
                    unique_number,
                    &mut remote_predicates,
                )?,
            })
        })
        .collect::<Result<Vec<_>, plan_error::Error>>()?;

    Ok((order_by_elements, remote_predicates))
}

fn plan_order_by_target(
    order_by_target: &plan_types::OrderByTarget<plan_types::Expression<'_>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
    remote_predicates: &mut PredicateQueryTrees,
) -> Result<plan_types::OrderByTarget<plan_types::ResolvedFilterExpression>, plan_error::Error> {
    let target = match order_by_target {
        plan_types::OrderByTarget::Column {
            name,
            field_path,
            relationship_path,
        } => plan_types::OrderByTarget::Column {
            relationship_path: relationship_path
                .iter()
                .map(|element| {
                    Ok(plan_types::RelationshipPathElement {
                        field_path: element.field_path.clone(),
                        relationship_name: element.relationship_name.clone(),
                        filter_predicate: element
                            .filter_predicate
                            .as_ref()
                            .map(|pred| {
                                plan_expression(
                                    pred,
                                    relationships,
                                    remote_predicates,
                                    unique_number,
                                )
                                .map_err(|plan_error| {
                                    plan_error::Error::Internal(
                                        plan_error::InternalError::InternalGeneric {
                                            description: plan_error.to_string(),
                                        },
                                    )
                                })
                            })
                            .transpose()?,
                    })
                })
                .collect::<Result<Vec<_>, plan_error::Error>>()?,
            name: name.clone(),
            field_path: field_path.clone(),
        },
    };

    Ok(target)
}
