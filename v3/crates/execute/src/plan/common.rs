use std::collections::BTreeMap;

use open_dds::types::DataConnectorArgumentName;

use super::error;
use crate::ir::arguments;
use crate::ir::filter;

pub fn ndc_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, arguments::Argument>,
) -> Result<BTreeMap<ndc_models::ArgumentName, ndc_models::Argument>, error::Error> {
    arguments
        .iter()
        .map(|(argument_name, argument_value)| {
            Ok((
                ndc_models::ArgumentName::from(argument_name.as_str()),
                ndc_argument(argument_value)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, error::Error>>()
}

pub fn ndc_raw_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, arguments::Argument>,
) -> Result<BTreeMap<ndc_models::ArgumentName, serde_json::Value>, error::Error> {
    arguments
        .iter()
        .map(|(argument_name, argument_value)| {
            Ok((
                ndc_models::ArgumentName::from(argument_name.as_str()),
                ndc_raw_argument(argument_value)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, error::Error>>()
}

pub fn ndc_relationship_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, arguments::Argument>,
) -> Result<BTreeMap<ndc_models::ArgumentName, ndc_models::RelationshipArgument>, error::Error> {
    arguments
        .iter()
        .map(|(argument_name, argument_value)| {
            Ok((
                ndc_models::ArgumentName::from(argument_name.as_str()),
                ndc_relationship_argument(argument_value)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, error::Error>>()
}

fn ndc_argument(
    argument_value: &arguments::Argument,
) -> Result<ndc_models::Argument, error::Error> {
    Ok(ndc_models::Argument::Literal {
        value: ndc_raw_argument(argument_value)?,
    })
}

fn ndc_relationship_argument(
    argument_value: &arguments::Argument,
) -> Result<ndc_models::RelationshipArgument, error::Error> {
    Ok(ndc_models::RelationshipArgument::Literal {
        value: ndc_raw_argument(argument_value)?,
    })
}

fn ndc_raw_argument(
    argument_value: &arguments::Argument,
) -> Result<serde_json::Value, error::Error> {
    match argument_value {
        arguments::Argument::Literal { value } => Ok(value.clone()),
        arguments::Argument::BooleanExpression { predicate } => {
            Ok(serde_json::to_value(ndc_expression(predicate))
                .map_err(error::InternalError::ExpressionSerializationError)?)
        }
    }
}

pub fn ndc_expression(expression: &filter::FilterExpression) -> ndc_models::Expression {
    match expression {
        filter::FilterExpression::And { expressions } => ndc_models::Expression::And {
            expressions: expressions.iter().map(ndc_expression).collect(),
        },
        filter::FilterExpression::Or { expressions } => ndc_models::Expression::Or {
            expressions: expressions.iter().map(ndc_expression).collect(),
        },
        filter::FilterExpression::Not { expression } => ndc_models::Expression::Not {
            expression: Box::new(ndc_expression(expression.as_ref())),
        },
        filter::FilterExpression::UnaryComparisonOperator {
            target: column,
            operator,
        } => ndc_models::Expression::UnaryComparisonOperator {
            column: ndc_comparison_target(column),
            operator: match operator {
                metadata_resolve::UnaryComparisonOperator::IsNull => {
                    ndc_models::UnaryComparisonOperator::IsNull
                }
            },
        },
        filter::FilterExpression::BinaryComparisonOperator {
            target: column,
            operator,
            value,
        } => ndc_models::Expression::BinaryComparisonOperator {
            column: ndc_comparison_target(column),
            operator: ndc_models::ComparisonOperatorName::from(operator.as_str()),
            value: ndc_comparison_value(value),
        },
        filter::FilterExpression::Exists {
            in_collection,
            predicate,
        } => ndc_models::Expression::Exists {
            in_collection: ndc_exists_in_collection(in_collection),
            predicate: Some(Box::new(ndc_expression(predicate.as_ref()))),
        },
    }
}

fn ndc_comparison_target(target: &filter::ComparisonTarget) -> ndc_models::ComparisonTarget {
    match target {
        filter::ComparisonTarget::Column { name, field_path } => {
            ndc_models::ComparisonTarget::Column {
                name: ndc_models::FieldName::from(name.as_str()),
                field_path: if field_path.is_empty() {
                    None
                } else {
                    Some(
                        field_path
                            .iter()
                            .map(|e| ndc_models::FieldName::from(e.as_str()))
                            .collect(),
                    )
                },
                path: vec![],
            }
        }
    }
}

fn ndc_comparison_value(value: &filter::ComparisonValue) -> ndc_models::ComparisonValue {
    match value {
        filter::ComparisonValue::Scalar { value } => ndc_models::ComparisonValue::Scalar {
            value: value.clone(),
        },
        filter::ComparisonValue::Variable { name } => ndc_models::ComparisonValue::Variable {
            name: ndc_models::VariableName::from(name.as_str()),
        },
    }
}

fn ndc_exists_in_collection(
    in_collection: &filter::ExistsInCollection,
) -> ndc_models::ExistsInCollection {
    match in_collection {
        filter::ExistsInCollection::Related { relationship } => {
            ndc_models::ExistsInCollection::Related {
                relationship: ndc_models::RelationshipName::from(relationship.as_str()),
                arguments: BTreeMap::new(),
            }
        }
    }
}
