use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error as plan_error;
use plan::{plan_expression, UnresolvedArgument};
use plan_types::{
    Argument, MutationArgument, NdcRelationshipName, PredicateQueryTrees, Relationship,
    UniqueNumber,
};

/// Generate the argument plan from IR argument
pub fn plan_argument(
    ir_argument: &UnresolvedArgument<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<(Argument, PredicateQueryTrees), plan_error::Error> {
    let mut remote_predicates = PredicateQueryTrees::new();
    let argument = match ir_argument {
        UnresolvedArgument::Literal { value } => {
            Ok::<Argument, plan_error::Error>(Argument::Literal {
                value: value.clone(),
            })
        }
        UnresolvedArgument::BooleanExpression { predicate } => {
            let expression = plan_expression(
                predicate,
                relationships,
                &mut remote_predicates,
                unique_number,
            )
            .map_err(|plan_error| {
                plan_error::Error::Internal(plan_error::InternalError::InternalGeneric {
                    description: plan_error.to_string(),
                })
            })?;

            Ok(Argument::BooleanExpression {
                predicate: expression,
            })
        }
    }?;

    Ok((argument, remote_predicates))
}

/// Generate the argument plan from IR argument
#[allow(clippy::unnecessary_wraps)]
pub fn plan_mutation_argument(
    ir_argument: &UnresolvedArgument<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<MutationArgument, plan_error::Error> {
    let mut remote_predicates = PredicateQueryTrees::new();

    let argument = match ir_argument {
        UnresolvedArgument::Literal { value } => {
            Ok::<MutationArgument, plan_error::Error>(MutationArgument::Literal {
                value: value.clone(),
            })
        }
        UnresolvedArgument::BooleanExpression { predicate } => {
            let expression = plan_expression(
                predicate,
                relationships,
                &mut remote_predicates,
                unique_number,
            )
            .map_err(|plan_error| {
                plan_error::Error::Internal(plan_error::InternalError::InternalGeneric {
                    description: plan_error.to_string(),
                })
            })?;

            Ok(MutationArgument::BooleanExpression {
                predicate: expression,
            })
        }
    }?;

    Ok(argument)
}

pub fn plan_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'_>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<
    (
        BTreeMap<DataConnectorArgumentName, Argument>,
        PredicateQueryTrees,
    ),
    plan_error::Error,
> {
    let mut result = BTreeMap::new();
    let mut remote_predicates = PredicateQueryTrees::new();

    for (argument_name, argument_value) in arguments {
        let (argument, argument_remote_predicates) =
            plan_argument(argument_value, relationships, unique_number)?;

        remote_predicates.0.extend(argument_remote_predicates.0);

        result.insert(argument_name.clone(), argument);
    }

    Ok((result, remote_predicates))
}

pub fn plan_mutation_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'_>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<BTreeMap<DataConnectorArgumentName, MutationArgument>, plan_error::Error> {
    arguments
        .iter()
        .map(|(name, argument)| {
            Ok((
                name.clone(),
                plan_mutation_argument(argument, relationships, unique_number)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, plan_error::Error>>()
}
