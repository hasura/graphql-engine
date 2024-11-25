use super::filter;
use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error as plan_error;
use plan_types::{
    Argument, MutationArgument, NdcRelationshipName, PredicateQueryTrees, Relationship,
};

/// Generate the argument plan from IR argument
pub fn plan_argument(
    ir_argument: &crate::Argument<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<Argument, super::error::Error> {
    match ir_argument {
        crate::Argument::Literal { value } => Ok(Argument::Literal {
            value: value.clone(),
        }),
        crate::Argument::BooleanExpression { predicate } => {
            let expression =
                filter::plan_expression(predicate, relationships, &mut PredicateQueryTrees::new())?;

            Ok(Argument::BooleanExpression {
                predicate: expression,
            })
        }
    }
}

/// Generate the argument plan from IR argument
#[allow(clippy::unnecessary_wraps)]
pub fn plan_mutation_argument(
    ir_argument: &crate::Argument<'_>,
    _relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<MutationArgument, super::error::Error> {
    let planned_argument = match ir_argument {
        crate::Argument::Literal { value } => MutationArgument::Literal {
            value: value.clone(),
        },
        crate::Argument::BooleanExpression { predicate: _ } => {
            todo!("here we'll need to plan straight into the new data types");
            /*
                            let expression = super::filter::plan_expression(
                                predicate,
                                relationships,
                                &mut PredicateQueryTrees::new(),
                            )?;

                            let resolved_predicate =
                                filter::resolve_expression(predicate, resolve_context).await?;
                            Ok(plan_types::MutationArgument::BooleanExpression {
                                predicate: resolved_predicate,
                            })
            */
        }
    };
    Ok(planned_argument)
}

pub fn plan_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, crate::Argument<'_>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<BTreeMap<DataConnectorArgumentName, Argument>, plan_error::Error> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name.clone(),
            plan_argument(argument_value, relationships)?,
        );
    }
    Ok(result)
}

pub fn plan_mutation_arguments(
    arguments: &BTreeMap<DataConnectorArgumentName, crate::Argument<'_>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<BTreeMap<DataConnectorArgumentName, MutationArgument>, plan_error::Error> {
    arguments
        .iter()
        .map(|(name, argument)| {
            Ok((
                name.clone(),
                plan_mutation_argument(argument, relationships)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, plan_error::Error>>()
}
