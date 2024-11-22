use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error as plan_error;
use super::filter;
use super::filter::ResolveFilterExpressionContext;
use crate::error;
use plan_types::{NdcRelationshipName, PredicateQueryTrees, Relationship, VariableName};

pub type UnresolvedArgument<'s> = Argument<'s>;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum Argument<'s> {
    /// The argument is provided as a literal value
    Literal { value: serde_json::Value },
    /// The argument is provided by reference to a variable
    Variable { name: VariableName },
    BooleanExpression {
        predicate: plan_types::Expression<'s>,
    },
}

impl<'s> Argument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a graphql_ir::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            graphql_ir::Argument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
            graphql_ir::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(
                    predicate,
                    relationships,
                    &mut PredicateQueryTrees::new(),
                )?;
                Argument::BooleanExpression {
                    predicate: expression,
                }
            }
        };
        Ok(planned_argument)
    }

    pub async fn resolve(
        self,
        resolve_context: &ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::Argument, error::FieldError> {
        match self {
            Argument::Literal { value } => Ok(plan_types::Argument::Literal { value }),
            Argument::Variable { name } => Ok(plan_types::Argument::Variable { name }),
            Argument::BooleanExpression { predicate } => {
                let resolved_predicate =
                    filter::resolve_expression(predicate, resolve_context).await?;
                Ok(plan_types::Argument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub type UnresolvedMutationArgument<'s> = MutationArgument<'s>;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum MutationArgument<'s> {
    /// The argument is provided as a literal value
    Literal { value: serde_json::Value },
    BooleanExpression {
        predicate: plan_types::Expression<'s>,
    },
}

impl<'s> MutationArgument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a graphql_ir::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            graphql_ir::Argument::Literal { value } => MutationArgument::Literal {
                value: value.clone(),
            },
            graphql_ir::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(
                    predicate,
                    relationships,
                    &mut PredicateQueryTrees::new(),
                )?;
                MutationArgument::BooleanExpression {
                    predicate: expression,
                }
            }
        };
        Ok(planned_argument)
    }

    pub async fn resolve(
        self,
        resolve_context: &ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::MutationArgument, error::FieldError> {
        match self {
            MutationArgument::Literal { value } => {
                Ok(plan_types::MutationArgument::Literal { value })
            }
            MutationArgument::BooleanExpression { predicate } => {
                let resolved_predicate =
                    filter::resolve_expression(predicate, resolve_context).await?;
                Ok(plan_types::MutationArgument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub fn plan_arguments<'s>(
    arguments: &BTreeMap<DataConnectorArgumentName, graphql_ir::Argument<'s>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>, plan_error::Error> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name.clone(),
            Argument::plan(argument_value, relationships)?,
        );
    }
    Ok(result)
}

pub fn plan_mutation_arguments<'s>(
    arguments: &BTreeMap<DataConnectorArgumentName, graphql_ir::Argument<'s>>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedMutationArgument<'s>>, plan_error::Error>
{
    arguments
        .iter()
        .map(|(name, argument)| {
            Ok((
                name.clone(),
                MutationArgument::plan(argument, relationships)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, plan_error::Error>>()
}

pub(crate) async fn resolve_arguments<'s>(
    resolve_context: &ResolveFilterExpressionContext<'_>,
    arguments: BTreeMap<DataConnectorArgumentName, Argument<'s>>,
) -> Result<BTreeMap<DataConnectorArgumentName, plan_types::Argument>, error::FieldError> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name,
            argument_value.resolve(resolve_context).await?,
        );
    }
    Ok(result)
}

pub(crate) async fn resolve_mutation_arguments<'s>(
    resolve_context: &ResolveFilterExpressionContext<'_>,
    arguments: BTreeMap<DataConnectorArgumentName, MutationArgument<'s>>,
) -> Result<BTreeMap<DataConnectorArgumentName, plan_types::MutationArgument>, error::FieldError> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name,
            argument_value.resolve(resolve_context).await?,
        );
    }
    Ok(result)
}
