use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error as plan_error;
use super::filter;
use super::filter::PredicateQueryTrees;
use super::filter::ResolveFilterExpressionContext;
use super::relationships;
use crate::error;
use plan_types::NdcRelationshipName;
use plan_types::VariableName;

pub type UnresolvedArgument<'s> = Argument<plan_types::Expression<'s>>;
pub type ResolvedArgument = Argument<filter::ResolvedFilterExpression>;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum Argument<TFilterExpression> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    /// The argument is provided by reference to a variable
    Variable {
        name: VariableName,
    },
    BooleanExpression {
        predicate: TFilterExpression,
    },
}

impl<'s> UnresolvedArgument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a graphql_ir::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
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
    ) -> Result<ResolvedArgument, error::FieldError> {
        match self {
            Argument::Literal { value } => Ok(Argument::Literal { value }),
            Argument::Variable { name } => Ok(Argument::Variable { name }),
            Argument::BooleanExpression { predicate } => {
                let resolved_predicate =
                    filter::resolve_expression(predicate, resolve_context).await?;
                Ok(Argument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub type UnresolvedMutationArgument<'s> = MutationArgument<plan_types::Expression<'s>>;
pub type ResolvedMutationArgument = MutationArgument<filter::ResolvedFilterExpression>;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum MutationArgument<TFilterExpression> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    BooleanExpression {
        predicate: TFilterExpression,
    },
}

impl<'s> UnresolvedMutationArgument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a graphql_ir::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
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
    ) -> Result<ResolvedMutationArgument, error::FieldError> {
        match self {
            MutationArgument::Literal { value } => Ok(MutationArgument::Literal { value }),
            MutationArgument::BooleanExpression { predicate } => {
                let resolved_predicate =
                    filter::resolve_expression(predicate, resolve_context).await?;
                Ok(MutationArgument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub fn plan_arguments<'s>(
    arguments: &BTreeMap<DataConnectorArgumentName, graphql_ir::Argument<'s>>,
    relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
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
    relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
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
    arguments: BTreeMap<DataConnectorArgumentName, Argument<plan_types::Expression<'s>>>,
) -> Result<
    BTreeMap<DataConnectorArgumentName, Argument<filter::ResolvedFilterExpression>>,
    error::FieldError,
> {
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
    arguments: BTreeMap<DataConnectorArgumentName, MutationArgument<plan_types::Expression<'s>>>,
) -> Result<
    BTreeMap<DataConnectorArgumentName, MutationArgument<filter::ResolvedFilterExpression>>,
    error::FieldError,
> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name,
            argument_value.resolve(resolve_context).await?,
        );
    }
    Ok(result)
}
