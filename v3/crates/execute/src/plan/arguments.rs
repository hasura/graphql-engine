use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error as plan_error;
use super::filter;
use super::relationships;
use crate::ir;
use crate::{
    error, ir::selection_set::NdcRelationshipName, remote_joins::types::VariableName, HttpContext,
};

pub type UnresolvedArgument<'s> = Argument<ir::filter::expression::Expression<'s>>;
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
        ir_argument: &'a ir::arguments::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            ir::arguments::Argument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
            ir::arguments::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(predicate, relationships)?;
                Argument::BooleanExpression {
                    predicate: expression,
                }
            }
        };
        Ok(planned_argument)
    }

    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ResolvedArgument, error::FieldError> {
        match self {
            Argument::Literal { value } => Ok(Argument::Literal { value }),
            Argument::Variable { name } => Ok(Argument::Variable { name }),
            Argument::BooleanExpression { predicate } => {
                let resolved_predicate =
                    filter::resolve_expression(predicate, http_context).await?;
                Ok(Argument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub type UnresolvedMutationArgument<'s> = MutationArgument<ir::filter::expression::Expression<'s>>;
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
        ir_argument: &'a ir::arguments::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            ir::arguments::Argument::Literal { value } => MutationArgument::Literal {
                value: value.clone(),
            },
            ir::arguments::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(predicate, relationships)?;
                MutationArgument::BooleanExpression {
                    predicate: expression,
                }
            }
        };
        Ok(planned_argument)
    }

    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ResolvedMutationArgument, error::FieldError> {
        match self {
            MutationArgument::Literal { value } => Ok(MutationArgument::Literal { value }),
            MutationArgument::BooleanExpression { predicate } => {
                let resolved_predicate =
                    filter::resolve_expression(predicate, http_context).await?;
                Ok(MutationArgument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub fn plan_arguments<'s>(
    arguments: &BTreeMap<DataConnectorArgumentName, ir::arguments::Argument<'s>>,
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
    arguments: &BTreeMap<DataConnectorArgumentName, ir::arguments::Argument<'s>>,
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
    http_context: &HttpContext,
    arguments: BTreeMap<
        DataConnectorArgumentName,
        Argument<ir::filter::expression::Expression<'s>>,
    >,
) -> Result<
    BTreeMap<DataConnectorArgumentName, Argument<filter::ResolvedFilterExpression>>,
    error::FieldError,
> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}

pub(crate) async fn resolve_mutation_arguments<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<
        DataConnectorArgumentName,
        MutationArgument<ir::filter::expression::Expression<'s>>,
    >,
) -> Result<
    BTreeMap<DataConnectorArgumentName, MutationArgument<filter::ResolvedFilterExpression>>,
    error::FieldError,
> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}
