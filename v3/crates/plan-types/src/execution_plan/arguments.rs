use super::filter::ResolvedFilterExpression;

use crate::VariableName;
use std::hash::Hash;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Argument {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    /// The argument is provided by reference to a variable
    Variable {
        name: VariableName,
    },
    BooleanExpression {
        predicate: ResolvedFilterExpression,
    },
}

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum MutationArgument {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    BooleanExpression {
        predicate: ResolvedFilterExpression,
    },
}
