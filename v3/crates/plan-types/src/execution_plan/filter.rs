use crate::{LocalFieldComparison, NdcRelationshipName, RemotePredicateKey};
use open_dds::data_connector::DataConnectorColumnName;

/// Filter expression plan to be resolved
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedFilterExpression {
    And {
        expressions: Vec<ResolvedFilterExpression>,
    },
    Or {
        expressions: Vec<ResolvedFilterExpression>,
    },
    Not {
        expression: Box<ResolvedFilterExpression>,
    },
    LocalFieldComparison(LocalFieldComparison),
    LocalNestedArray {
        column: DataConnectorColumnName,
        field_path: Vec<DataConnectorColumnName>,
        predicate: Box<ResolvedFilterExpression>,
    },
    LocalRelationshipComparison {
        field_path: Vec<DataConnectorColumnName>,
        relationship: NdcRelationshipName,
        predicate: Box<ResolvedFilterExpression>,
    },
    RemoteRelationshipComparison {
        remote_predicate_id: RemotePredicateKey,
    },
}

impl ResolvedFilterExpression {
    pub fn remove_always_true_expression(self) -> Option<ResolvedFilterExpression> {
        match &self {
            ResolvedFilterExpression::And { expressions } if expressions.is_empty() => None,
            ResolvedFilterExpression::Not { expression } => match expression.as_ref() {
                ResolvedFilterExpression::Or { expressions } if expressions.is_empty() => None,
                _ => Some(self),
            },
            _ => Some(self),
        }
    }

    /// Creates a 'FilterExpression::And' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_and(expressions: Vec<ResolvedFilterExpression>) -> ResolvedFilterExpression {
        // If the `and` only contains one expression, we can unwrap it and get rid of the `and`
        // ie. and([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `and`, we can flatten into a single `and`
        // ie. and([and([x,y]), and([a,b])]) == and([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, ResolvedFilterExpression::And { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    ResolvedFilterExpression::And { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            ResolvedFilterExpression::And {
                expressions: subexprs,
            }
        } else {
            ResolvedFilterExpression::And { expressions }
        }
    }

    /// Creates a 'FilterExpression::Or' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_or(expressions: Vec<ResolvedFilterExpression>) -> ResolvedFilterExpression {
        // If the `or` only contains one expression, we can unwrap it and get rid of the `or`
        // ie. or([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `or`, we can flatten into a single `or`
        // ie. or([or([x,y]), or([a,b])]) == or([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, ResolvedFilterExpression::Or { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    ResolvedFilterExpression::Or { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            ResolvedFilterExpression::Or {
                expressions: subexprs,
            }
        } else {
            ResolvedFilterExpression::Or { expressions }
        }
    }

    /// Creates a 'FilterExpression::Not' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_not(expression: ResolvedFilterExpression) -> ResolvedFilterExpression {
        match expression {
            // Double negations can be removed
            // ie. not(not(x))) == x
            ResolvedFilterExpression::Not { expression } => *expression,
            _ => ResolvedFilterExpression::Not {
                expression: Box::new(expression),
            },
        }
    }
}
