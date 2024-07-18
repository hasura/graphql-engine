use metadata_resolve::Qualified;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::models::ModelName;
use open_dds::relationships::RelationshipName;
use serde::Serialize;
use std::collections::BTreeMap;

use crate::ir::relationship::LocalModelRelationshipInfo;
use crate::ir::selection_set::NDCRelationshipName;

/// Represent a boolean expression that can be used to filter data
#[derive(Debug, Serialize, Clone, PartialEq)]
pub enum Expression<'s> {
    And {
        expressions: Vec<Expression<'s>>,
    },
    Or {
        expressions: Vec<Expression<'s>>,
    },
    Not {
        expression: Box<Expression<'s>>,
    },
    LocalField(LocalFieldComparison),
    LocalRelationship {
        relationship: NDCRelationshipName,
        arguments: BTreeMap<ndc_models::ArgumentName, ndc_models::RelationshipArgument>,
        predicate: Box<Expression<'s>>,
        info: LocalModelRelationshipInfo<'s>,
    },
    RemoteRelationship {
        relationship: RelationshipName,
        target_model_name: &'s Qualified<ModelName>,
        target_model_source: &'s metadata_resolve::ModelSource,
        ndc_column_mapping: Vec<RelationshipColumnMapping>,
        predicate: Box<Expression<'s>>,
    },
}

impl<'s> Expression<'s> {
    /// Creates a 'Expression::And' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_and(expressions: Vec<Expression>) -> Expression {
        // If the `and` only contains one expression, we can unwrap it and get rid of the `and`
        // ie. and([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `and`, we can flatten into a single `and`
        // ie. and([and([x,y]), and([a,b])]) == and([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, Expression::And { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    Expression::And { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            Expression::And {
                expressions: subexprs,
            }
        } else {
            Expression::And { expressions }
        }
    }

    /// Creates a 'Expression::Or' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_or(expressions: Vec<Expression>) -> Expression {
        // If the `or` only contains one expression, we can unwrap it and get rid of the `or`
        // ie. or([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `or`, we can flatten into a single `or`
        // ie. or([or([x,y]), or([a,b])]) == or([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, Expression::Or { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    Expression::Or { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            Expression::Or {
                expressions: subexprs,
            }
        } else {
            Expression::Or { expressions }
        }
    }

    /// Creates a 'Expression::Not' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_not(expression: Expression) -> Expression {
        match expression {
            // Double negations can be removed
            // ie. not(not(x))) == x
            Expression::Not { expression } => *expression,
            _ => Expression::Not {
                expression: Box::new(expression),
            },
        }
    }
}

/// Represent a local field (column) comparison
#[derive(Debug, Serialize, Clone, PartialEq)]
pub enum LocalFieldComparison {
    /// A comparison with just a field without a target value
    UnaryComparison {
        column: ndc_models::ComparisonTarget,
        operator: ndc_models::UnaryComparisonOperator,
    },
    /// A comparison between a field and a value
    BinaryComparison {
        column: ndc_models::ComparisonTarget,
        operator: ndc_models::ComparisonOperatorName,
        value: ndc_models::ComparisonValue,
    },
}

/// Represent a mapping between a source and target NDC columns
#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct RelationshipColumnMapping {
    pub source_ndc_column: SourceNdcColumn,
    pub target_ndc_column: DataConnectorColumnName,
}

/// Represent a source NDC column in a mapping
#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct SourceNdcColumn {
    /// Column name
    pub column: DataConnectorColumnName,
    /// Field path if the column is part of nested object type
    pub field_path: Option<Vec<ndc_models::FieldName>>,
    /// An equality operator needed for resolving remote relationship predicate
    pub eq_operator: ndc_models::ComparisonOperatorName,
}
