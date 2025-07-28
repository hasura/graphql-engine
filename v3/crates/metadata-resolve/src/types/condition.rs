use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::hash::{DefaultHasher, Hash, Hasher};

use crate::ValueExpression;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperation {
    Equals,
    Contains,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperation {
    IsNull,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
// An optimization we could consider in future is allowing referring to `ConditionHash`
// so that we can reuse common parts like `ValueExpression` comparisons.
pub enum Condition {
    All(Vec<Condition>),
    Any(Vec<Condition>),
    Not(Box<Condition>),
    BinaryOperation {
        op: BinaryOperation,
        left: ValueExpression,
        right: ValueExpression,
    },
    UnaryOperation {
        op: UnaryOperation,
        value: ValueExpression,
    },
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConditionHash(u64);

impl std::fmt::Display for ConditionHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// we collect all the `Condition` used in metadata by their Hash
// to deduplicate them
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Conditions {
    conditions: BTreeMap<ConditionHash, Condition>,
}
impl Default for Conditions {
    fn default() -> Self {
        Self::new()
    }
}

impl Conditions {
    pub fn new() -> Self {
        Self {
            conditions: BTreeMap::new(),
        }
    }

    pub fn get(&self, hash: &ConditionHash) -> Option<&Condition> {
        self.conditions.get(hash)
    }

    pub fn add(&mut self, condition: Condition) -> ConditionHash {
        let mut hasher = DefaultHasher::new();
        condition.hash(&mut hasher);
        let hash = ConditionHash(hasher.finish());

        self.conditions.insert(hash, condition);
        hash
    }
}
