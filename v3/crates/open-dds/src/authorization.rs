use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{permissions::ValueExpression, types::FieldName};

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "Condition")]
/// A boolean expression used to determine if a rules-based permission
/// should be applied.
pub enum Condition {
    /// Combine multiple Conditions with `&&`
    And(Vec<Condition>),
    /// Combine multiple Conditions with `||`
    Or(Vec<Condition>),
    /// Negate a Condition
    Not(Box<Condition>),
    /// Compare two ValueExpressions for equality
    Equal {
        left: ValueExpression,
        right: ValueExpression,
    },
    /// Is the left value contained in the right value? The right value must be an array type.
    Contains {
        left: ValueExpression,
        right: ValueExpression,
    },
    /// Is the left value greater than the right value?
    GreaterThan {
        left: ValueExpression,
        right: ValueExpression,
    },
    /// Is the left value less than the right value?
    LessThan {
        left: ValueExpression,
        right: ValueExpression,
    },
    /// Is the left value greater than or equal to the right value?
    GreaterThanOrEqual {
        left: ValueExpression,
        right: ValueExpression,
    },
    /// Is the left value less than or equal to the right value?
    LessThanOrEqual {
        left: ValueExpression,
        right: ValueExpression,
    },
    /// Is the value null?
    IsNull { value: ValueExpression },
    /*RegexMatch {
        value: ValueExpression,
        pattern: RegexPattern,
    }*/
}

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "TypeAuthorizationRule")]
/// A rule that determines which fields of a type are available to a user
pub enum TypeAuthorizationRule {
    // if a condition is provided, it must evaluate to `true` for these fields
    // to be made available to the user
    AllowFields {
        fields: Vec<FieldName>,
        condition: Option<Condition>,
    },
    // if a condition is provided, it must evaluate to `true` for these fields
    // to be denied to the user. A denied field takes precedence over an allowed field.
    DenyFields {
        fields: Vec<FieldName>,
        condition: Option<Condition>,
    },
}
