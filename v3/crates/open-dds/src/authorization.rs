use opendds_derive;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    permissions::{ModelPredicate, ValueExpression, ValueExpressionOrPredicate},
    query::ArgumentName,
    spanned::Spanned,
    types::FieldName,
};

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "Condition"))]
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
    Equal(Comparison),
    /// Is the left value contained in the right value? The right value must be an array type.
    Contains(Comparison),
    /// Is the left value greater than the right value?
    GreaterThan(Comparison),
    /// Is the left value less than the right value?
    LessThan(Comparison),
    /// Is the left value greater than or equal to the right value?
    GreaterThanOrEqual(Comparison),
    /// Is the left value less than or equal to the right value?
    LessThanOrEqual(Comparison),
    /// Is the value null?
    IsNull(ValueExpression),
    /*RegexMatch {
        value: ValueExpression,
        pattern: RegexPattern,
    }*/
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "Comparison"))]
/// A left and right value for comparison
pub struct Comparison {
    pub left: ValueExpression,
    pub right: ValueExpression,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "TypeAuthorizationRule"))]
/// A rule that determines which fields of a type are available to a user
pub enum TypeAuthorizationRule {
    // if a condition is provided, it must evaluate to `true` for these fields
    // to be made available to the user
    AllowFields(Fields),
    // if a condition is provided, it must evaluate to `true` for these fields
    // to be denied to the user. A denied field takes precedence over an allowed field.
    DenyFields(Fields),
    // if a condition is provided, it must evaluate to `true` for this field
    // to be preset for the user.
    FieldPreset(InputTypeFieldPreset),
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "Fields"))]
pub struct Fields {
    pub fields: Vec<FieldName>,
    pub condition: Option<Condition>,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "InputTypeFieldPreset"))]
pub struct InputTypeFieldPreset {
    pub condition: Option<Condition>,
    pub field_name: FieldName,
    pub value: ValueExpression,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "CommandAuthorizationRule"))]
/// A rule that determines which commands and argument presets are available to a user
pub enum CommandAuthorizationRule {
    // if a condition is provided, it must evaluate to 'true' for
    // this Command to be available to the user
    Allow(Allow),
    // if the provided condition evaluates to 'true' this Command will not be available to the user
    Deny(Deny),
    // if a condition is provided, it must evaluate to `true` for this argument to be preset
    PresetArgument(PresetArgument),
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "Allow"))]
pub struct Allow {
    pub condition: Option<Condition>,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "Deny"))]
pub struct Deny {
    pub condition: Condition,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "PresetArgument"))]
pub struct PresetArgument {
    pub argument_name: Spanned<ArgumentName>,
    pub condition: Option<Condition>,
    pub value: Spanned<ValueExpressionOrPredicate>,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "ModelAuthorizationRule"))]
/// A rule that determines which model and argument presets are available to a user
pub enum ModelAuthorizationRule {
    // if a condition is provided, it must evaluate to 'true' for
    // this Model to be available to the user
    Allow(Allow),
    // if the provided condition evaluates to 'true' this Model will not be available to the user
    Deny(Deny),
    // if a condition is provided, it must evaluate to `true` for this argument to be preset
    PresetArgument(PresetArgument),
    // if a condition is provided, it must evaluate to `true` for this filter to be included in
    // requests. Multiple filters that match will be combined with `AND`
    Filter(Filter),
    // if a condition is provided, it must evaluate to `true` for these relational operations to be
    // available to the user
    AllowRelationalOperations(AllowRelationalOperations),
    // if the provided condition evaluates to `true` these relational operations will not be available
    // to the user
    DenyRelationalOperations(DenyRelationalOperations),
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "Filter"))]
pub struct Filter {
    pub condition: Option<Condition>,
    pub predicate: ModelPredicate,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd,
)]
#[schemars(title = "RelationalOperation")]
#[serde(rename_all = "camelCase")]
pub enum RelationalOperation {
    Insert,
    Update,
    Delete,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "AllowRelationalOperations"))]
pub struct AllowRelationalOperations {
    pub condition: Option<Condition>,
    pub operations: Vec<RelationalOperation>,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "DenyRelationalOperations"))]
pub struct DenyRelationalOperations {
    pub condition: Condition,
    pub operations: Vec<RelationalOperation>,
}
