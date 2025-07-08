//! this is where we evaluate Conditions

use std::fmt::Display;

use hasura_authn_core::{SessionVariableName, SessionVariables};

use metadata_resolve::{
    BinaryOperation, Condition, ConditionHash, Conditions, UnaryOperation, ValueExpression,
};

use crate::ConditionCache;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ConditionError {
    #[error("Session variable not found: {name}")]
    SessionVariableNotFound { name: SessionVariableName },
    #[error("Serde error: {error}")]
    SerdeError { error: String },
    #[error("Condition {condition_hash} not found")]
    ConditionNotFound { condition_hash: ConditionHash },
    #[error("Expected array or null for right-hand value of contains operation")]
    ExpectedArrayOrNullForContains,
    #[error("Expected number for {side}-hand value of comparison operation")]
    ExpectedNumberForComparison { side: Side },
    #[error(
        "Number for {side}-hand value of comparison operation is outside precision or range of a double-precision float"
    )]
    NumberOutOfRange { side: Side },
}

// evaluate conditions used in permissions
fn evaluate_condition(
    condition: &Condition,
    session_variables: &SessionVariables,
) -> Result<bool, ConditionError> {
    match condition {
        Condition::All(conditions) => conditions.iter().try_fold(true, |acc, condition| {
            Ok(acc && evaluate_condition(condition, session_variables)?)
        }),
        Condition::Any(conditions) => conditions.iter().try_fold(false, |acc, condition| {
            Ok(acc || evaluate_condition(condition, session_variables)?)
        }),
        Condition::Not(condition) => {
            let value = evaluate_condition(condition, session_variables)?;
            Ok(!value)
        }
        Condition::UnaryOperation { op, value } => match op {
            UnaryOperation::IsNull => {
                Ok(evaluate_value_expression(value, session_variables)?.is_null())
            }
        },
        Condition::BinaryOperation { left, right, op } => {
            let left = evaluate_value_expression(left, session_variables)?;
            let right = evaluate_value_expression(right, session_variables)?;
            binary_operation(&left, op, &right)
        }
    }
}

fn binary_operation(
    left: &serde_json::Value,
    op: &BinaryOperation,
    right: &serde_json::Value,
) -> Result<bool, ConditionError> {
    match op {
        BinaryOperation::Equals => Ok(left == right),
        BinaryOperation::Contains => match right {
            serde_json::Value::Array(values) => Ok(values.contains(left)),
            serde_json::Value::Null => Ok(false),
            _ => Err(ConditionError::ExpectedArrayOrNullForContains),
        },
        BinaryOperation::GreaterThan => {
            Ok(as_float(left, Side::Left)? > as_float(right, Side::Right)?)
        }
        BinaryOperation::LessThan => {
            Ok(as_float(left, Side::Left)? < as_float(right, Side::Right)?)
        }
        BinaryOperation::GreaterThanOrEqual => {
            Ok(as_float(left, Side::Left)? >= as_float(right, Side::Right)?)
        }
        BinaryOperation::LessThanOrEqual => {
            Ok(as_float(left, Side::Left)? <= as_float(right, Side::Right)?)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

impl Display for Side {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Side::Left => write!(f, "left"),
            Side::Right => write!(f, "right"),
        }
    }
}

fn as_float(value: &serde_json::Value, side: Side) -> Result<f64, ConditionError> {
    let serde_json::Value::Number(number) = value else {
        return Err(ConditionError::ExpectedNumberForComparison { side });
    };

    if let Some(f) = number.as_f64() {
        Ok(f)
    } else {
        Err(ConditionError::NumberOutOfRange { side })
    }
}

// evaluate a condition, saving the result in a cache
// to save recomputation
pub fn evaluate_condition_hash(
    condition_hash: &ConditionHash,
    session_variables: &SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<bool, ConditionError> {
    // first lookup result, we can end this here
    if let Some(result) = condition_cache.get(condition_hash) {
        return Ok(result);
    }

    // lookup condition
    let condition = conditions
        .get(condition_hash)
        .ok_or(ConditionError::ConditionNotFound {
            condition_hash: *condition_hash,
        })?;

    let result = evaluate_condition(condition, session_variables)?;

    condition_cache.set(*condition_hash, result);

    Ok(result)
}

// we should probably replace this with `make_argument_from_value_expression` from `plan`
// which has more complete correct JSON
fn evaluate_value_expression(
    value_expression: &ValueExpression,
    session_variables: &SessionVariables,
) -> Result<serde_json::Value, ConditionError> {
    match value_expression {
        ValueExpression::Literal(value) => Ok(value.clone()),
        ValueExpression::SessionVariable(reference) => {
            let session_variable_value = session_variables.get(&reference.name).ok_or(
                ConditionError::SessionVariableNotFound {
                    name: reference.name.clone(),
                },
            )?;

            let value = if reference.passed_as_json {
                session_variable_value
                    .as_value()
                    .map_err(|error| ConditionError::SerdeError {
                        error: error.to_string(),
                    })?
            } else {
                // In v1 (ie before json type support in session variables), we expect every session
                // variable to arrive as a string and then we parse that string into whatever type we need
                let session_var_value = session_variable_value.as_str().unwrap();
                // whilst we are only porting `x-hasura-role` comparisons, we can always assume it's a string
                // however we should use the `make_argument_from_value_expression` function from `plan` instead
                // once we start using the authorization rules properly
                serde_json::Value::String(session_var_value.to_string())
            };
            Ok(value)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use derive_more::FromStr;
    use std::collections::BTreeMap;

    use hasura_authn_core::{Identity, Role, SessionVariableName, SessionVariableReference};
    use metadata_resolve::{BinaryOperation, Condition, ValueExpression};

    #[test]
    fn test_evaluate_condition() {
        let true_val = ValueExpression::Literal(serde_json::Value::Bool(true));

        let false_val = ValueExpression::Literal(serde_json::Value::Bool(false));

        let null_val = ValueExpression::Literal(serde_json::Value::Null);

        let string = |s: &str| ValueExpression::Literal(serde_json::Value::String(s.to_string()));

        let number = |f: f64| {
            ValueExpression::Literal(serde_json::Value::Number(
                serde_json::Number::from_f64(f).unwrap(),
            ))
        };

        let array_of_strings = |values: Vec<&str>| {
            ValueExpression::Literal(serde_json::Value::Array(
                values
                    .into_iter()
                    .map(|s| serde_json::Value::String(s.to_string()))
                    .collect(),
            ))
        };

        let not = |condition: Condition| Condition::Not(Box::new(condition));

        let all = |conditions: Vec<Condition>| Condition::All(conditions);

        let any = |conditions: Vec<Condition>| Condition::Any(conditions);

        let session_variable = |name: &str| {
            ValueExpression::SessionVariable(SessionVariableReference {
                name: SessionVariableName::from_str(name).unwrap(),
                disallow_unknown_fields: true,
                passed_as_json: true,
            })
        };

        let equals = |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
            left,
            right,
            op: BinaryOperation::Equals,
        };

        let contains = |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
            left,
            right,
            op: BinaryOperation::Contains,
        };

        let greater_than =
            |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
                left,
                right,
                op: BinaryOperation::GreaterThan,
            };

        let less_than =
            |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
                left,
                right,
                op: BinaryOperation::LessThan,
            };

        let less_than_or_equal =
            |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
                left,
                right,
                op: BinaryOperation::LessThanOrEqual,
            };

        let greater_than_or_equal =
            |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
                left,
                right,
                op: BinaryOperation::GreaterThanOrEqual,
            };

        let is_null = |value: ValueExpression| Condition::UnaryOperation {
            op: UnaryOperation::IsNull,
            value,
        };

        let test_cases_that_should_succeed = vec![
            equals(true_val.clone(), true_val.clone()),
            equals(session_variable("x-hasura-role"), string("user")),
            contains(
                session_variable("x-hasura-role"),
                array_of_strings(vec!["user"]),
            ),
            greater_than(number(1.0), number(0.0)),
            greater_than_or_equal(number(1.0), number(0.0)),
            greater_than_or_equal(number(1.0), number(1.0)),
            less_than(number(0.0), number(1.0)),
            less_than_or_equal(number(0.0), number(1.0)),
            less_than_or_equal(number(0.0), number(0.0)),
            not(equals(session_variable("x-hasura-role"), string("admin"))),
            all(vec![
                equals(true_val.clone(), true_val.clone()),
                equals(true_val.clone(), true_val.clone()),
            ]),
            any(vec![
                equals(false_val.clone(), true_val.clone()),
                equals(true_val.clone(), true_val.clone()),
            ]),
            not(equals(false_val.clone(), true_val.clone())),
            is_null(null_val.clone()),
        ];

        // return an arbitrary identity with role emulation enabled
        let authorization = Identity::admin(Role::new("user"));
        let role = Role::new("user");
        let role_authorization = authorization.get_role_authorization(Some(&role)).unwrap();

        let session = role_authorization.build_session(BTreeMap::new());

        for test in test_cases_that_should_succeed {
            assert_eq!(evaluate_condition(&test, &session.variables), Ok(true));
        }

        let test_cases_that_should_fail = vec![
            equals(true_val.clone(), false_val.clone()),
            equals(session_variable("x-hasura-role"), string("not-user")),
            contains(
                session_variable("x-hasura-role"),
                array_of_strings(vec!["loser"]),
            ),
            contains(session_variable("x-hasura-role"), null_val.clone()),
            greater_than(number(0.0), number(1.0)),
            greater_than_or_equal(number(0.0), number(1.0)),
            less_than(number(1.0), number(0.0)),
            less_than_or_equal(number(1.0), number(0.0)),
            not(equals(session_variable("x-hasura-role"), string("user"))),
            all(vec![
                equals(true_val.clone(), true_val.clone()),
                equals(true_val.clone(), false_val.clone()),
            ]),
            any(vec![equals(false_val.clone(), true_val)]),
            any(vec![]),
            not(equals(false_val.clone(), false_val)),
            not(is_null(null_val)),
        ];

        for test in test_cases_that_should_fail {
            assert_eq!(evaluate_condition(&test, &session.variables), Ok(false));
        }
    }
}
