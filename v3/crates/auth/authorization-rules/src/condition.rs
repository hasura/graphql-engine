//! this is where we evaluate Conditions

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
}

// evaluate conditions used in permissions
// enough to evaluate `x-hasura-role` == "some-string" and not much else
// fortunately that's all we need for now
fn evaluate_condition(
    condition: &Condition,
    session_variables: &SessionVariables,
) -> Result<bool, ConditionError> {
    match condition {
        Condition::And(conditions) => conditions.iter().try_fold(true, |acc, condition| {
            Ok(acc && evaluate_condition(condition, session_variables)?)
        }),
        Condition::Or(conditions) => conditions.iter().try_fold(true, |acc, condition| {
            Ok(acc || evaluate_condition(condition, session_variables)?)
        }),
        Condition::Not(condition) => {
            let value = evaluate_condition(condition, session_variables)?;
            Ok(!value)
        }
        Condition::UnaryOperation { op, value: _ } => match op {
            UnaryOperation::IsNull => todo!("UnaryOperation::IsNull"),
        },
        Condition::BinaryOperation { left, right, op } => {
            let left = evaluate_value_expression(left, session_variables)?;
            let right = evaluate_value_expression(right, session_variables)?;
            Ok(match op {
                BinaryOperation::Equals => left == right,
                BinaryOperation::Contains => todo!("BinaryOperation::Contains"),
                BinaryOperation::GreaterThan => todo!("BinaryOperation::GreaterThan"),
                BinaryOperation::LessThan => todo!("BinaryOperation::LessThan"),
                BinaryOperation::GreaterThanOrEqual => todo!("BinaryOperation::GreaterThanOrEqual"),
                BinaryOperation::LessThanOrEqual => todo!("BinaryOperation::LessThanOrEqual"),
            })
        }
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

        let string = |s: &str| ValueExpression::Literal(serde_json::Value::String(s.to_string()));

        let not = |condition: Condition| Condition::Not(Box::new(condition));

        let and = |conditions: Vec<Condition>| Condition::And(conditions);

        let or = |conditions: Vec<Condition>| Condition::Or(conditions);

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

        let test_cases_that_should_succeed = vec![
            equals(true_val.clone(), true_val.clone()),
            equals(session_variable("x-hasura-role"), string("user")),
            not(equals(session_variable("x-hasura-role"), string("admin"))),
            and(vec![
                equals(true_val.clone(), true_val.clone()),
                equals(true_val.clone(), true_val.clone()),
            ]),
            or(vec![
                equals(false_val.clone(), true_val.clone()),
                equals(true_val.clone(), true_val.clone()),
            ]),
            not(equals(false_val, true_val)),
        ];

        // return an arbitrary identity with role emulation enabled
        let authorization = Identity::admin(Role::new("user"));
        let role = Role::new("user");
        let role_authorization = authorization.get_role_authorization(Some(&role)).unwrap();

        let session = role_authorization.build_session(BTreeMap::new());

        for test in test_cases_that_should_succeed {
            assert_eq!(evaluate_condition(&test, &session.variables), Ok(true));
        }
    }
}
