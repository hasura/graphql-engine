//! Module that defines the Hasura session variables.

use std::borrow::Cow;
use std::convert::Infallible;
use std::str::FromStr;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Used to represent the name of a session variable, like
/// "x-hasura-role".
#[derive(Debug, Clone, Hash, PartialEq, Eq, JsonSchema, Serialize, Deserialize)]
#[schemars(rename = "OpenDdSessionVariable")]
pub struct SessionVariable(Cow<'static, str>);

pub const SESSION_VARIABLE_ROLE: SessionVariable = SessionVariable(Cow::Borrowed("x-hasura-role"));

impl FromStr for SessionVariable {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SessionVariable(s.trim().to_lowercase().into()))
    }
}

impl std::fmt::Display for SessionVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// New type wrapper over String to represent
/// a session variable value.
#[derive(Serialize, Deserialize, Clone, Hash, PartialEq, Eq, Debug)]
pub struct SessionVariableValue(pub String);

impl From<&str> for SessionVariableValue {
    fn from(value: &str) -> Self {
        SessionVariableValue(value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    #[cfg(test)]
    use pretty_assertions::assert_eq;
    use serde_json;

    use super::*;

    #[test]
    fn serialize_and_deserialize_session_variable() {
        let mut session_variables = HashMap::new();
        session_variables.insert(
            SessionVariable("test-role".into()),
            SessionVariableValue("test-role".into()),
        );
        let json_str = serde_json::to_string(&session_variables).unwrap();

        let parsed_from_string: HashMap<SessionVariable, SessionVariableValue> =
            serde_json::from_str(json_str.trim()).unwrap();

        assert_eq!(parsed_from_string, session_variables);
    }
}
