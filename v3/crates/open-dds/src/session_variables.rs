/// Module that defines the Hasura session variables.
use core::fmt;
use std::convert::Infallible;
use std::str::FromStr;

use lazy_static::lazy_static;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Used to represent the name of a session variable, like
/// "x-hasura-role".
#[derive(Debug, Clone, Hash, PartialEq, Eq, JsonSchema, Serialize, Deserialize)]
#[schemars(rename = "OpenDdSessionVariable")]
pub struct SessionVariable(String);

lazy_static! {
    pub static ref SESSION_VARIABLE_ROLE: SessionVariable =
        SessionVariable("x-hasura-role".to_string());
}

impl FromStr for SessionVariable {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SessionVariable(s.trim().to_lowercase()))
    }
}

impl fmt::Display for SessionVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            SessionVariable("test-role".to_string()),
            SessionVariableValue("test-role".to_string()),
        );
        let json_str = serde_json::to_string(&session_variables).unwrap();

        let parsed_from_string: HashMap<SessionVariable, SessionVariableValue> =
            serde_json::from_str(json_str.trim()).unwrap();

        assert_eq!(parsed_from_string, session_variables)
    }
}
