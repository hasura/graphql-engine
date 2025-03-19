//! Module that defines the Hasura session variables.

use std::borrow::Cow;
use std::convert::Infallible;
use std::str::FromStr;

use serde::{Deserialize, Serialize};

use crate::impl_JsonSchema_with_OpenDd_for;

/// Used to represent a reference to a session variable,
/// where one is required in the IR
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdSessionVariableReference"))]
pub struct SessionVariableReference {
    pub name: SessionVariableName,
    pub passed_as_json: bool,
    pub disallow_unknown_fields: bool,
}

/// Used to represent the name of a session variable, like
/// "x-hasura-role".
#[derive(
    Debug,
    Clone,
    Hash,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    PartialOrd,
    Ord,
    opendds_derive::OpenDd,
)]
#[opendd(json_schema(rename = "OpenDdSessionVariable"))]
pub struct SessionVariableName(Cow<'static, str>);

impl_JsonSchema_with_OpenDd_for!(SessionVariableName);

pub const SESSION_VARIABLE_ROLE: SessionVariableName =
    SessionVariableName(Cow::Borrowed("x-hasura-role"));

impl FromStr for SessionVariableName {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SessionVariableName(s.trim().to_lowercase().into()))
    }
}

impl std::fmt::Display for SessionVariableName {
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
            SessionVariableName("test-role".into()),
            SessionVariableValue("test-role".into()),
        );
        let json_str = serde_json::to_string(&session_variables).unwrap();

        let parsed_from_string: HashMap<SessionVariableName, SessionVariableValue> =
            serde_json::from_str(json_str.trim()).unwrap();

        assert_eq!(parsed_from_string, session_variables);
    }
}
