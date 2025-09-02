use axum::Extension;
use axum::middleware::Next;
use axum::{
    extract::State,
    http::StatusCode,
    http::{HeaderMap, Request},
};
use axum_core::body::Body;
use schemars::JsonSchema;
use std::collections::{BTreeMap, btree_map};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    str::FromStr,
};

/// The architecture is as follows:
/// 1. Every authn mechanism returns an 'Identity'.
///    An 'Identity' lists the allowed roles and a default role
///
/// 2. The identity is then resolved to a 'session'
///    The client could pick a specific role from the roles allowed in the session.
// Session variable and role are defined as part of OpenDD
pub use open_dds::{
    permissions::Role,
    session_variables::{SESSION_VARIABLE_ROLE, SessionVariableName, SessionVariableReference},
};

#[derive(
    Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize, derive_more::Display,
)]
/// Value of a session variable, used to capture session variable input from parsed sources (jwt, webhook, etc)
/// and unparsed sources (http headers)
pub enum SessionVariableValue {
    /// An unparsed session variable value as a string. Might be a raw string, might be a number, might be json.
    /// How we interpret it depends on what type we're trying to coerce to from the string
    #[display("{_0}")]
    Unparsed(String),
    /// A parsed JSON session variable value. We know what the type is because we parsed it from JSON.
    #[display("{_0}")]
    Parsed(serde_json::Value),
}

impl SessionVariableValue {
    pub fn new(value: &str) -> Self {
        SessionVariableValue::Unparsed(value.to_string())
    }

    /// Assert that a session variable represents a string, regardless of encoding
    pub fn as_str(&self) -> Option<&str> {
        match self {
            SessionVariableValue::Unparsed(s) => Some(s.as_str()),
            SessionVariableValue::Parsed(value) => value.as_str(),
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            SessionVariableValue::Unparsed(s) => s.parse::<i64>().ok(),
            SessionVariableValue::Parsed(value) => value.as_i64(),
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            SessionVariableValue::Unparsed(s) => s.parse::<f64>().ok(),
            SessionVariableValue::Parsed(value) => value.as_f64(),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            SessionVariableValue::Unparsed(s) => s.parse::<bool>().ok(),
            SessionVariableValue::Parsed(value) => value.as_bool(),
        }
    }

    pub fn as_value(&self) -> serde_json::Result<serde_json::Value> {
        match self {
            SessionVariableValue::Unparsed(s) => serde_json::from_str(s),
            SessionVariableValue::Parsed(value) => Ok(value.clone()),
        }
    }
}

impl From<JsonSessionVariableValue> for SessionVariableValue {
    fn from(value: JsonSessionVariableValue) -> Self {
        SessionVariableValue::Parsed(value.0)
    }
}

/// JSON value of a session variable
// This is used instead of SessionVariableValue when only JSON session variable values are accepted
#[derive(Debug, serde::Serialize, serde::Deserialize, PartialEq, Clone, JsonSchema)]
#[schemars(rename = "SessionVariableValue")] // Renamed to keep json schema compatibility
pub struct JsonSessionVariableValue(pub serde_json::Value);

#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SessionVariables(BTreeMap<SessionVariableName, SessionVariableValue>);

impl SessionVariables {
    pub fn get(&self, session_variable: &SessionVariableName) -> Option<&SessionVariableValue> {
        self.0.get(session_variable)
    }

    pub fn iter(&self) -> btree_map::Iter<'_, SessionVariableName, SessionVariableValue> {
        self.0.iter()
    }
}

impl IntoIterator for SessionVariables {
    type Item = (SessionVariableName, SessionVariableValue);
    type IntoIter = btree_map::IntoIter<SessionVariableName, SessionVariableValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a SessionVariables {
    type Item = (
        &'a open_dds::session_variables::SessionVariableName,
        &'a SessionVariableValue,
    );
    type IntoIter =
        btree_map::Iter<'a, open_dds::session_variables::SessionVariableName, SessionVariableValue>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
// The privilege with which a request is executed
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Session {
    pub role: Role,
    pub variables: SessionVariables,
}

// Privileges of a role
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RoleAuthorization {
    pub role: Role,
    pub session_variables: HashMap<SessionVariableName, SessionVariableValue>,
    pub allowed_session_variables_from_request: SessionVariableList,
}

impl RoleAuthorization {
    pub fn build_session(
        &self,
        mut variables: BTreeMap<SessionVariableName, SessionVariableValue>,
    ) -> Session {
        let allowed_client_session_variables = match &self.allowed_session_variables_from_request {
            SessionVariableList::All => variables,
            SessionVariableList::Some(allowed) => {
                variables.retain(|variable, _value| allowed.contains(variable));
                variables
            }
        };
        let mut session_variables = allowed_client_session_variables;
        session_variables.extend(self.session_variables.clone());

        session_variables.insert(
            SESSION_VARIABLE_ROLE,
            SessionVariableValue::Parsed(serde_json::json!(self.role.to_string())),
        );

        Session {
            role: self.role.clone(),
            variables: SessionVariables(session_variables),
        }
    }
}

// A list of session variables
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SessionVariableList {
    // like * in Select * from ...
    All,
    // An explicit list
    Some(HashSet<SessionVariableName>),
}

// Privileges of the current user
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Identity {
    // The name of the role for which role emulation is enabled
    RoleEmulationEnabled(Role),
    Specific {
        default_role: Role,
        allowed_roles: HashMap<Role, RoleAuthorization>,
    },
}

impl Identity {
    pub fn admin(role: Role) -> Self {
        Identity::RoleEmulationEnabled(role)
    }
    // Builds a session
    pub fn get_role_authorization(
        &self,
        role: Option<&Role>,
    ) -> Result<Cow<RoleAuthorization>, SessionError> {
        match self {
            Identity::RoleEmulationEnabled(admin_role) => Ok(Cow::Owned(RoleAuthorization {
                role: role.cloned().unwrap_or(admin_role.clone()),
                session_variables: HashMap::new(),
                allowed_session_variables_from_request: SessionVariableList::All,
            })),
            Identity::Specific {
                default_role,
                allowed_roles,
            } => {
                match role {
                    // if the role has been specified
                    Some(role) => allowed_roles.get(role).map_or_else(
                        || Err(SessionError::Unauthorized(role.clone())),
                        |authorization| Ok(Cow::Borrowed(authorization)),
                    ),
                    // fall back to the default role specified as part of authorization
                    None => allowed_roles.get(default_role).map_or_else(
                        || Err(SessionError::InternalRoleNotFound(default_role.clone())),
                        |authorization| Ok(Cow::Borrowed(authorization)),
                    ),
                }
            }
        }
    }
}

// Error when resolving a session
#[derive(Debug, thiserror::Error)]
pub enum SessionError {
    // The requested role isn't allowed
    #[error("cannot be authorized as role: {0}")]
    Unauthorized(Role),
    // Default role information is not present in allowed_roles
    #[error("internal: RoleAuthorization of role: {0} not found")]
    InternalRoleNotFound(Role),
    #[error("the value of the header '{header_name}' isn't a valid string: '{error}'")]
    InvalidHeaderValue { header_name: String, error: String },
}

impl SessionError {
    pub fn into_middleware_error(self) -> engine_types::MiddlewareError {
        let code = match self {
            SessionError::Unauthorized(_) => StatusCode::UNAUTHORIZED,
            SessionError::InternalRoleNotFound(_) => StatusCode::INTERNAL_SERVER_ERROR,
            SessionError::InvalidHeaderValue { .. } => StatusCode::BAD_REQUEST,
        };
        let is_internal = match self {
            SessionError::Unauthorized(_) | SessionError::InvalidHeaderValue { .. } => false,
            SessionError::InternalRoleNotFound(_) => true,
        };
        engine_types::MiddlewareError {
            status: code,
            message: self.to_string(),
            is_internal,
        }
    }
}

// Using the x-hasura-* headers of the request and the identity set by the authn system,
// this layer resolves a 'session' which is then used by the execution engine
pub async fn resolve_session(
    State(state): State<engine_types::WithMiddlewareErrorConverter<()>>,
    Extension(identity): Extension<Identity>,
    mut request: Request<Body>,
    next: Next,
) -> axum::response::Result<axum::response::Response> {
    let session = authorize_identity(&identity, request.headers())
        .map_err(|session_err| state.handle_error(session_err.into_middleware_error()))?;
    request.extensions_mut().insert(session);
    let response = next.run(request).await;
    Ok(response)
}

/// Authorize the authenticated identity based on the provided headers.
pub fn authorize_identity(
    identity: &Identity,
    headers: &HeaderMap,
) -> Result<Session, SessionError> {
    let mut session_variables = BTreeMap::new();
    let mut role = None;
    // traverse through the headers and collect role and session variables
    for (header_name, header_value) in headers {
        let Ok(session_variable) = SessionVariableName::from_str(header_name.as_str());
        let variable_value_str = match header_value.to_str() {
            Err(e) => Err(SessionError::InvalidHeaderValue {
                header_name: header_name.to_string(),
                error: e.to_string(),
            })?,
            Ok(h) => h,
        };
        let variable_value = SessionVariableValue::Unparsed(variable_value_str.to_string());

        if session_variable == SESSION_VARIABLE_ROLE {
            role = Some(Role::new(variable_value_str));
        } else {
            // TODO: Handle the duplicate case?
            session_variables.insert(session_variable, variable_value);
        }
    }
    let session = identity
        .get_role_authorization(role.as_ref())?
        .build_session(session_variables);

    Ok(session)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions as pa;

    #[test]
    fn test_build_session_allow_all_session_variables() {
        let mut client_session_variables = BTreeMap::new();

        let mut authenticated_session_variables = HashMap::new();

        authenticated_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        client_session_variables.insert(
            SessionVariableName::from_str("x-hasura-custom").unwrap(),
            SessionVariableValue::new("test"),
        );
        client_session_variables.insert(
            SessionVariableName::from_str("x-hasura-custom-claim").unwrap(),
            SessionVariableValue::new("claim-value"),
        );
        let role_authorization = RoleAuthorization {
            role: Role::new("test-role"),
            session_variables: authenticated_session_variables,
            allowed_session_variables_from_request: SessionVariableList::All,
        };

        let session = role_authorization.build_session(client_session_variables.clone());

        let mut expected_session_variables = client_session_variables.clone();

        expected_session_variables.insert(
            SessionVariableName::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::Parsed("test-role".into()),
        );

        expected_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );
        pa::assert_eq!(
            Session {
                role: Role::new("test-role"),
                variables: SessionVariables(expected_session_variables),
            },
            session
        );
    }

    #[test]
    fn test_build_session_allow_specific_session_variables() {
        let mut client_session_variables = BTreeMap::new();

        let mut authenticated_session_variables = HashMap::new();

        authenticated_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        client_session_variables.insert(
            SessionVariableName::from_str("x-hasura-custom").unwrap(),
            SessionVariableValue::new("test"),
        );
        client_session_variables.insert(
            SessionVariableName::from_str("x-hasura-custom-claim").unwrap(),
            SessionVariableValue::new("claim-value"),
        );
        let mut allowed_sesion_variables_from_request = HashSet::new();
        allowed_sesion_variables_from_request
            .insert(SessionVariableName::from_str("x-hasura-custom").unwrap());
        allowed_sesion_variables_from_request
            .insert(SessionVariableName::from_str("x-hasura-custom-author-id").unwrap());
        let role_authorization = RoleAuthorization {
            role: Role::new("test-role"),
            session_variables: authenticated_session_variables,
            allowed_session_variables_from_request: SessionVariableList::Some(
                allowed_sesion_variables_from_request,
            ),
        };

        let session = role_authorization.build_session(client_session_variables.clone());

        let mut expected_session_variables = client_session_variables;
        expected_session_variables
            .remove(&SessionVariableName::from_str("x-hasura-custom-claim").unwrap());

        expected_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        expected_session_variables.insert(
            SessionVariableName::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::Parsed("test-role".into()),
        );

        pa::assert_eq!(
            Session {
                role: Role::new("test-role"),
                variables: SessionVariables(expected_session_variables),
            },
            session
        );
    }

    #[test]
    fn test_build_session_allow_no_session_variables() {
        let mut client_session_variables = BTreeMap::new();

        let mut authenticated_session_variables = HashMap::new();

        authenticated_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        client_session_variables.insert(
            SessionVariableName::from_str("x-hasura-custom").unwrap(),
            SessionVariableValue::new("test"),
        );
        client_session_variables.insert(
            SessionVariableName::from_str("x-hasura-custom-claim").unwrap(),
            SessionVariableValue::new("claim-value"),
        );

        let role_authorization = RoleAuthorization {
            role: Role::new("test-role"),
            session_variables: authenticated_session_variables,
            allowed_session_variables_from_request: SessionVariableList::Some(HashSet::new()),
        };

        let session = role_authorization.build_session(client_session_variables.clone());

        let mut expected_session_variables = BTreeMap::new();

        expected_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        expected_session_variables.insert(
            SessionVariableName::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::Parsed("test-role".into()),
        );

        pa::assert_eq!(
            Session {
                role: Role::new("test-role"),
                variables: SessionVariables(expected_session_variables),
            },
            session
        );
    }
}
