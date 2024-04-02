use axum::middleware::Next;
use axum::response::IntoResponse;
use axum::Extension;
use axum::{http::Request, http::StatusCode};
use lang_graphql::http::Response;
use schemars::JsonSchema;
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
    session_variables::{SessionVariable, SESSION_VARIABLE_ROLE},
};

// Value of a session variable
// TODO: This need to change to serde_json::Value
// TODO: Get rid of the pub access to inner value
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize, JsonSchema)]
pub struct SessionVariableValue(pub String);

impl SessionVariableValue {
    pub fn new(value: &str) -> Self {
        SessionVariableValue(value.to_string())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionVariables(HashMap<SessionVariable, SessionVariableValue>);

impl SessionVariables {
    pub fn get(&self, session_variable: &SessionVariable) -> Option<&SessionVariableValue> {
        self.0.get(session_variable)
    }
}

// The privilege with which a request is executed
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Session {
    pub role: Role,
    pub variables: SessionVariables,
}

// Privileges of a role
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RoleAuthorization {
    pub role: Role,
    pub session_variables: HashMap<SessionVariable, SessionVariableValue>,
    pub allowed_session_variables_from_request: SessionVariableList,
}

impl RoleAuthorization {
    pub fn build_session(
        &self,
        mut variables: HashMap<SessionVariable, SessionVariableValue>,
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
    Some(HashSet<SessionVariable>),
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

impl IntoResponse for SessionError {
    fn into_response(self) -> axum::response::Response {
        let code = match self {
            SessionError::Unauthorized(_) => StatusCode::UNAUTHORIZED,
            SessionError::InternalRoleNotFound(_) => StatusCode::INTERNAL_SERVER_ERROR,
            SessionError::InvalidHeaderValue { .. } => StatusCode::BAD_REQUEST,
        };
        Response::error_message_with_status(code, self.to_string()).into_response()
    }
}

// Using the x-hasura-* headers of the request and the identity set by the authn system,
// this layer resolves a 'session' which is then used by the execution engine
pub async fn resolve_session<'a, B>(
    Extension(identity): Extension<Identity>,
    mut request: Request<B>,
    next: Next<B>,
) -> axum::response::Result<axum::response::Response> {
    let mut session_variables = HashMap::new();
    let mut role = None;
    // traverse through the headers and collect role and session variables
    for (header_name, header_value) in request.headers() {
        if let Ok(session_variable) = SessionVariable::from_str(header_name.as_str()) {
            let variable_value = match header_value.to_str() {
                Err(e) => Err(SessionError::InvalidHeaderValue {
                    header_name: header_name.to_string(),
                    error: e.to_string(),
                })?,
                Ok(h) => SessionVariableValue::new(h),
            };

            if session_variable == SESSION_VARIABLE_ROLE.to_owned() {
                role = Some(Role::new(&variable_value.0))
            } else {
                // TODO: Handle the duplicate case?
                session_variables.insert(session_variable, variable_value);
            }
        }
    }
    let session = identity
        .get_role_authorization(role.as_ref())?
        .build_session(session_variables);
    request.extensions_mut().insert(session);
    let response = next.run(request).await;
    Ok(response)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions as pa;

    #[test]
    fn test_build_session_allow_all_session_variables() {
        let mut client_session_variables = HashMap::new();

        let mut authenticated_session_variables = HashMap::new();

        authenticated_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        client_session_variables.insert(
            SessionVariable::from_str("x-hasura-custom").unwrap(),
            SessionVariableValue::new("test"),
        );
        client_session_variables.insert(
            SessionVariable::from_str("x-hasura-custom-claim").unwrap(),
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
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
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
        let mut client_session_variables = HashMap::new();

        let mut authenticated_session_variables = HashMap::new();

        authenticated_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        client_session_variables.insert(
            SessionVariable::from_str("x-hasura-custom").unwrap(),
            SessionVariableValue::new("test"),
        );
        client_session_variables.insert(
            SessionVariable::from_str("x-hasura-custom-claim").unwrap(),
            SessionVariableValue::new("claim-value"),
        );
        let mut allowed_sesion_variables_from_request = HashSet::new();
        allowed_sesion_variables_from_request
            .insert(SessionVariable::from_str("x-hasura-custom").unwrap());
        allowed_sesion_variables_from_request
            .insert(SessionVariable::from_str("x-hasura-custom-author-id").unwrap());
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
            .remove(&SessionVariable::from_str("x-hasura-custom-claim").unwrap());

        expected_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
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
    fn test_build_session_allow_no_session_variables() {
        let mut client_session_variables = HashMap::new();

        let mut authenticated_session_variables = HashMap::new();

        authenticated_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );

        client_session_variables.insert(
            SessionVariable::from_str("x-hasura-custom").unwrap(),
            SessionVariableValue::new("test"),
        );
        client_session_variables.insert(
            SessionVariable::from_str("x-hasura-custom-claim").unwrap(),
            SessionVariableValue::new("claim-value"),
        );

        let role_authorization = RoleAuthorization {
            role: Role::new("test-role"),
            session_variables: authenticated_session_variables,
            allowed_session_variables_from_request: SessionVariableList::Some(HashSet::new()),
        };

        let session = role_authorization.build_session(client_session_variables.clone());

        let mut expected_session_variables = HashMap::new();

        expected_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
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
}
