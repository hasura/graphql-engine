use hasura_authn_core::{JsonSessionVariableValue, Role, SessionVariableName};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::{HashMap, HashSet};

#[derive(Serialize, Deserialize, Clone, Debug, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "NoAuthConfig")]
#[schemars(example = "NoAuthConfig::example")]
/// Configuration used when running engine without authentication
pub struct NoAuthConfig {
    /// role to assume whilst running the engine
    pub role: Role,
    /// static session variables to use whilst running the engine
    #[schemars(title = "SessionVariables")]
    pub session_variables: HashMap<SessionVariableName, JsonSessionVariableValue>,
}

impl NoAuthConfig {
    fn example() -> Self {
        serde_json::from_value(json! {
            {
                "role": "admin",
                "sessionVariables": {
                    "x-hasura-user-id": "100"
                }
            }
        })
        .unwrap()
    }
}

pub fn identity_from_config(no_auth_config: &NoAuthConfig) -> hasura_authn_core::Identity {
    let mut allowed_roles = HashMap::new();

    // allow role
    allowed_roles.insert(
        no_auth_config.role.clone(),
        hasura_authn_core::RoleAuthorization {
            role: no_auth_config.role.clone(),
            session_variables: no_auth_config
                .session_variables
                .iter()
                .map(|(k, v)| (k.clone(), v.clone().into()))
                .collect(),
            allowed_session_variables_from_request: hasura_authn_core::SessionVariableList::Some(
                HashSet::new(),
            ),
        },
    );

    hasura_authn_core::Identity::Specific {
        default_role: no_auth_config.role.clone(),
        allowed_roles,
    }
}
