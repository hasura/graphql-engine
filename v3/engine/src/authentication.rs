use hasura_authn_core::Role;
use hasura_authn_jwt::jwt;
use hasura_authn_webhook::webhook;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "AuthModeConfig")]
/// The configuration for the authentication mode to use - webhook or JWT.
pub enum AuthModeConfig {
    Webhook(webhook::AuthHookConfig),
    Jwt(Box<jwt::JWTConfig>),
}

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthConfig")]
/// Definition of the authentication configuration used by the API server.
pub enum AuthConfig {
    V1(AuthConfigV1),
}

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthConfigV1")]
#[schemars(example = "AuthConfigV1::example")]
/// Definition of the authentication configuration used by the API server.
pub struct AuthConfigV1 {
    pub allow_role_emulation_by: Option<Role>,
    pub mode: AuthModeConfig,
}

impl AuthConfigV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "allowRoleEmulationBy": "admin",
                "mode": {
                  "webhook": {
                    "url": "http://auth_hook:3050/validate-request",
                    "method": "Post"
                  }
                }
            }
        "#,
        )
        .unwrap()
    }
}
