use axum::http::HeaderMap;
use hasura_authn_core::{Identity, Role};
use hasura_authn_jwt::{auth as jwt_auth, jwt};
use hasura_authn_noauth as noauth;
use hasura_authn_webhook::webhook;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "AuthModeConfig")]
/// The configuration for the authentication mode to use - webhook, JWT or NoAuth.
pub enum AuthModeConfig {
    Webhook(webhook::AuthHookConfig),
    Jwt(Box<jwt::JWTConfig>),
    NoAuth(noauth::NoAuthConfig),
}

#[derive(Serialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd, Deserialize)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(as_versioned_with_definition)]
#[opendd(json_schema(title = "AuthConfig"))]
/// Definition of the authentication configuration used by the API server.
pub enum AuthConfig {
    /// Definition of the authentication configuration v1, used by the API server.
    #[opendd(json_schema(title = "AuthConfigV1"))]
    V1(AuthConfigV1),
    /// Definition of the authentication configuration v2, used by the API server.
    #[opendd(json_schema(title = "AuthConfigV2"))]
    V2(AuthConfigV2),
}

impl AuthConfig {
    pub fn upgrade(self) -> AuthConfigV2 {
        match self {
            AuthConfig::V1(v1) => AuthConfigV2 { mode: v1.mode },
            AuthConfig::V2(v2) => v2,
        }
    }
}

#[derive(Serialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthConfigV2")]
#[schemars(example = "AuthConfigV2::example")]
/// Definition of the authentication configuration v2, used by the API server.
pub struct AuthConfigV2 {
    pub mode: AuthModeConfig,
}

impl AuthConfigV2 {
    fn example() -> Self {
        open_dds::traits::OpenDd::deserialize(
            serde_json::json!(
                {
                    "mode": {
                      "webhook": {
                        "url": "http://auth_hook:3050/validate-request",
                        "method": "Post"
                      }
                    }
                }
            ),
            jsonpath::JSONPath::new(),
        )
        .unwrap()
    }
}

#[derive(Serialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthConfigV1")]
#[schemars(example = "AuthConfigV1::example")]
/// Definition of the authentication configuration v1, used by the API server.
pub struct AuthConfigV1 {
    pub allow_role_emulation_by: Option<Role>,
    pub mode: AuthModeConfig,
}

impl AuthConfigV1 {
    fn example() -> Self {
        open_dds::traits::OpenDd::deserialize(
            serde_json::json!(
                {
                    "allowRoleEmulationBy": "admin",
                    "mode": {
                      "webhook": {
                        "url": "http://auth_hook:3050/validate-request",
                        "method": "Post"
                      }
                    }
                }
            ),
            jsonpath::JSONPath::new(),
        )
        .unwrap()
    }
}

/// Warnings for the user raised during auth config generation
/// These are things that don't break the build, but may do so in future
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum Warning {
    #[error("AuthConfig v1 is deprecated. `allowRoleEmulationBy` has been removed. Please consider upgrading to AuthConfig v2.")]
    PleaseUpgradeToV2,
}

/// Resolve `AuthConfig` which is not part of metadata. Hence we resolve/build
/// it separately. This also emits warnings.
pub fn resolve_auth_config(
    raw_auth_config: &str,
) -> Result<(AuthConfig, Vec<Warning>), anyhow::Error> {
    let auth_config: AuthConfig = open_dds::traits::OpenDd::deserialize(
        serde_json::from_str(raw_auth_config)?,
        jsonpath::JSONPath::new(),
    )?;
    let warnings = validate_auth_config(&auth_config);
    Ok((auth_config, warnings))
}

pub fn validate_auth_config(auth_config: &AuthConfig) -> Vec<Warning> {
    let mut warnings = vec![];
    match auth_config {
        AuthConfig::V1(_) => warnings.push(Warning::PleaseUpgradeToV2),
        AuthConfig::V2(_) => (),
    }
    warnings
}

/// Errors that can occur during authentication
#[derive(Debug, thiserror::Error)]
pub enum AuthError {
    #[error("JWT auth error: {0}")]
    Jwt(#[from] jwt::Error),
    #[error("Webhook auth error: {0}")]
    Webhook(#[from] webhook::Error),
}

impl tracing_util::TraceableError for AuthError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            AuthError::Jwt(e) => e.visibility(),
            AuthError::Webhook(e) => e.visibility(),
        }
    }
}

impl axum::response::IntoResponse for AuthError {
    fn into_response(self) -> axum::response::Response {
        match self {
            AuthError::Jwt(e) => e.into_response(),
            AuthError::Webhook(e) => e.into_response(),
        }
    }
}

/// Authenticate the user based on the headers and the auth config
pub async fn authenticate(
    headers_map: &HeaderMap,
    client: &reqwest::Client,
    auth_config: &AuthConfig,
) -> Result<Identity, AuthError> {
    // We are still supporting AuthConfig::V1, hence we need to
    // support role emulation
    let (auth_mode, allow_role_emulation_by) = match auth_config {
        AuthConfig::V1(auth_config) => (
            &auth_config.mode,
            auth_config.allow_role_emulation_by.as_ref(),
        ),
        // There is no role emulation in AuthConfig::V2
        AuthConfig::V2(auth_config) => (&auth_config.mode, None),
    };
    match auth_mode {
        AuthModeConfig::NoAuth(no_auth_config) => Ok(noauth::identity_from_config(no_auth_config)),
        AuthModeConfig::Webhook(webhook_config) => webhook::authenticate_request(
            client,
            webhook_config,
            headers_map,
            allow_role_emulation_by,
        )
        .await
        .map_err(AuthError::from),
        AuthModeConfig::Jwt(jwt_secret_config) => jwt_auth::authenticate_request(
            client,
            *jwt_secret_config.clone(),
            headers_map,
            allow_role_emulation_by,
        )
        .await
        .map_err(AuthError::from),
    }
}

#[cfg(test)]
mod tests {
    use goldenfile::Mint;
    use open_dds::{
        test_utils::{validate_root_json_schema, JsonSchemaValidationConfig},
        traits::gen_root_schema_for,
    };
    use pretty_assertions::assert_eq;
    use std::{io::Write, path::PathBuf};

    #[test]
    fn test_auth_config_schema() {
        let mut mint = Mint::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
        let mut expected = mint.new_goldenfile("tests/auth_config.jsonschema").unwrap();
        let schema = gen_root_schema_for::<super::AuthConfig>(
            &mut schemars::gen::SchemaGenerator::default(),
        );
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&schema).unwrap()
        )
        .unwrap();
    }

    #[test]
    /// This test is a round trip test for the AuthConfig type. It reads a reference auth config file,
    /// deserializes it into a AuthConfig type, serializes it back to JSON, deserializes it back into
    /// a AuthConfig type, and compares the two AuthConfig types to ensure they are equal.
    fn test_serialize_reference_auth_config() {
        let path = {
            let mut path_buf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            path_buf.pop();
            path_buf.pop();
            path_buf.pop();
            path_buf.join("static/auth/auth_config.json")
        };
        let auth_config_from_json = <super::AuthConfig as open_dds::traits::OpenDd>::deserialize(
            serde_json::from_str(&std::fs::read_to_string(path).unwrap()).unwrap(),
            jsonpath::JSONPath::new(),
        )
        .unwrap();
        let auth_config_serialized = serde_json::to_value(auth_config_from_json.clone()).unwrap();
        let auth_config_from_serialized =
            <super::AuthConfig as open_dds::traits::OpenDd>::deserialize(
                auth_config_serialized,
                jsonpath::JSONPath::new(),
            )
            .unwrap();
        assert_eq!(auth_config_from_json, auth_config_from_serialized);
    }

    #[test]
    /// Runs various checks on the generated JSONSchema to ensure it follows certain conventions.
    fn test_validate_auth_config_json_schema() {
        validate_root_json_schema(
            gen_root_schema_for::<super::AuthConfig>(&mut schemars::gen::SchemaGenerator::default()),
            &JsonSchemaValidationConfig {
                schemas_with_arbitrary_additional_properties_allowed: vec!["JWTClaimsMap"],
            },
        );
    }
}
