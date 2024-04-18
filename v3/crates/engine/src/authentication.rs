use hasura_authn_core::Role;
use hasura_authn_jwt::jwt;
use hasura_authn_webhook::webhook;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "AuthModeConfig")]
/// The configuration for the authentication mode to use - webhook or JWT.
pub enum AuthModeConfig {
    Webhook(webhook::AuthHookConfig),
    Jwt(Box<jwt::JWTConfig>),
}

#[derive(Serialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd, Deserialize)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthConfig")]
#[opendd(as_versioned_with_definition)]
/// Definition of the authentication configuration used by the API server.
pub enum AuthConfig {
    V1(AuthConfigV1),
}

#[derive(Serialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd, Deserialize)]
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
        open_dds::traits::OpenDd::deserialize(serde_json::json!(
            {
                "allowRoleEmulationBy": "admin",
                "mode": {
                  "webhook": {
                    "url": "http://auth_hook:3050/validate-request",
                    "method": "Post"
                  }
                }
            }
        ))
        .unwrap()
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
            path_buf.join("auth_config.json")
        };
        let auth_config_from_json = <super::AuthConfig as open_dds::traits::OpenDd>::deserialize(
            serde_json::from_str(&std::fs::read_to_string(path).unwrap()).unwrap(),
        )
        .unwrap();
        let auth_config_serialized = serde_json::to_value(auth_config_from_json.clone()).unwrap();
        let auth_config_from_serialized =
            <super::AuthConfig as open_dds::traits::OpenDd>::deserialize(auth_config_serialized)
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
