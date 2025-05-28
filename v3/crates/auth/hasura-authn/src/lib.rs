use std::{fmt::Display, str::FromStr};

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
    // Definition of the authentication configuration v3, used by the API server.
    #[opendd(json_schema(title = "AuthConfigV3"))]
    V3(AuthConfigV3),
}

#[derive(Serialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthConfigV3")]
#[schemars(example = "AuthConfigV3::example")]
/// Definition of the authentication configuration v3, used by the API server.
pub struct AuthConfigV3 {
    pub mode: AuthModeConfigV3,
}

impl AuthConfigV3 {
    fn example() -> Self {
        open_dds::traits::OpenDd::deserialize(
            serde_json::json!(
                {
                    "mode": {
                        "webhook": {
                            "method": "GET",
                            "url": {
                                "value": "http://auth_hook:3050/validate-request"
                            },
                            "customHeadersConfig": {
                                "headers": {
                                    "forward": ["Authorization"],
                                    "additional": {
                                        "user-agent": "hasura-ddn"
                                    }
                                }
                            }
                        }
                    }
                }
            ),
            jsonpath::JSONPath::new(),
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "AuthModeConfigV3")]
/// The configuration for the authentication mode to use - webhook, JWT or NoAuth.
pub enum AuthModeConfigV3 {
    Webhook(webhook::AuthHookConfigV3),
    Jwt(Box<jwt::JWTConfig>),
    NoAuth(noauth::NoAuthConfig),
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
    #[error(
        "AuthConfig v1 is deprecated. `allowRoleEmulationBy` has been removed. Please consider upgrading to AuthConfig v3."
    )]
    PleaseUpgradeV1ToV3,
    #[error("AuthConfig v2 is deprecated. Please consider upgrading to AuthConfig v3.")]
    PleaseUpgradeV2ToV3,
    #[error("Header '{0}', used in the auth config, is not a valid header name")]
    InvalidHeaderName(String),
    #[error("Header value '{0}' is not a valid header value for header '{1}' in the auth config")]
    InvalidHeaderValue(String, String),
}

impl Warning {
    pub fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            Warning::InvalidHeaderName(_) | Warning::InvalidHeaderValue(_, _) => {
                flags.contains(open_dds::flags::Flag::DisallowInvalidHeadersInAuthConfig)
            }
            _ => false,
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum Error {
    #[error("Invalid URL for auth webhook: {0}")]
    InvalidAuthWebhookUrl(String),
    #[error("{0}")]
    AuthConfigWarningsAsErrors(SeparatedBy<Warning>),
}

// A small utility type which exists for the sole purpose of displaying a vector with a certain
// separator.
#[derive(Debug, PartialEq)]
pub struct SeparatedBy<T> {
    pub lines_of: Vec<T>,
    pub separator: String,
}

impl<T: Display> Display for SeparatedBy<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, elem) in self.lines_of.iter().enumerate() {
            elem.fmt(f)?;
            if index < self.lines_of.len() - 1 {
                self.separator.fmt(f)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResolvedAuthConfig {
    pub auth_config: AuthConfig,
    pub auth_config_flags: AuthConfigFlags,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuthConfigFlags {
    /// If true, if a JWT contains the 'aud' claim, it must be validated against
    /// the audienced configured in the AuthConfig. If the audience is missing in the AuthConfig,
    /// the JWT will be rejected.
    pub require_audience_validation: hasura_authn_jwt::jwt::AudienceValidationMode,
}

impl Default for AuthConfigFlags {
    fn default() -> Self {
        Self {
            // Legacy behaviour for backwards compatiblity
            require_audience_validation: hasura_authn_jwt::jwt::AudienceValidationMode::Optional,
        }
    }
}

pub fn parse_auth_config(raw_auth_config: &str) -> Result<AuthConfig, anyhow::Error> {
    Ok(open_dds::traits::OpenDd::deserialize(
        serde_json::from_str(raw_auth_config)?,
        jsonpath::JSONPath::new(),
    )?)
}

/// Resolve `AuthConfig` which is not part of metadata. Hence we resolve/build
/// it separately. This also emits warnings.
pub fn resolve_auth_config(
    auth_config: AuthConfig,
    flags: &open_dds::flags::OpenDdFlags,
) -> Result<(ResolvedAuthConfig, Vec<Warning>), Error> {
    let warnings = validate_auth_config(&auth_config)?;
    let warnings = auth_config_warnings_as_error_by_compatibility(flags, warnings)?;
    let auth_config_flags = resolve_auth_config_flags(flags);
    Ok((
        ResolvedAuthConfig {
            auth_config,
            auth_config_flags,
        },
        warnings,
    ))
}

fn resolve_auth_config_flags(flags: &open_dds::flags::OpenDdFlags) -> AuthConfigFlags {
    AuthConfigFlags {
        require_audience_validation: if flags
            .contains(open_dds::flags::Flag::RequireJwtAudienceValidationIfAudClaimPresent)
        {
            hasura_authn_jwt::jwt::AudienceValidationMode::Required
        } else {
            hasura_authn_jwt::jwt::AudienceValidationMode::Optional
        },
    }
}

fn validate_auth_config(auth_config: &AuthConfig) -> Result<Vec<Warning>, Error> {
    let mut warnings = vec![];
    match auth_config {
        AuthConfig::V1(_) => warnings.push(Warning::PleaseUpgradeV1ToV3),
        AuthConfig::V2(_) => warnings.push(Warning::PleaseUpgradeV2ToV3),
        AuthConfig::V3(conf) => {
            if let AuthModeConfigV3::Webhook(config) = &conf.mode {
                // Validate the URL is valid
                reqwest::Url::parse(config.get_url())
                    .map_err(|e| Error::InvalidAuthWebhookUrl(e.to_string()))?;
                match &config {
                    webhook::AuthHookConfigV3::GET(config) => {
                        if let Some(headers_config) = config
                            .custom_headers_config
                            .as_ref()
                            .and_then(|c| c.headers.as_ref())
                        {
                            warnings.extend(validate_header_config(headers_config));
                        }
                    }
                    webhook::AuthHookConfigV3::POST(config) => {
                        if let Some(headers_config) = config
                            .custom_headers_config
                            .as_ref()
                            .and_then(|c| c.headers.as_ref())
                        {
                            warnings.extend(validate_header_config(headers_config));
                        }
                        if let Some(body_header_config) = config
                            .custom_headers_config
                            .as_ref()
                            .and_then(|c| c.body.as_ref())
                            .and_then(|c| c.headers.as_ref())
                        {
                            warnings.extend(validate_header_config(body_header_config));
                        }
                    }
                }
            }
        }
    }
    Ok(warnings)
}

fn auth_config_warnings_as_error_by_compatibility(
    flags: &open_dds::flags::OpenDdFlags,
    auth_warnings: Vec<Warning>,
) -> Result<Vec<Warning>, Error> {
    let (warnings_that_are_errors, remaining_warnings): (Vec<Warning>, Vec<Warning>) =
        auth_warnings
            .into_iter()
            .partition(|warning| warning.should_be_an_error(flags));

    if warnings_that_are_errors.is_empty() {
        Ok(remaining_warnings)
    } else {
        Err(Error::AuthConfigWarningsAsErrors(SeparatedBy {
            lines_of: warnings_that_are_errors,
            separator: "\n".to_string(),
        }))
    }
}

// Validate the header config
fn validate_header_config(header_config: &webhook::AuthHookConfigV3Headers) -> Vec<Warning> {
    let mut warnings = vec![];
    if let webhook::AllOrList::List(list) = &header_config.forward {
        for header_name in list {
            if let Err(_e) = axum::http::header::HeaderName::from_str(header_name.as_str()) {
                warnings.push(Warning::InvalidHeaderName(header_name.to_string()));
            }
        }
    }
    for (header_name, header_value) in &header_config.additional {
        if let Err(_e) = axum::http::header::HeaderName::from_str(header_name.as_str()) {
            warnings.push(Warning::InvalidHeaderName(header_name.to_string()));
        }
        if let Err(_e) = axum::http::header::HeaderValue::from_str(header_value.as_str()) {
            warnings.push(Warning::InvalidHeaderValue(
                header_value.to_string(),
                header_name.to_string(),
            ));
        }
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

impl AuthError {
    pub fn into_middleware_error(self) -> engine_types::MiddlewareError {
        match self {
            AuthError::Jwt(e) => e.into_middleware_error(),
            AuthError::Webhook(e) => e.into_middleware_error(),
        }
    }
}

pub enum PossibleAuthModeConfig<'a> {
    V1V2(&'a AuthModeConfig),
    V3(&'a AuthModeConfigV3),
}

/// Authenticate the user based on the headers and the auth config
pub async fn authenticate(
    headers_map: &HeaderMap,
    client: &reqwest::Client,
    resolved_auth_config: &ResolvedAuthConfig,
) -> Result<Identity, AuthError> {
    // We are still supporting AuthConfig::V1, hence we need to
    // support role emulation
    let (auth_mode, allow_role_emulation_by) = match &resolved_auth_config.auth_config {
        AuthConfig::V1(auth_config) => (
            &PossibleAuthModeConfig::V1V2(&auth_config.mode),
            auth_config.allow_role_emulation_by.as_ref(),
        ),
        // There is no role emulation in AuthConfig::V2
        AuthConfig::V2(auth_config) => (&PossibleAuthModeConfig::V1V2(&auth_config.mode), None),
        // There is no role emulation in AuthConfig::V3
        AuthConfig::V3(auth_config) => (&PossibleAuthModeConfig::V3(&auth_config.mode), None),
    };
    match &auth_mode {
        PossibleAuthModeConfig::V1V2(AuthModeConfig::NoAuth(no_auth_config))
        | PossibleAuthModeConfig::V3(AuthModeConfigV3::NoAuth(no_auth_config)) => {
            Ok(noauth::identity_from_config(no_auth_config))
        }
        PossibleAuthModeConfig::V1V2(AuthModeConfig::Webhook(webhook_config)) => {
            webhook::authenticate_request(
                client,
                webhook_config,
                headers_map,
                allow_role_emulation_by,
            )
            .await
            .map_err(AuthError::from)
        }
        PossibleAuthModeConfig::V3(AuthModeConfigV3::Webhook(webhook_config)) => {
            webhook::authenticate_request_v2(
                client,
                webhook_config,
                headers_map,
                allow_role_emulation_by,
            )
            .await
            .map_err(AuthError::from)
        }
        PossibleAuthModeConfig::V1V2(AuthModeConfig::Jwt(jwt_secret_config))
        | PossibleAuthModeConfig::V3(AuthModeConfigV3::Jwt(jwt_secret_config)) => {
            jwt_auth::authenticate_request(
                client,
                jwt_secret_config,
                headers_map,
                allow_role_emulation_by,
                resolved_auth_config
                    .auth_config_flags
                    .require_audience_validation,
            )
            .await
            .map_err(AuthError::from)
        }
    }
}

#[cfg(test)]
mod tests {
    use goldenfile::Mint;
    use open_dds::{
        test_utils::{JsonSchemaValidationConfig, validate_root_json_schema},
        traits::gen_root_schema_for,
    };
    use pretty_assertions::assert_eq;
    use std::{io::Write, path::PathBuf};

    #[test]
    fn test_auth_config_schema() {
        let mut mint = Mint::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
        let mut expected = mint.new_goldenfile("tests/auth_config.jsonschema").unwrap();
        let schema = gen_root_schema_for::<super::AuthConfig>(
            &mut schemars::r#gen::SchemaGenerator::default(),
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
            path_buf.join("static/auth/auth_config_v3.json")
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
            gen_root_schema_for::<super::AuthConfig>(
                &mut schemars::r#gen::SchemaGenerator::default(),
            ),
            &JsonSchemaValidationConfig {
                schemas_with_arbitrary_additional_properties_allowed: vec!["JWTClaimsMap"],
            },
        );
    }
}
