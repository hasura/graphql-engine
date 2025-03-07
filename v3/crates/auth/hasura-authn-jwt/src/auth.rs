use axum::http::HeaderMap;
use hasura_authn_core::{self as auth_base, Identity, Role};
use std::collections::{HashMap, HashSet};
use tracing_util::SpanVisibility;

use crate::jwt::*;
use auth_base::SESSION_VARIABLE_ROLE;

fn build_allowed_roles(
    hasura_claims: &HasuraClaims,
) -> Result<HashMap<Role, auth_base::RoleAuthorization>, Error> {
    let mut allowed_roles = HashMap::new();
    for role in &hasura_claims.allowed_roles {
        let role_authorization = auth_base::RoleAuthorization {
            role: role.clone(),
            // Note: The same `custom_claims` is being cloned
            // for every role present in the allowed roles.
            // We should think of having common claims.
            session_variables: hasura_claims
                .custom_claims
                .iter()
                .map(|(k, v)| (k.clone(), v.clone().into()))
                .collect(),
            allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                HashSet::new(),
            ),
        };
        if allowed_roles
            .insert(role.clone(), role_authorization)
            .is_some()
        {
            Err(InternalError::DuplicateAllowedRoleFound)?;
        }
    }
    Ok(allowed_roles)
}

/// Authenticates the request by accepting the `Authorization` header along
/// with the `JWTSecretConfig` and returns `hasura_authn_core::Identity`
pub async fn authenticate_request(
    http_client: &reqwest::Client,
    jwt_config: &JWTConfig,
    headers: &HeaderMap,
    allow_role_emulation_for: Option<&Role>,
    audience_validation_mode: AudienceValidationMode,
) -> Result<Identity, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "jwt_authenticate_request",
            "Authenticate request using JSON Web Token",
            SpanVisibility::Internal,
            || {
                Box::pin({
                    async {
                        let authorization_token: String =
                            get_authorization_token(&jwt_config.token_location, headers)?;
                        let hasura_claims = tracer
                            .in_span_async(
                                "decode_and_parse_hasura_claims",
                                "Decode and parse Hasura claims",
                                SpanVisibility::Internal,
                                || {
                                    Box::pin(decode_and_parse_hasura_claims(
                                        http_client,
                                        jwt_config,
                                        authorization_token,
                                        audience_validation_mode,
                                    ))
                                },
                            )
                            .await?;
                        Ok(match allow_role_emulation_for {
                            // No emulation role found, so build the specific identity.
                            None => Identity::Specific {
                                default_role: hasura_claims.default_role.clone(),
                                allowed_roles: build_allowed_roles(&hasura_claims)?,
                            },
                            Some(emulation_role) => {
                                // Look for the `x-hasura-role` in the decoded claims.
                                let role = hasura_claims
                                    .custom_claims
                                    .get(&SESSION_VARIABLE_ROLE)
                                    .map(|v| {
                                        Ok::<_, Error>(Role::new(v.0.as_str().ok_or_else(
                                            || Error::ClaimMustBeAString {
                                                claim_name: SESSION_VARIABLE_ROLE.to_string(),
                                            },
                                        )?))
                                    })
                                    .transpose()?;
                                match role {
                                    // `x-hasura-role` is found, check if it's the
                                    // role that can emulate by comparing it to
                                    // `allow_role_emulation_for`, otherwise
                                    // return the specific identity.
                                    Some(role) => {
                                        if role == *emulation_role {
                                            Identity::RoleEmulationEnabled(role)
                                        } else {
                                            Identity::Specific {
                                                default_role: hasura_claims.default_role.clone(),
                                                allowed_roles: build_allowed_roles(&hasura_claims)?,
                                            }
                                        }
                                    }
                                    // `x-hasura-role` is not found, so build the specific identity.
                                    None => Identity::Specific {
                                        default_role: hasura_claims.default_role.clone(),
                                        allowed_roles: build_allowed_roles(&hasura_claims)?,
                                    },
                                }
                            }
                        })
                    }
                })
            },
        )
        .await
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use auth_base::{RoleAuthorization, SessionVariableValue};
    use hasura_authn_core::JsonSessionVariableValue;
    use jsonwebtoken as jwt;
    use jsonwebtoken::Algorithm;
    use jwt::{encode, EncodingKey};
    use open_dds::session_variables::SessionVariableName;
    use reqwest::header::AUTHORIZATION;
    use serde_json::json;
    use tokio;

    use super::*;

    fn get_claims(
        hasura_claims: &serde_json::Value,
        insert_hasura_claims_at: &str,
    ) -> anyhow::Result<Claims> {
        let mut claims_json = json!(
            {
                "sub": "1234567890",
                "name": "John Doe",
                "iat": 1693439022,
                "exp": 1916239022,
                "claims.jwt.hasura.io": {}
            }
        );
        *claims_json
            .pointer_mut(insert_hasura_claims_at)
            .ok_or(anyhow::anyhow!(
                "Could not find {insert_hasura_claims_at:?}"
            ))? = hasura_claims.clone();
        let claims = serde_json::from_value(claims_json)?;
        Ok(claims)
    }

    fn get_encoded_claims(
        alg: jwt::Algorithm,
        hasura_claims: &HasuraClaims,
    ) -> anyhow::Result<String> {
        let claims: Claims = get_claims(
            &serde_json::to_value(hasura_claims)?,
            jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]).as_str(),
        )?;
        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };

        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )?;
        Ok(encoded_claims)
    }

    fn get_default_hasura_claims() -> HasuraClaims {
        let mut hasura_custom_claims = HashMap::new();
        hasura_custom_claims.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            JsonSessionVariableValue(json!("1")),
        );
        HasuraClaims {
            default_role: Role::new("user"),
            allowed_roles: vec![Role::new("foo"), Role::new("bar"), Role::new("user")],
            custom_claims: hasura_custom_claims,
        }
    }

    #[tokio::test]
    async fn test_unsuccessful_role_emulation() -> anyhow::Result<()> {
        let encoded_claims = get_encoded_claims(Algorithm::HS256, &get_default_hasura_claims())?;

        let jwt_secret_config_json = json!({
            "key": {
                "fixed": {
                    "algorithm": "HS256",
                    "key": {
                        "value": "token"
                    },
                },
            },
            "tokenLocation": {
                "type": "BearerAuthorization",
            },
            "claimsConfig": {
                "namespace": {
                    "claimsFormat": "Json",
                    "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                },
            },
        });

        let jwt_config: JWTConfig = serde_json::from_value(jwt_secret_config_json)?;

        let http_client = reqwest::Client::new();

        let mut header_map = HeaderMap::new();
        header_map.insert(
            AUTHORIZATION,
            ("Bearer ".to_owned() + &encoded_claims).parse()?,
        );

        let authenticated_identity = authenticate_request(
            &http_client,
            &jwt_config,
            &header_map,
            Some(&Role::new("admin")),
            AudienceValidationMode::Required,
        )
        .await?;

        let test_role = Role::new("user");
        let mut expected_allowed_roles = HashMap::new();
        let mut role_authorization_session_variables = HashMap::new();

        role_authorization_session_variables.insert(
            SessionVariableName::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::Parsed(json!("1")),
        );
        expected_allowed_roles.insert(
            test_role.clone(),
            RoleAuthorization {
                role: test_role.clone(),
                session_variables: role_authorization_session_variables.clone(),
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );
        expected_allowed_roles.insert(
            Role::new("foo"),
            RoleAuthorization {
                role: Role::new("foo"),
                session_variables: role_authorization_session_variables.clone(),
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );

        expected_allowed_roles.insert(
            Role::new("bar"),
            RoleAuthorization {
                role: Role::new("bar"),
                session_variables: role_authorization_session_variables,
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );

        assert_eq!(
            authenticated_identity,
            Identity::Specific {
                default_role: test_role,
                allowed_roles: expected_allowed_roles
            }
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_successful_role_emulation() -> anyhow::Result<()> {
        let mut hasura_claims = get_default_hasura_claims();
        hasura_claims.custom_claims.insert(
            SessionVariableName::from_str("x-hasura-role").unwrap(),
            JsonSessionVariableValue(json!("admin")),
        );
        let encoded_claims = get_encoded_claims(Algorithm::HS256, &hasura_claims)?;

        let jwt_secret_config_json = json!({
            "key": {
                "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token",
                    },
                }
            },
            "tokenLocation": {
                "type": "BearerAuthorization",
            },
            "claimsConfig": {
                "namespace": {
                    "claimsFormat": "Json",
                    "location": jsonptr::Pointer::new([DEFAULT_HASURA_CLAIMS_NAMESPACE]),
                },
            },
        });

        let jwt_config: JWTConfig = serde_json::from_value(jwt_secret_config_json)?;

        let http_client = reqwest::Client::new();

        let mut header_map = HeaderMap::new();
        header_map.insert(
            AUTHORIZATION,
            ("Bearer ".to_owned() + &encoded_claims).parse()?,
        );

        let authenticated_identity = authenticate_request(
            &http_client,
            &jwt_config,
            &header_map,
            Some(&Role::new("admin")),
            AudienceValidationMode::Required,
        )
        .await?;

        assert_eq!(
            authenticated_identity,
            Identity::RoleEmulationEnabled(Role::new("admin"))
        );
        Ok(())
    }
}
