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
    for role in hasura_claims.allowed_roles.iter() {
        let role_authorization = auth_base::RoleAuthorization {
            role: role.clone(),
            // Note: The same `custom_claims` is being cloned
            // for every role present in the allowed roles.
            // We should think of having common claims.
            session_variables: hasura_claims.custom_claims.clone(),
            allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                HashSet::new(),
            ),
        };
        if allowed_roles
            .insert(role.clone(), role_authorization)
            .is_some()
        {
            Err(InternalError::DuplicateAllowedRoleFound)?
        }
    }
    Ok(allowed_roles)
}

/// Authenticates the request by accepting the `Authorization` header along
/// with the `JWTSecretConfig` and returns `hasura_authn_core::Identity`
pub async fn authenticate_request(
    http_client: &reqwest::Client,
    jwt_config: JWTConfig,
    allow_role_emulation_for: Option<&Role>,
    headers: &HeaderMap,
) -> Result<Identity, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "jwt_authenticate_request",
            "Authenticate request using JSON Web Token".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin({
                    async {
                        let authorization_token: String =
                            get_authorization_token(&jwt_config.token_location, headers)?;
                        let hasura_claims = tracer
                            .in_span_async(
                                "decode_and_parse_hasura_claims",
                                "Decode and parse Hasura claims".to_string(),
                                SpanVisibility::Internal,
                                || {
                                    Box::pin(decode_and_parse_hasura_claims(
                                        http_client,
                                        jwt_config,
                                        authorization_token,
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
                                    .map(|v| Role::new(v.0.as_str()));
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

    use auth_base::{RoleAuthorization, SessionVariable, SessionVariableValue};
    use indoc::indoc;
    use jsonwebtoken as jwt;
    use jsonwebtoken::Algorithm;
    use jwt::{encode, EncodingKey};
    use reqwest::header::AUTHORIZATION;
    use tokio;

    use super::*;

    fn get_claims(hasura_claims: &serde_json::Value, insert_hasura_claims_at: &str) -> Claims {
        let claims_str = r#"
                            {
                              "sub": "1234567890",
                              "name": "John Doe",
                              "iat": 1693439022,
                              "exp": 1916239022,
                              "https://hasura.io/jwt/claims": {}
                            }"#;
        let mut claims: serde_json::Value = serde_json::from_str(claims_str).unwrap();
        *claims.pointer_mut(insert_hasura_claims_at).unwrap() = hasura_claims.clone();

        serde_json::from_value(claims).unwrap()
    }

    fn get_encoded_claims(alg: jwt::Algorithm, hasura_claims: &HasuraClaims) -> String {
        let claims: Claims = get_claims(
            &serde_json::to_value(hasura_claims.clone()).unwrap(),
            &DEFAULT_HASURA_CLAIMS_NAMESPACE_POINTER,
        );
        let jwt_header = jwt::Header {
            alg,
            ..Default::default()
        };

        let encoded_claims = encode(
            &jwt_header,
            &claims,
            &EncodingKey::from_secret("token".as_ref()),
        )
        .unwrap();
        encoded_claims
    }

    fn get_default_hasura_claims() -> HasuraClaims {
        let mut hasura_custom_claims = HashMap::new();
        hasura_custom_claims.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue("1".to_string()),
        );
        HasuraClaims {
            default_role: Role::new("user"),
            allowed_roles: vec![Role::new("foo"), Role::new("bar"), Role::new("user")],
            custom_claims: hasura_custom_claims,
        }
    }

    #[tokio::test]
    async fn test_unsuccessful_role_emulation() {
        let encoded_claims = get_encoded_claims(Algorithm::HS256, &get_default_hasura_claims());

        let jwt_secret_config_str = indoc! {r#"
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                  "namespace": {
                     "claimsFormat": "Json",
                     "location": "/https:~1~1hasura.io~1jwt~1claims"
                  }
               }
            }
            "#};

        let jwt_config: JWTConfig = serde_json::from_str(jwt_secret_config_str).unwrap();

        let http_client = reqwest::Client::new();

        let mut header_map = HeaderMap::new();
        header_map.insert(
            AUTHORIZATION,
            ("Bearer ".to_owned() + &encoded_claims).parse().unwrap(),
        );

        let authenticated_identity = authenticate_request(
            &http_client,
            jwt_config,
            Some(&Role::new("admin")),
            &header_map,
        )
        .await
        .unwrap();

        let test_role = Role::new("user");
        let mut expected_allowed_roles = HashMap::new();
        let mut role_authorization_session_variables = HashMap::new();

        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
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
        )
    }

    #[tokio::test]
    async fn test_successful_role_emulation() {
        let mut hasura_claims = get_default_hasura_claims();
        hasura_claims.custom_claims.insert(
            SessionVariable::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::new("admin"),
        );
        let encoded_claims = get_encoded_claims(Algorithm::HS256, &hasura_claims);

        let jwt_secret_config_str = indoc! {r#"
            {
               "key": {
                 "fixed": {
                    "algorithm": "HS256",
                    "key": {
                       "value": "token"
                    }
                 }
               },
               "tokenLocation": {
                  "type": "BearerAuthorization"
               },
               "claimsConfig": {
                  "namespace": {
                     "claimsFormat": "Json",
                     "location": "/https:~1~1hasura.io~1jwt~1claims"
                  }
               }
            }
            "#};

        let jwt_config: JWTConfig = serde_json::from_str(jwt_secret_config_str).unwrap();

        let http_client = reqwest::Client::new();

        let mut header_map = HeaderMap::new();
        header_map.insert(
            AUTHORIZATION,
            ("Bearer ".to_owned() + &encoded_claims).parse().unwrap(),
        );

        let authenticated_identity = authenticate_request(
            &http_client,
            jwt_config,
            Some(&Role::new("admin")),
            &header_map,
        )
        .await
        .unwrap();

        assert_eq!(
            authenticated_identity,
            Identity::RoleEmulationEnabled(Role::new("admin"))
        )
    }
}
