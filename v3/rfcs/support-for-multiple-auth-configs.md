# Multiple Auth Configs RFC

### Motivation

There are instances where it may be useful to have multiple different
Authentication providers. For example you may want to be able to have both
Webhook & JWT authentication enabled.

### Proposal

Add a field for alternative_modes into the AuthConfig, and allow a user to pass
the Hasura Engine a additional header `X-Hasura-Auth-Mode` to specify a
alternative authentication mode.

The AuthConfig structure becomes:

```rust
pub struct AuthConfigV4 {
    pub mode: AuthModeConfig,
    pub alternative_modes: Option<Vec<AlternativeMode>>,
}

pub struct AlternativeMode {
    pub identifier: String,
    pub config: AuthModeConfigV3,
}
```

There could be a header that gets sent to engine to specify the Auth-Mode if a
alternative Auth Mode other than the default mode is desired.

This header could _potentially_ be configurable, (and in the code implementation
in this branch it was made configurable) however it might be better to hard-code
it to `X-Hasura-Auth-Mode`. This was made configurable in the ServerOptions.

```rust
#[allow(clippy::struct_excessive_bools)] // booleans are pretty useful here
#[derive(Parser, Serialize)]
#[command(version = VERSION)]
struct ServerOptions {
    /// ...
    ///
    /// The name of the header used to specify the auth mode when using alternative Auth Modes.
    /// Defaults to "X-Hasura-Auth-Mode" if not specified.
    #[arg(long, env = "AUTH_MODE_HEADER", default_value = "X-Hasura-Auth-Mode")]
    auth_mode_header: String,
}
```

When a request is made, if the user passes an `X-Hasura-Auth-Mode`, that
Auth-Mode is used from the alternativeModes.

Example:

The V3 Auth Config can currently be set like this for NoAuth:

```json
{
  "version": "v3",
  "definition": {
    "mode": {
      "noAuth": {
        "role": "admin",
        "sessionVariables": {
          "x-hasura-user-id": "1"
        }
      }
    }
  }
}
```

Imagine being able to specify multiple Auth-Configs like so:

```json
{
  "version": "v4",
  "definition": {
    "mode": {
      "noAuth": {
        "role": "admin",
        "sessionVariables": {
          "x-hasura-user-id": "1"
        }
      }
    },
    "alternativeModes": [
      {
        "identifier": "user2",
        "config": {
          "noAuth": {
            "role": "admin",
            "sessionVariables": {
              "x-hasura-user-id": "2"
            }
          }
        }
      },
      {
        "identifier": "webhook",
        "config": {
          "webhook": {
            "url": "http://auth_hook:3050/validate-request",
            "method": "POST"
          }
        }
      }
    ]
  }
}
```

When sending a request, the Authentication Middleware will check for a specified
`X-Hasura-Auth-Mode` header and if it is found it will use that auth mode.
Otherwise it will use the "default" mode specified.

```rust
/// Authenticate the user based on the headers and the auth config
pub async fn authenticate(
    headers_map: &HeaderMap,
    client: &reqwest::Client,
    resolved_auth_config: &ResolvedAuthConfig,
    auth_mode_header: &str,
) -> Result<Identity, AuthError> {
    let (auth_mode, allow_role_emulation_by) = match &resolved_auth_config.auth_config {
        AuthConfig::V1(auth_config) => (
            &PossibleAuthModeConfig::V1V2(&auth_config.mode),
            auth_config.allow_role_emulation_by.as_ref(),
        ),
        // There is no role emulation in AuthConfig::V2
        AuthConfig::V2(auth_config) => (&PossibleAuthModeConfig::V1V2(&auth_config.mode), None),
        // There is no role emulation in AuthConfig::V3
        AuthConfig::V3(auth_config) => (&PossibleAuthModeConfig::V3V4(&auth_config.mode), None),
        AuthConfig::V4(auth_config) => {
            let auth_mode_header = headers_map.get(auth_mode_header);
            match (auth_mode_header, &auth_config.alternative_modes) {
                (Some(mode), Some(alt_modes)) => {
                    let mode = mode
                        .to_str()
                        .map_err(|e| AuthError::InvalidAuthModeHeader(e.to_string()))?;
                    if let Some(mode_config) = alt_modes.iter().find(|m| m.identifier == mode) {
                        (&PossibleAuthModeConfig::V3V4(&mode_config.config), None)
                    } else {
                        (&PossibleAuthModeConfig::V3V4(&auth_config.mode), None)
                    }
                }
                _ => (&PossibleAuthModeConfig::V3V4(&auth_config.mode), None),
            }
        }
    };
    match &auth_mode {
        PossibleAuthModeConfig::V1V2(AuthModeConfig::NoAuth(no_auth_config))
        | PossibleAuthModeConfig::V3V4(AuthModeConfigV3::NoAuth(no_auth_config)) => {
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
        PossibleAuthModeConfig::V3V4(AuthModeConfigV3::Webhook(webhook_config)) => {
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
        | PossibleAuthModeConfig::V3V4(AuthModeConfigV3::Jwt(jwt_secret_config)) => {
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
```

For example, if nothing is passed for the `X-Hasura-Auth-Mode` and a request is
sent it will use the default mode. If the header is set to `user2` it will use
the alternative mode for `user2`. Etc.

Additional thoughts:

This could potentially serve as an admin secret. Although it would be nice to
have an additional AuthConfig type for ApiKey
