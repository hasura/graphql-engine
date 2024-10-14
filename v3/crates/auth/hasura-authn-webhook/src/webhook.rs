use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use std::sync::OnceLock;
use std::time::Duration;

use auth_base::{Identity, Role, RoleAuthorization, SessionVariable, SessionVariableValue};
use axum::{
    http::{HeaderMap, HeaderName, StatusCode},
    response::IntoResponse,
};
use reqwest::{header::ToStrError, Url};
use serde::{de::Error as SerdeDeError, Deserialize, Deserializer, Serialize, Serializer};

use hasura_authn_core as auth_base;
use open_dds::session_variables;
use schemars::JsonSchema;
use tracing_util::{ErrorVisibility, SpanVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error in converting the header value corresponding to the {header_name} to a String - {error}")]
    ErrorInConvertingHeaderValueToString {
        header_name: HeaderName,
        error: ToStrError,
    },
    #[error("The Authentication hook has denied to execute the request.")]
    AuthenticationFailed,
    #[error("Internal Error - {0}")]
    Internal(#[from] InternalError),
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        // For the purpose of traces, all webhook errors should be developer facing.
        ErrorVisibility::User
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InternalError {
    #[error("Error while making the authentication HTTP request to the webhook - {0}")]
    ErrorWhileMakingHTTPRequestToTheAuthHook(reqwest::Error),
    #[error("The authentication hook has returned the status {0}. Only 200 and 401 response status are recognized.")]
    AuthHookUnexpectedStatus(reqwest::StatusCode),
    #[error("Reqwest error: {0}")]
    ReqwestError(reqwest::Error),
    #[error("'x-hasura-role' session variable not found in the webhook response.")]
    RoleSessionVariableNotFound,
}

impl TraceableError for InternalError {
    fn visibility(&self) -> ErrorVisibility {
        // For the purpose of traces, all webhook errors should be developer facing.
        ErrorVisibility::User
    }
}

impl Error {
    pub fn to_status_code(&self) -> StatusCode {
        match self {
            Error::ErrorInConvertingHeaderValueToString { .. } => StatusCode::BAD_REQUEST,
            Error::AuthenticationFailed => StatusCode::FORBIDDEN,
            Error::Internal(_e) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        lang_graphql::http::Response::error_message_with_status(
            self.to_status_code(),
            self.to_string(),
        )
        .into_response()
    }
}

fn serialize_url<S>(url: &Url, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.serialize_str(url.as_str())
}

fn deserialize_url<'de, D>(deserializer: D) -> Result<Url, D::Error>
where
    D: Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?;

    Url::parse(&buf).map_err(SerdeDeError::custom)
}

#[derive(Serialize, Deserialize, Clone, Debug, JsonSchema, PartialEq)]
#[schemars(title = "AuthHookMethod")]
pub enum AuthHookMethod {
    Get,
    Post,
}

#[derive(Serialize, Deserialize, Clone, Debug, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "AuthHookConfig")]
#[schemars(example = "AuthHookConfig::example")]
/// The configuration of the authentication webhook.
pub struct AuthHookConfig {
    #[serde(serialize_with = "serialize_url", deserialize_with = "deserialize_url")]
    /// The URL of the authentication webhook.
    pub url: Url,
    /// The HTTP method to be used to make the request to the auth hook.
    pub method: AuthHookMethod,
}

impl AuthHookConfig {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "url": "http://auth_hook:3050/validate-request",
                "method": "Post"
            }
        "#,
        )
        .unwrap()
    }
}

/// The body of the POST request to be made to the Auth hook.
#[derive(Serialize, Debug)]
struct AuthHookPostRequestBody {
    headers: HashMap<String, String>,
}

async fn make_auth_hook_request(
    // HTTP client that needs to be passed to make the
    // HTTP request to the auth hook.
    http_client: &reqwest::Client,
    auth_hook_config: &AuthHookConfig,
    client_headers: &HeaderMap,
    allow_role_emulation_for: Option<&Role>,
) -> Result<auth_base::Identity, Error> {
    let tracer = tracing_util::global_tracer();
    let http_request_builder = match auth_hook_config.method {
        AuthHookMethod::Get => {
            let mut auth_hook_headers = tracing_util::get_trace_headers();
            for (header_name, header_value) in client_headers {
                if !ignore_header(header_name.as_str()) {
                    auth_hook_headers.insert(header_name, header_value.clone());
                }
            }
            http_client
                .get(auth_hook_config.url.clone())
                .headers(auth_hook_headers)
                .timeout(Duration::from_secs(60))
        }
        AuthHookMethod::Post => {
            let mut auth_hook_headers = HashMap::new();
            for (header_name, header_value) in client_headers {
                auth_hook_headers.insert(
                    header_name.to_string(),
                    header_value
                        .to_str()
                        .map_err(|e| Error::ErrorInConvertingHeaderValueToString {
                            error: e,
                            header_name: header_name.clone(),
                        })?
                        .to_string(),
                );
            }
            let request_body = AuthHookPostRequestBody {
                headers: auth_hook_headers,
            };
            http_client
                .post(auth_hook_config.url.clone())
                .headers(tracing_util::get_trace_headers())
                .json(&request_body)
                .timeout(Duration::from_secs(60))
        }
    };

    let req = http_request_builder
        .build()
        .map_err(InternalError::ReqwestError)?;

    let response = tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    http_client
                        .execute(req)
                        .await
                        .map_err(InternalError::ErrorWhileMakingHTTPRequestToTheAuthHook)
                })
            },
        )
        .await?;

    match response.status() {
        reqwest::StatusCode::UNAUTHORIZED => Err(Error::AuthenticationFailed),
        reqwest::StatusCode::OK => {
            let auth_hook_response: HashMap<String, String> =
                response.json().await.map_err(InternalError::ReqwestError)?;
            let mut session_variables = HashMap::new();
            for (k, v) in &auth_hook_response {
                match SessionVariable::from_str(k) {
                    Ok(session_variable) => {
                        session_variables
                            .insert(session_variable, SessionVariableValue(v.to_string()));
                    }
                    Err(_e) => {}
                }
            }
            let role = auth_base::Role::new(
                session_variables
                    .get(&session_variables::SESSION_VARIABLE_ROLE)
                    .ok_or(InternalError::RoleSessionVariableNotFound)?
                    .0
                    .as_str(),
            );
            let role_authorization = RoleAuthorization {
                role: role.clone(),
                session_variables,
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            };
            let mut allowed_roles = HashMap::new();
            allowed_roles.insert(role.clone(), role_authorization);

            Ok(match allow_role_emulation_for {
                Some(emulation_role) => {
                    if role == *emulation_role {
                        Identity::RoleEmulationEnabled(role)
                    } else {
                        Identity::Specific {
                            default_role: role,
                            allowed_roles,
                        }
                    }
                }
                None => Identity::Specific {
                    default_role: role,
                    allowed_roles,
                },
            })
        }
        status_code => Err(InternalError::AuthHookUnexpectedStatus(status_code))?,
    }
}

/// Makes the HTTP request to the auth hook. The webhook
/// is authenticates the request based on the client headers.
pub async fn authenticate_request(
    // HTTP client that needs to be passed to make the
    // HTTP request to the auth hook.
    http_client: &reqwest::Client,
    auth_hook_config: &AuthHookConfig,
    client_headers: &HeaderMap,
    allow_role_emulation_for: Option<&Role>,
) -> Result<auth_base::Identity, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "webhook_authenticate_request",
            "Webhook authenticate request",
            SpanVisibility::Internal,
            || {
                Box::pin(make_auth_hook_request(
                    http_client,
                    auth_hook_config,
                    client_headers,
                    allow_role_emulation_for,
                ))
            },
        )
        .await
}

/// Ignore the following list of request headers, sent by the client when making a GET request to
/// the auth hook.
///
/// Note that, in the case the auth hook mode is `POST`, this is *not* applicable, i.e. all the
/// headers sent by the client are forwarded to the auth hook.
const COMMON_CLIENT_HEADERS_TO_IGNORE: [&str; 14] = [
    "Accept",
    "Accept-Datetime",
    "Accept-Encoding",
    "Accept-Language",
    "Cache-Control",
    "Connection",
    "Content-Length",
    "Content-MD5",
    "Content-Type",
    "DNT",
    "Host",
    "Origin",
    "Referer",
    "User-Agent",
];

/// Decides whether to ignore the given header sent by the client when making a GET request to
/// the auth hook.
///
/// The header is compared to a static list of headers (above).
///
/// Note that, in the case the auth hook mode is `POST`, this is *not* applicable, i.e. all the
/// headers sent by the client are forwarded to the auth hook.
fn ignore_header(header: &str) -> bool {
    static CELL: OnceLock<HashSet<String>> = OnceLock::new();
    CELL.get_or_init(|| HashSet::from(COMMON_CLIENT_HEADERS_TO_IGNORE.map(str::to_lowercase)))
        .contains(header)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;
    use auth_base::Role;
    use mockito;
    use rand::{thread_rng, Rng};
    use reqwest::header::CONTENT_TYPE;

    #[tokio::test]
    // This test emulates a successful authentication by the webhook using Get method
    async fn test_get_successful_webhook_auth() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        // Create a mock
        let mock = server
            .mock("GET", "/validate-request")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_header("x-api-key", "1234")
            .with_body("{\"x-hasura-role\": \"test-role\", \"x-hasura-test-role-id\": \"1\"}")
            .match_header("foo", "baz") // Make sure, other than the common client headers, other headers are forwarded correctly.
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Get\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());

        let auth_response =
            make_auth_hook_request(&http_client, &auth_hook_config, &client_headers, None)
                .await
                .unwrap();

        mock.assert(); // Make sure the webhook has been called.

        let test_role = Role::new("test-role");
        let mut expected_allowed_roles = HashMap::new();
        let mut role_authorization_session_variables = HashMap::new();
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::new("test-role"),
        );
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-test-role-id").unwrap(),
            SessionVariableValue::new("1"),
        );
        expected_allowed_roles.insert(
            test_role.clone(),
            RoleAuthorization {
                role: test_role,
                session_variables: role_authorization_session_variables,
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );
        assert_eq!(
            auth_response,
            Identity::Specific {
                default_role: Role::new("test-role"),
                allowed_roles: expected_allowed_roles
            }
        );
    }

    #[tokio::test]
    // This test emulates a successful authentication by the webhook using Post method
    async fn test_post_webhook_successful_post_auth() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        // Create a mock
        let mock = server
            .mock("POST", "/validate-request")
            .match_body(r#"{"headers":{"foo":"baz"}}"#)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{
                      "x-hasura-role": "test-role",
                      "x-hasura-test-role-id": "1"
                   }"#,
            )
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Post\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());

        let auth_response =
            make_auth_hook_request(&http_client, &auth_hook_config, &client_headers, None)
                .await
                .unwrap();

        mock.assert(); // Make sure the webhook has been called.

        let test_role = Role::new("test-role");
        let mut expected_allowed_roles = HashMap::new();
        let mut role_authorization_session_variables = HashMap::new();
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::new("test-role"),
        );
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-test-role-id").unwrap(),
            SessionVariableValue::new("1"),
        );
        expected_allowed_roles.insert(
            test_role.clone(),
            RoleAuthorization {
                role: test_role,
                session_variables: role_authorization_session_variables,
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );
        assert_eq!(
            auth_response,
            Identity::Specific {
                default_role: Role::new("test-role"),
                allowed_roles: expected_allowed_roles
            }
        );
    }

    #[tokio::test]
    // This test emulates the scenario where the webhook may return non-hasura session variables
    async fn test_allow_non_hasura_session_variables_in_webhook_response() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        // Create a mock
        let mock = server
            .mock("POST", "/validate-request")
            .match_body(r#"{"headers":{"foo":"baz"}}"#)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{
                      "x-hasura-role": "test-role",
                      "x-hasura-test-role-id": "1",
                      "status": "true"
                   }"#,
            )
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Post\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());

        let auth_response =
            make_auth_hook_request(&http_client, &auth_hook_config, &client_headers, None)
                .await
                .unwrap();

        mock.assert(); // Make sure the webhook has been called.

        let test_role = Role::new("test-role");
        let mut expected_allowed_roles = HashMap::new();
        let mut role_authorization_session_variables = HashMap::new();
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::new("test-role"),
        );
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-test-role-id").unwrap(),
            SessionVariableValue::new("1"),
        );
        role_authorization_session_variables.insert(
            SessionVariable::from_str("status").unwrap(),
            SessionVariableValue::new("true"),
        );
        expected_allowed_roles.insert(
            test_role.clone(),
            RoleAuthorization {
                role: test_role,
                session_variables: role_authorization_session_variables,
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );
        assert_eq!(
            auth_response,
            Identity::Specific {
                default_role: Role::new("test-role"),
                allowed_roles: expected_allowed_roles
            }
        );
    }

    #[tokio::test]
    // This test emulates the scenario where role emulation is allowed
    async fn test_role_emulation() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        // Create a mock
        let mock = server
            .mock("POST", "/validate-request")
            .match_body(r#"{"headers":{"foo":"baz"}}"#)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{
                      "x-hasura-role": "test-admin-role",
                      "x-hasura-user-id": "1"
                   }"#,
            )
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Post\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());

        let auth_response = make_auth_hook_request(
            &http_client,
            &auth_hook_config,
            &client_headers,
            Some(&Role::new("test-admin-role")),
        )
        .await
        .unwrap();

        mock.assert(); // Make sure the webhook has been called.

        let test_role = Role::new("test-admin-role");
        assert_eq!(auth_response, Identity::RoleEmulationEnabled(test_role));
    }

    #[tokio::test]
    // This test emulates the scenario where role emulation is denied
    async fn test_deny_role_emulation() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        // Create a mock
        let mock = server
            .mock("POST", "/validate-request")
            .match_body(r#"{"headers":{"foo":"baz"}}"#)
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{
                      "x-hasura-role": "user",
                      "x-hasura-user-id": "1"
                   }"#,
            )
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Post\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());

        let auth_response = make_auth_hook_request(
            &http_client,
            &auth_hook_config,
            &client_headers,
            Some(&Role::new("test-admin-role")),
        )
        .await
        .unwrap();

        mock.assert(); // Make sure the webhook has been called.

        let test_role = Role::new("user");
        let mut expected_allowed_roles = HashMap::new();
        let mut role_authorization_session_variables = HashMap::new();
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-role").unwrap(),
            SessionVariableValue::new("user"),
        );
        role_authorization_session_variables.insert(
            SessionVariable::from_str("x-hasura-user-id").unwrap(),
            SessionVariableValue::new("1"),
        );
        expected_allowed_roles.insert(
            test_role.clone(),
            RoleAuthorization {
                role: test_role.clone(),
                session_variables: role_authorization_session_variables,
                allowed_session_variables_from_request: auth_base::SessionVariableList::Some(
                    HashSet::new(),
                ),
            },
        );
        assert_eq!(
            auth_response,
            Identity::Specific {
                default_role: test_role,
                allowed_roles: expected_allowed_roles
            }
        );
    }

    #[tokio::test]
    /// This test emulates denial of the  authentication
    /// by sending a 401 status
    async fn test_post_webhook_denies_authentication() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        // Create a mock
        let mock = server
            .mock("POST", "/validate-request")
            .with_status(401)
            .with_header("content-type", "application/json")
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Post\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());
        client_headers.insert(CONTENT_TYPE, "application/json".parse().unwrap());

        let auth_response =
            make_auth_hook_request(&http_client, &auth_hook_config, &client_headers, None).await;

        mock.assert(); // Make sure the webhook has been called.

        assert_eq!(
            auth_response.unwrap_err().to_string(),
            "The Authentication hook has denied to execute the request."
        );
    }

    #[tokio::test]
    /// Test HTTP status codes returned by the webhook,
    /// other than 200 and 401 are not recognized.
    async fn test_webhook_returning_arbitrary_status() {
        // Request a new server from the pool
        let mut server = mockito::Server::new_async().await;

        let url = server.url();

        let mut rng = thread_rng();

        // Generate a random HTTP status code
        let mut http_status_code: usize = rng.gen_range(100..600);

        // Make sure that it's not either 200/401.
        while http_status_code == 200 || http_status_code == 401 {
            http_status_code = rng.gen_range(100..600);
        }

        // Create a mock
        let mock = server
            .mock("POST", "/validate-request")
            .with_status(http_status_code)
            .with_header("content-type", "application/json")
            .create();

        let http_client = reqwest::Client::new();

        let webhook_url = url + "/validate-request";

        let auth_hook_config_str =
            format!("{{ \"url\": \"{webhook_url}\", \"method\": \"Post\"  }}");

        let auth_hook_config: AuthHookConfig = serde_json::from_str(&auth_hook_config_str).unwrap();

        let mut client_headers = HeaderMap::new();
        client_headers.insert("foo", "baz".parse().unwrap());
        client_headers.insert(CONTENT_TYPE, "application/json".parse().unwrap());

        let auth_response =
            make_auth_hook_request(&http_client, &auth_hook_config, &client_headers, None).await;

        mock.assert(); // Make sure the webhook has been called.

        assert_eq!(
            auth_response
                .unwrap_err()
                .to_string()
                .split('.')
                .collect::<Vec<&str>>()[1]
                .trim(),
            "Only 200 and 401 response status are recognized"
        );
    }
}
