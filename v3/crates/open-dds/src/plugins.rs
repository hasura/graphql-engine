use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    EnvironmentValue,
    data_connector::{DataConnectorName, HttpHeaders},
    impl_OpenDd_default_for,
};

#[derive(
    Serialize, Deserialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd, JsonSchema,
)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "LifecyclePluginHook",
        example = "LifecyclePluginHook::example"
    )
)]
#[schemars(title = "LifecyclePluginHook")]
/// Definition of a lifecycle plugin hook.
pub enum LifecyclePluginHook {
    V1(LifecyclePluginHookV1),
}

impl LifecyclePluginHook {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "LifecyclePluginHook",
                "version": "v1",
                "definition": {
                    "pre": "parse",
                    "name": "test",
                    "url": {
                        "value": "http://localhost:8080",
                    },
                    "config": {
                        "request": {
                            "headers": {
                                "additional": {
                                    "hasura-m-auth": {
                                        "value": "zZkhKqFjqXR4g5MZCsJUZCnhCcoPyZ"
                                    }
                                }
                            },
                            "session": {},
                            "rawRequest": {
                                "query": {},
                                "variables": {}
                            }
                        }
                    }
                }
            }
        )
    }

    pub fn upgrade(self) -> LifecyclePluginHookV1 {
        match self {
            LifecyclePluginHook::V1(v1) => v1,
        }
    }
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "pre")]
#[schemars(title = "LifecyclePluginHookV1")]
#[serde(deny_unknown_fields)]
/// Definition of a lifecycle plugin hook - version 1.
pub enum LifecyclePluginHookV1 {
    /// Definition of a lifecycle plugin hook for the pre-parse stage.
    Parse(LifecyclePreParsePluginHook),
    /// Definition of a lifecycle plugin hook for the pre-response stage.
    Response(LifecyclePreResponsePluginHook),
    /// Definition of a lifecycle plugin hook for the pre-route stage.
    Route(LifecyclePreRoutePluginHook),
    /// Definition of a lifecycle plugin hook for the pre-ndc-request stage.
    NdcRequest(LifecyclePreNdcRequestPluginHook),
    /// Definition of a lifecycle plugin hook for the pre-ndc-response stage.
    NdcResponse(LifecyclePreNdcResponsePluginHook),
}

pub type LifecyclePluginUrl = EnvironmentValue;

pub type LifecyclePluginName = String;

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreParsePluginHook")]
/// Definition of a lifecycle plugin hook for the pre-parse stage.
pub struct LifecyclePreParsePluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreParsePluginHookConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreParsePluginHookConfig")]
/// Configuration for a lifecycle plugin hook.
pub struct LifecyclePreParsePluginHookConfig {
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreParsePluginHookConfigRequest,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreParsePluginHookConfigRequest")]
/// Configuration for a lifecycle plugin hook request.
pub struct LifecyclePreParsePluginHookConfigRequest {
    /// Configuration for the headers.
    pub headers: Option<LifecyclePluginHookHeadersConfig>,
    /// Configuration for the session (includes roles and session variables).
    pub session: Option<LeafConfig>,
    /// Configuration for the raw request.
    pub raw_request: RawRequestConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePluginHookHeadersConfig")]
/// Configuration for a lifecycle plugin hook headers.
pub struct LifecyclePluginHookHeadersConfig {
    /// Additional headers to be sent with the request.
    pub additional: Option<HttpHeaders>,
    #[serde(default)]
    /// Headers to be forwarded from the incoming request.
    pub forward: Vec<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, Eq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LeafConfig")]
/// Leaf Configuration.
pub struct LeafConfig {}

impl_OpenDd_default_for!(LeafConfig);

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RawRequestConfig")]
/// Configuration for the raw request.
pub struct RawRequestConfig {
    /// Configuration for the query.
    pub query: Option<LeafConfig>,
    /// Configuration for the variables.
    pub variables: Option<LeafConfig>,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreResponsePluginHook")]
/// Definition of a lifecycle plugin hook for the pre-parse stage.
pub struct LifecyclePreResponsePluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreResponsePluginHookConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreResponsePluginHookConfig")]
/// Configuration for a lifecycle plugin hook.
pub struct LifecyclePreResponsePluginHookConfig {
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreResponsePluginHookConfigRequest,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreResponsePluginHookConfigRequest")]
/// Configuration for a lifecycle plugin hook request.
pub struct LifecyclePreResponsePluginHookConfigRequest {
    /// Configuration for the headers.
    pub headers: Option<LifecyclePluginHookHeadersConfig>,
    /// Configuration for the session (includes roles and session variables).
    pub session: Option<LeafConfig>,
    /// Configuration for the raw request.
    pub raw_request: RawRequestConfig,
    /// Configuration for the response.
    pub raw_response: Option<LeafConfig>,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreRoutePluginHook")]
/// Definition of a lifecycle plugin hook for the pre-route stage.
pub struct LifecyclePreRoutePluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreRoutePluginHookConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[schemars(title = "RequestMethod")]
#[serde(deny_unknown_fields)]
/// Possible HTTP Request Methods for the incoming requests handled by the pre-route plugin hook.
pub enum LifecyclePreRoutePluginHookIncomingHTTPMethod {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreRoutePluginHookConfig")]
/// Configuration for a lifecycle plugin hook.
pub struct LifecyclePreRoutePluginHookConfig {
    #[serde(alias = "match")]
    #[opendd(alias = "match")]
    /// Regex to match the request path
    pub match_path: String,
    /// Possible HTTP methods for the request
    pub match_methods: Vec<LifecyclePreRoutePluginHookIncomingHTTPMethod>,
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreRoutePluginHookConfigRequest,
    #[serde(default)]
    /// Configuration for the response to the lifecycle plugin hook.
    pub response: Option<LifecyclePreRoutePluginHookConfigResponse>,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreRoutePluginHookConfigRequest")]
/// Configuration for a lifecycle plugin hook request.
pub struct LifecyclePreRoutePluginHookConfigRequest {
    /// Configuration for the headers in the pre-route plugin hook HTTP requests.
    pub headers: Option<LifecyclePluginHookHeadersConfig>,
    /// Configuration for the HTTP method for the pre-route plugin hook HTTP requests.
    pub method: LifecyclePreRoutePluginHookConfigRequestMethod,
    // Configuration for the request body for the pre-route plugin hook HTTP requests.
    pub raw_request: PreRouteRequestConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[schemars(title = "LifecyclePreRoutePluginHookConfigRequestMethods")]
#[serde(deny_unknown_fields)]
/// Configuration for the method for the pre-route plugin hook HTTP requests.
pub enum LifecyclePreRoutePluginHookConfigRequestMethod {
    GET,
    POST,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[schemars(title = "PreRouteRequestConfig")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
/// Configuration for the raw request body for the pre-route plugin hook HTTP requests.
pub struct PreRouteRequestConfig {
    /// Configuration for adding/excluding the request path of the incoming request
    pub path: Option<LeafConfig>,
    /// Configuration for adding/excluding the request method of the incoming request
    pub method: Option<LeafConfig>,
    /// Configuration for adding/excluding the query params of the incoming request
    pub query: Option<LeafConfig>,
    /// Configuration for adding/excluding the body of the incoming request
    pub body: Option<LeafConfig>,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreRoutePluginHookConfigResponse")]
/// Configuration for a lifecycle plugin hook response.
pub struct LifecyclePreRoutePluginHookConfigResponse {
    /// Configuration for the headers in the response from the engine.
    pub headers: Option<LifecyclePluginHookHeadersConfig>,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreNdcRequestPluginHook")]
pub struct LifecyclePreNdcRequestPluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// A list of data connectors that this plugin hook should be applied to.
    /// There can only be one plugin hook of this type per data connector.
    pub connectors: Vec<DataConnectorName>,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreNdcRequestPluginHookConfig,
}

/// config for pre-ndc-request hook
#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreNdcRequestPluginHookConfig")]
/// Configuration for a lifecycle plugin hook.
pub struct LifecyclePreNdcRequestPluginHookConfig {
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreNdcRequestPluginHookConfigRequest,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreNdcRequestPluginHookConfigRequest")]
/// Configuration for a lifecycle plugin hook request.
pub struct LifecyclePreNdcRequestPluginHookConfigRequest {
    /// Configuration for the headers.
    pub headers: Option<HttpHeaders>,
    /// Configuration for the session (includes roles and session variables).
    pub session: Option<LeafConfig>,
    /// Configuration for the request.
    pub ndc_request: Option<LeafConfig>,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreNdcResponsePluginHook")]
pub struct LifecyclePreNdcResponsePluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// A list of data connectors that this plugin hook should be applied to.
    /// There can only be one plugin hook of this type per data connector.
    pub connectors: Vec<DataConnectorName>,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreNdcResponsePluginHookConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreNdcResponsePluginHookConfig")]
/// Configuration for a lifecycle plugin hook.
pub struct LifecyclePreNdcResponsePluginHookConfig {
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreNdcResponsePluginHookConfigRequest,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePreNdcResponsePluginHookConfigRequest")]
/// Configuration for a lifecycle plugin hook request.
pub struct LifecyclePreNdcResponsePluginHookConfigRequest {
    /// Configuration for the headers.
    pub headers: Option<HttpHeaders>,
    /// Configuration for the session (includes roles and session variables).
    pub session: Option<LeafConfig>,
    /// Configuration for the request.
    pub ndc_request: Option<LeafConfig>,
    /// Configuration for the response.
    pub ndc_response: Option<LeafConfig>,
}

#[test]
fn test_lifecycle_plugin_hook_parse() {
    let hook = LifecyclePluginHook::V1(LifecyclePluginHookV1::Parse(LifecyclePreParsePluginHook {
        name: "test".to_string(),
        url: crate::EnvironmentValue {
            value: "http://localhost:8080".to_string(),
        },
        config: LifecyclePreParsePluginHookConfig {
            request: LifecyclePreParsePluginHookConfigRequest {
                headers: Some(LifecyclePluginHookHeadersConfig {
                    additional: Some(HttpHeaders(
                        vec![(
                            "hasura-m-auth".to_string(),
                            crate::EnvironmentValue {
                                value: "zZkhKqFjqXR4g5MZCsJUZCnhCcoPyZ".to_string(),
                            },
                        )]
                        .into_iter()
                        .collect(),
                    )),
                    forward: Vec::default(),
                }),
                session: Some(LeafConfig {}),
                raw_request: RawRequestConfig {
                    query: Some(LeafConfig {}),
                    variables: Some(LeafConfig {}),
                },
            },
        },
    }));

    let json = serde_json::to_string(&hook).unwrap();
    let _hook: LifecyclePluginHook = serde_json::from_str(&json).unwrap();
    assert_eq!(hook, _hook);

    let json = LifecyclePluginHook::example();
    let _hook: LifecyclePluginHook = serde_json::from_value(json).unwrap();
    assert_eq!(hook, _hook);
}
