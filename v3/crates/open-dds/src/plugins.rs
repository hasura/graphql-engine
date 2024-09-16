use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{data_connector::HttpHeaders, impl_OpenDd_default_for, EnvironmentValue};

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
    Parse(LifecyclePluginHookPreParse),
}

type LifecyclePluginUrl = EnvironmentValue;

type LifecyclePluginName = String;

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePluginHookPreParse")]
/// Definition of a lifecycle plugin hook for the pre-parse stage.
pub struct LifecyclePluginHookPreParse {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePluginHookConfig,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePluginHookConfig")]
/// Configuration for a lifecycle plugin hook.
pub struct LifecyclePluginHookConfig {
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePluginHookConfigRequest,
}

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "LifecyclePluginHookConfigRequest")]
/// Configuration for a lifecycle plugin hook request.
pub struct LifecyclePluginHookConfigRequest {
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

#[test]
fn test_lifecycle_plugin_hook_parse() {
    let hook = LifecyclePluginHook::V1(LifecyclePluginHookV1::Parse(LifecyclePluginHookPreParse {
        name: "test".to_string(),
        url: crate::EnvironmentValue {
            value: "http://localhost:8080".to_string(),
        },
        config: LifecyclePluginHookConfig {
            request: LifecyclePluginHookConfigRequest {
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
