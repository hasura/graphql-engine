use reqwest::Url;
use schemars::JsonSchema;
use serde::{de::Error as SerdeDeError, Deserialize, Deserializer, Serialize, Serializer};

#[derive(Serialize, Deserialize, Clone, Debug, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RequestConfig")]
pub struct RequestConfig {
    pub headers: bool,
    pub session: bool,
    pub raw_request: RawRequestConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, JsonSchema, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RawRequestConfig")]
pub struct RawRequestConfig {
    pub query: bool,
    pub variables: bool,
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
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "PrePluginConfig")]
#[schemars(example = "PrePluginConfig::example")]
pub struct PrePluginConfig {
    pub name: String,
    #[serde(serialize_with = "serialize_url", deserialize_with = "deserialize_url")]
    pub url: Url,
    pub request: RequestConfig,
}

impl PrePluginConfig {
    fn example() -> Self {
        serde_json::from_str(
            r#"{
                "name": "example",
                "url": "http://example.com",
                "request": {
                    "headers": true,
                    "session": true,
                    "rawRequest": {
                        "query": true,
                        "variables": true
                    }
                }
            }"#,
        )
        .unwrap()
    }
}
