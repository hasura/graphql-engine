use indexmap::IndexMap;
use ndc_client::models::SchemaResponse;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::SecretValue;

use super::{
    ndc_schema_response_schema_reference, CapabilitiesResponseWithSchema, DataConnectorName,
    DataConnectorUrl, DataConnectorV2, ReadWriteUrls,
};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ReadWriteUrlsV1")]
pub struct ReadWriteUrlsV1 {
    pub read: String,
    pub write: String,
}

impl ReadWriteUrlsV1 {
    fn upgrade(self) -> ReadWriteUrls {
        ReadWriteUrls {
            read: SecretValue { value: self.read },
            write: SecretValue { value: self.write },
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[schemars(title = "DataConnectorUrlV1")]
#[serde(rename_all = "camelCase")]
pub enum DataConnectorUrlV1 {
    SingleUrl(String),
    ReadWriteUrls(ReadWriteUrlsV1),
}

impl DataConnectorUrlV1 {
    fn upgrade(self) -> DataConnectorUrl {
        match self {
            Self::SingleUrl(value) => DataConnectorUrl::SingleUrl(SecretValue { value }),
            Self::ReadWriteUrls(urls) => DataConnectorUrl::ReadWriteUrls(urls.upgrade()),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "DataConnectorV1")]
#[schemars(example = "DataConnectorV1::example")]
/// Definition of a data connector - version 1.
pub struct DataConnectorV1 {
    /// The name of the data connector.
    pub name: DataConnectorName,
    /// The url(s) to access the data connector.
    pub url: DataConnectorUrlV1,
    #[serde(default)]
    /// Key value map of HTTP headers to be sent with each request to the data connector.
    pub headers: IndexMap<String, SecretValue>,
    #[serde(default)]
    #[schemars(schema_with = "ndc_schema_response_schema_reference")]
    /// The schema of the data connector. This schema is used as the source of truth when
    /// serving requests and the live schema of the data connector is not looked up.
    pub schema: SchemaResponse,
    /// The capabilities of the data connector. This field is required, and is marked
    /// optional only for temporary backwards compatibility.
    pub capabilities: Option<CapabilitiesResponseWithSchema>,
}

impl DataConnectorV1 {
    pub fn upgrade(self) -> DataConnectorV2 {
        DataConnectorV2 {
            name: self.name,
            url: self.url.upgrade(),
            headers: self.headers,
            schema: self.schema,
            capabilities: self.capabilities,
        }
    }

    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
              "name": "data_connector",
              "url": {
                "singleUrl": "http://data_connector:8100"
              },
              "capabilities": {
                "versions": "*",
                "capabilities": {
                  "query": {
                    "variables": {}
                  }
                }
              }
            }
        "#,
        )
        .unwrap()
    }
}
