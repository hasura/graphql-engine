use indexmap::IndexMap;
use ndc_client::models::SchemaResponse;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::SecretValue;

use super::{
    ndc_schema_response_schema_reference, CapabilitiesResponseWithSchema, DataConnectorName,
};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ReadWriteUrlsV2")]
pub struct ReadWriteUrlsV2 {
    pub read: SecretValue,
    pub write: SecretValue,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[schemars(title = "DataConnectorUrlV2")]
#[serde(rename_all = "camelCase")]
pub enum DataConnectorUrlV2 {
    SingleUrl(SecretValue),
    ReadWriteUrls(ReadWriteUrlsV2),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "DataConnectorV2")]
#[schemars(example = "DataConnectorV2::example")]
/// Definition of a data connector - version 2.
pub struct DataConnectorV2 {
    /// The name of the data connector.
    pub name: DataConnectorName,
    /// The url(s) to access the data connector.
    pub url: DataConnectorUrlV2,
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

impl DataConnectorV2 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
              "name": "data_connector",
              "url": {
                "singleUrl": {
                  "value": "http://data_connector:8100"
                }
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
