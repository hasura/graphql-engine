use indexmap::IndexMap;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::EnvironmentValue;

use super::{
    ndc_schema_response_schema_reference, CapabilitiesResponseWithSchema, DataConnectorName,
    VersionedSchemaResponse,
};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ReadWriteUrls")]
pub struct ReadWriteUrls {
    pub read: EnvironmentValue,
    pub write: EnvironmentValue,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[schemars(title = "DataConnectorUrlV1")]
#[serde(rename_all = "camelCase")]
pub enum DataConnectorUrlV1 {
    SingleUrl(EnvironmentValue),
    ReadWriteUrls(ReadWriteUrls),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "DataConnectorLinkV1")]
#[schemars(example = "DataConnectorLinkV1::example")]
/// Definition of a data connector - version 1.
pub struct DataConnectorLinkV1 {
    /// The name of the data connector.
    pub name: DataConnectorName,
    /// The url(s) to access the data connector.
    pub url: DataConnectorUrlV1,
    #[serde(default)]
    /// Key value map of HTTP headers to be sent with each request to the data connector.
    pub headers: IndexMap<String, EnvironmentValue>,
    #[serde(default)]
    #[schemars(schema_with = "ndc_schema_response_schema_reference")]
    /// The schema of the data connector. This schema is used as the source of truth when
    /// serving requests and the live schema of the data connector is not looked up.
    pub schema: VersionedSchemaResponse,
    /// The capabilities of the data connector.
    pub capabilities: CapabilitiesResponseWithSchema,
}

impl DataConnectorLinkV1 {
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
