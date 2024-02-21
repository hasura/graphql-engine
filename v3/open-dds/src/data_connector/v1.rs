use indexmap::IndexMap;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::EnvironmentValue;

use super::{DataConnectorName, VersionedSchemaAndCapabilities};

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
    /// The schema of the data connector. This schema is used as the source of truth when
    /// serving requests and the live schema of the data connector is not looked up.
    pub schema: VersionedSchemaAndCapabilities,
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
              "schema": {
                "version": "v0.1",
                "capabilities": {
                  "version": "0.1.0" ,
                  "capabilities": {
                    "query": {
                      "variables": {}
                    },
                    "mutation": {}
                  }
                },
                "schema": {
                  "scalar_types": {},
                  "object_types": {},
                  "collections": [],
                  "functions": [],
                  "procedures": []
              }
              }
            }
        "#,
        )
        .unwrap()
    }
}
