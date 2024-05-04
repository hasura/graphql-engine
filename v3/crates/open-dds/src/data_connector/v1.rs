use std::ops::Deref;

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

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[schemars(title = "DataConnectorUrlV1")]
#[serde(rename_all = "camelCase")]
pub enum DataConnectorUrlV1 {
    SingleUrl(EnvironmentValue),
    ReadWriteUrls(ReadWriteUrls),
}

#[derive(Serialize, Default, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
/// Key value map of HTTP headers to be sent with an HTTP request. The key is the
/// header name and the value is a potential reference to an environment variable.
// We wrap maps into newtype structs so that we have a type and title for them in the JSONSchema which
// makes it easier to auto-generate documentation.
pub struct HttpHeaders(pub IndexMap<String, EnvironmentValue>);

impl Deref for HttpHeaders {
    type Target = IndexMap<String, EnvironmentValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "DataConnectorLinkV1",))]
/// Definition of a data connector - version 1.
pub struct DataConnectorLinkV1 {
    /// The name of the data connector.
    pub name: DataConnectorName,
    /// The url(s) to access the data connector.
    pub url: DataConnectorUrlV1,
    #[opendd(default)]
    /// Key value map of HTTP headers to be sent with each request to the data connector.
    pub headers: HttpHeaders,
    /// The schema of the data connector. This schema is used as the source of truth when
    /// serving requests and the live schema of the data connector is not looked up.
    pub schema: VersionedSchemaAndCapabilities,
}
