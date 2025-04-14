use std::ops::Deref;

use indexmap::IndexMap;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{EnvironmentValue, permissions::ValueExpression, types::DataConnectorArgumentName};

use super::{DataConnectorColumnName, DataConnectorName, VersionedSchemaAndCapabilities};

/// A pair of URLs to access a data connector, one for reading and one for writing.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "ReadWriteUrls"))]
pub struct ReadWriteUrls {
    pub read: EnvironmentValue,
    pub write: EnvironmentValue,
}

/// A URL to access a data connector. This can be a single URL or a pair of read and write URLs.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[opendd(externally_tagged, json_schema(title = "DataConnectorUrlV1"))]
#[serde(rename_all = "camelCase")]
pub enum DataConnectorUrlV1 {
    // #[opendd(json_schema(title = "SingleUrl"))]
    SingleUrl(EnvironmentValue),
    // #[opendd(json_schema(title = "ReadWriteUrls"))]
    ReadWriteUrls(ReadWriteUrls),
}

#[derive(
    Serialize, Deserialize, Default, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd, JsonSchema,
)]
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
    /// This is meant for protocol level use between engine and the data connector.
    pub headers: HttpHeaders,
    /// The schema of the data connector. This schema is used as the source of truth when
    /// serving requests and the live schema of the data connector is not looked up.
    pub schema: VersionedSchemaAndCapabilities,
    /// Argument presets that applies to all functions and procedures of this
    /// data connector. Defaults to no argument presets.
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub argument_presets: Vec<DataConnectorArgumentPreset>,
    /// HTTP response headers configuration that is forwarded from a data
    /// connector to the client.
    pub response_headers: Option<ResponseHeaders>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
/// An argument preset that can be applied to all functions/procedures of a
/// connector
pub struct DataConnectorArgumentPreset {
    pub argument: DataConnectorArgumentName,
    pub value: DataConnectorArgumentPresetValue,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
/// The value of a data connector argument preset.
pub struct DataConnectorArgumentPresetValue {
    /// HTTP headers that can be preset from request
    pub http_headers: HttpHeadersPreset,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
/// Configuration of what HTTP request headers should be forwarded to a data
/// connector.
pub struct HttpHeadersPreset {
    /// List of HTTP headers that should be forwarded from HTTP requests
    pub forward: Vec<String>,
    /// Additional headers that should be forwarded, from other contexts
    pub additional: AdditionalHttpHeaders,
}

#[derive(Serialize, Default, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
/// Key value map of HTTP headers to be forwarded in the headers argument of a data connector request.
// We wrap maps into newtype structs so that we have a type and title for them
// in the JSONSchema which makes it easier to auto-generate documentation.
pub struct AdditionalHttpHeaders(pub IndexMap<String, ValueExpression>);

impl Deref for AdditionalHttpHeaders {
    type Target = IndexMap<String, ValueExpression>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd, Eq)]
#[serde(rename_all = "camelCase")]
/// Configuration of what HTTP response headers should be forwarded from a data
/// connector to the client in HTTP response.
pub struct ResponseHeaders {
    /// Name of the field in the NDC function/procedure's result which contains
    /// the response headers
    pub headers_field: DataConnectorColumnName,
    /// Name of the field in the NDC function/procedure's result which contains
    /// the result
    pub result_field: DataConnectorColumnName,
    /// List of actual HTTP response headers from the data connector to be set as
    /// response headers
    pub forward_headers: Vec<String>,
}
