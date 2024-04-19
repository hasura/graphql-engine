use super::error::Error;
use super::subgraph::Qualified;
use indexmap::IndexMap;

use super::stages::data_connector_scalar_types;
use lang_graphql::ast::common::OperationType;
use ndc_models;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorUrl, ReadWriteUrls},
    EnvironmentValue,
};
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use serde::{
    de::Error as DeError,
    ser::{Error as SerError, SerializeMap},
    Deserialize, Serialize,
};
use std::{collections::HashMap, str::FromStr};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorLink {
    pub name: Qualified<DataConnectorName>,
    pub url: ResolvedDataConnectorUrl,
    pub headers: SerializableHeaderMap,
}

impl std::hash::Hash for DataConnectorLink {
    fn hash<H>(&self, h: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.name.hash(h)
    }
}

impl DataConnectorLink {
    pub(crate) fn new(
        name: Qualified<DataConnectorName>,
        url: DataConnectorUrl,
        headers: &IndexMap<String, EnvironmentValue>,
    ) -> Result<Self, Error> {
        let url = match url {
            DataConnectorUrl::SingleUrl(url) => ResolvedDataConnectorUrl::SingleUrl(
                SerializableUrl::new(&url.value).map_err(|e| Error::InvalidDataConnectorUrl {
                    data_connector_name: name.clone(),
                    error: e,
                })?,
            ),
            DataConnectorUrl::ReadWriteUrls(ReadWriteUrls { read, write }) => {
                ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls {
                    read: SerializableUrl::new(&read.value).map_err(|e| {
                        Error::InvalidDataConnectorUrl {
                            data_connector_name: name.clone(),
                            error: e,
                        }
                    })?,
                    write: SerializableUrl::new(&write.value).map_err(|e| {
                        Error::InvalidDataConnectorUrl {
                            data_connector_name: name.clone(),
                            error: e,
                        }
                    })?,
                })
            }
        };
        let headers = SerializableHeaderMap::new(headers).map_err(|e| match e {
            HeaderError::InvalidHeaderName { header_name } => Error::InvalidHeaderName {
                data_connector: name.clone(),
                header_name,
            },
            HeaderError::InvalidHeaderValue { header_name } => Error::InvalidHeaderValue {
                data_connector: name.clone(),
                header_name,
            },
        })?;
        Ok(Self { name, url, headers })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SerializableUrl(pub reqwest::Url);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedReadWriteUrls {
    pub read: SerializableUrl,
    pub write: SerializableUrl,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum ResolvedDataConnectorUrl {
    SingleUrl(SerializableUrl),
    ReadWriteUrls(ResolvedReadWriteUrls),
}

impl ResolvedDataConnectorUrl {
    pub fn get_url(&self, operation: OperationType) -> reqwest::Url {
        match self {
            ResolvedDataConnectorUrl::SingleUrl(url) => url.0.clone(),
            ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls { read, write }) => {
                match operation {
                    OperationType::Query => read.0.clone(),
                    OperationType::Mutation => write.0.clone(),
                    OperationType::Subscription => write.0.clone(),
                }
            }
        }
    }
}

impl SerializableUrl {
    pub fn new(url: &str) -> Result<Self, url::ParseError> {
        let url = reqwest::Url::parse(url)?;
        Ok(Self(url))
    }
}

impl Serialize for SerializableUrl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.0.as_str())
    }
}

impl<'de> Deserialize<'de> for SerializableUrl {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let url_str = String::deserialize(deserializer)?;
        let url = reqwest::Url::parse(&url_str)
            .map_err(|_| D::Error::custom(format!("Invalid URL: {url_str}")))?;
        Ok(SerializableUrl(url))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SerializableHeaderMap(pub HeaderMap);

pub enum HeaderError {
    InvalidHeaderName { header_name: String },
    InvalidHeaderValue { header_name: String },
}

impl SerializableHeaderMap {
    fn new(headers: &IndexMap<String, EnvironmentValue>) -> Result<Self, HeaderError> {
        let header_map = headers
            .iter()
            .map(|(k, v)| {
                Ok((
                    HeaderName::from_str(k).map_err(|_| HeaderError::InvalidHeaderName {
                        header_name: k.clone(),
                    })?,
                    HeaderValue::from_str(&v.value).map_err(|_| {
                        HeaderError::InvalidHeaderValue {
                            header_name: k.clone(),
                        }
                    })?,
                ))
            })
            .collect::<Result<HeaderMap, HeaderError>>()?;
        Ok(Self(header_map))
    }
}

impl Serialize for SerializableHeaderMap {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.0.len()))?;
        for (k, v) in &self.0 {
            map.serialize_entry(k.as_str(), v.to_str().map_err(S::Error::custom)?)?;
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for SerializableHeaderMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let hash_map = HashMap::<String, String>::deserialize(deserializer)?;
        let header_map: HeaderMap = hash_map
            .into_iter()
            .map(|(k, v)| {
                Ok((
                    HeaderName::from_str(&k).map_err(D::Error::custom)?,
                    HeaderValue::from_str(&v).map_err(D::Error::custom)?,
                ))
            })
            .collect::<Result<HeaderMap, D::Error>>()?;
        Ok(SerializableHeaderMap(header_map))
    }
}

// helper function to determine whether a ndc type is a simple scalar
pub fn get_simple_scalar<'a, 'b>(
    t: ndc_models::Type,
    scalars: &'a HashMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo<'b>>,
) -> Option<(
    String,
    &'a data_connector_scalar_types::ScalarTypeWithRepresentationInfo<'b>,
)> {
    match t {
        ndc_models::Type::Named { name } => scalars.get(name.as_str()).map(|info| (name, info)),
        ndc_models::Type::Nullable { underlying_type } => {
            get_simple_scalar(*underlying_type, scalars)
        }
        ndc_models::Type::Array { element_type: _ } => None,
        ndc_models::Type::Predicate {
            object_type_name: _,
        } => None,
    }
}

#[cfg(test)]
mod tests {
    use ndc_models;
    use open_dds::data_connector::DataConnectorLinkV1;

    use crate::metadata::resolved::stages::data_connectors::types::DataConnectorContext;

    #[test]
    fn test_url_serialization_deserialization() {
        let actual_url_str = r#"{"singleUrl":"http://hasura.io/"}"#;

        // Testing deserialization of url with trailing `/`
        let url: super::ResolvedDataConnectorUrl = serde_json::from_str(actual_url_str).unwrap();
        let serialized_url = serde_json::to_string(&url).unwrap();
        assert_eq!(actual_url_str, serialized_url);

        // Testing deserialization of url without trailing `/`
        let url_str = r#"{"singleUrl":"http://hasura.io"}"#;
        let url: super::ResolvedDataConnectorUrl = serde_json::from_str(url_str).unwrap();
        let serialized_url = serde_json::to_string(&url).unwrap();
        assert_eq!(actual_url_str, serialized_url);
    }

    #[test]
    fn test_header_map_serialization_deserialization() {
        let headers_str = r#"{"name":"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~!#$&'()*+,/:;=?@[]\""}"#;
        let headers: super::SerializableHeaderMap = serde_json::from_str(headers_str).unwrap();
        let serialized_headers = serde_json::to_string(&headers).unwrap();
        assert_eq!(headers_str, serialized_headers);
    }

    #[test]
    fn test_data_connector_context_capablities() {
        let data_connector_with_capabilities: DataConnectorLinkV1 =
            open_dds::traits::OpenDd::deserialize(serde_json::json!(
                {
                    "name": "foo",
                    "url": { "singleUrl": { "value": "http://test.com" } },
                    "schema": {
                        "version": "v0.1",
                        "capabilities": { "version": "1", "capabilities": { "query": {}, "mutation": {} }},
                        "schema": {
                            "scalar_types": {},
                            "object_types": {},
                            "collections": [],
                            "functions": [],
                            "procedures": []
                        }
                    }
                }
            ))
            .unwrap();

        let explicit_capabilities: ndc_models::CapabilitiesResponse = serde_json::from_str(
            r#" { "version": "1", "capabilities": { "query": {}, "mutation": {} } }"#,
        )
        .unwrap();

        // With explicit capabilities specified, we should use them
        assert_eq!(
            DataConnectorContext::new(&data_connector_with_capabilities)
                .unwrap()
                .inner
                .capabilities,
            &explicit_capabilities
        );
    }
}
