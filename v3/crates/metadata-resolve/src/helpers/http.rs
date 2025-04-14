//! Wrapper over HTTP types which can serialized/deserialized

use indexmap::IndexMap;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use serde::{Deserialize, Serialize};
use serde::{
    de::Error as DeError,
    ser::{Error as SerError, SerializeMap},
};
use std::{collections::BTreeMap, fmt::Display, str::FromStr};

#[derive(Debug)]
pub enum HeaderError {
    InvalidHeaderName { header_name: String },
    InvalidHeaderValue { header_name: String },
}

impl Display for HeaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeaderError::InvalidHeaderName { header_name } => {
                write!(f, "invalid HTTP header name: {header_name}")
            }
            HeaderError::InvalidHeaderValue { header_name } => {
                write!(f, "invalid HTTP header value: {header_name}")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SerializableUrl(pub reqwest::Url);

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SerializableHeaderName(pub HeaderName);

impl SerializableHeaderName {
    pub fn new(header_name_str: String) -> Result<Self, HeaderError> {
        let header_name =
            HeaderName::from_str(&header_name_str).map_err(|_| HeaderError::InvalidHeaderName {
                header_name: header_name_str,
            })?;
        Ok(SerializableHeaderName(header_name))
    }
}

impl Serialize for SerializableHeaderName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.0.as_str())
    }
}

impl<'de> Deserialize<'de> for SerializableHeaderName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let header_str = String::deserialize(deserializer)?;
        SerializableHeaderName::new(header_str).map_err(D::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SerializableHeaderMap(pub HeaderMap);

impl SerializableHeaderMap {
    pub fn new(headers: &IndexMap<String, String>) -> Result<Self, HeaderError> {
        let header_map = headers
            .iter()
            .map(|(k, v)| {
                Ok((
                    HeaderName::from_str(k).map_err(|_| HeaderError::InvalidHeaderName {
                        header_name: k.clone(),
                    })?,
                    HeaderValue::from_str(v).map_err(|_| HeaderError::InvalidHeaderValue {
                        header_name: k.clone(),
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
        let hash_map = BTreeMap::<String, String>::deserialize(deserializer)?;
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

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_header_name_serialization_deserialization() {
        let headers_str = json!([
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ-abcdefghijklmnopqrstuvwxyz0123456789_#.~!$&'*+",
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ-abcdefghijklmnopqrstuvwxyz0123456789_#.~!$&'*+",
        ]);
        let headers: Vec<SerializableHeaderName> = serde_json::from_value(headers_str).unwrap();
        let serialized_headers = serde_json::to_string(&headers).unwrap();
        let deserialized_headers: Vec<SerializableHeaderName> =
            serde_json::from_str(&serialized_headers).unwrap();
        assert_eq!(deserialized_headers, headers);
    }

    #[test]
    fn test_header_map_serialization_deserialization() {
        let headers_str = r#"{"some_header_name":"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~!#$&'()*+,/:;=?@[]\""}"#;
        let headers: SerializableHeaderMap = serde_json::from_str(headers_str).unwrap();
        let serialized_headers = serde_json::to_string(&headers).unwrap();
        assert_eq!(headers_str, serialized_headers);
    }
}
