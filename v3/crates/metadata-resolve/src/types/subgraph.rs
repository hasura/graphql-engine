use std::fmt::Display;
use std::{collections::BTreeMap, fmt::Write};

use open_dds::types::{BaseType, CustomTypeName, InbuiltType, TypeName, TypeReference};
use serde::{de::DeserializeOwned, ser::SerializeMap, Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub struct Qualified<T: Display> {
    pub subgraph: String,
    pub name: T,
}

impl<T: Display> Display for Qualified<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{}", qualifier)
    }
}

impl<T: Display> Qualified<T> {
    pub fn new(subgraph: String, name: T) -> Self {
        Qualified { subgraph, name }
    }

    /// Write the display string to the formatter, but return the qualifier string
    /// separately so it can be appended later
    fn fmt_and_return_qualifier(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<String, std::fmt::Error> {
        write!(f, "{}", self.name)?;
        Ok(format!(" (in subgraph {})", self.subgraph))
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq)]
pub struct QualifiedTypeReference {
    pub underlying_type: QualifiedBaseType,
    pub nullable: bool,
}

impl Display for QualifiedTypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{}", qualifier)
    }
}

impl QualifiedTypeReference {
    /// Write the display string to the formatter, but return the qualifier string
    /// separately so it can be appended later
    fn fmt_and_return_qualifier(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<String, std::fmt::Error> {
        let qualifier = self.underlying_type.fmt_and_return_qualifier(f)?;
        if !self.nullable {
            write!(f, "!")?;
        }
        Ok(qualifier)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: QualifiedTypeReference,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq)]
pub enum QualifiedBaseType {
    Named(QualifiedTypeName),
    List(Box<QualifiedTypeReference>),
}

impl Display for QualifiedBaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{}", qualifier)
    }
}

impl QualifiedBaseType {
    /// Write the display string to the formatter, but return the qualifier string
    /// separately so it can be appended later
    fn fmt_and_return_qualifier(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<String, std::fmt::Error> {
        match self {
            QualifiedBaseType::Named(named_type) => named_type.fmt_and_return_qualifier(f),
            QualifiedBaseType::List(element_type) => {
                f.write_char('[')?;
                let qualifier = element_type.fmt_and_return_qualifier(f)?;
                f.write_char(']')?;
                Ok(qualifier)
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq)]
pub enum QualifiedTypeName {
    Inbuilt(InbuiltType),
    Custom(Qualified<CustomTypeName>),
}

impl Display for QualifiedTypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{}", qualifier)
    }
}

impl QualifiedTypeName {
    /// Write the display string to the formatter, but return the qualifier string
    /// separately so it can be appended later
    fn fmt_and_return_qualifier(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<String, std::fmt::Error> {
        match self {
            QualifiedTypeName::Inbuilt(inbuilt_type) => {
                write!(f, "{}", inbuilt_type)?;
                Ok(String::new()) // No qualifier for inbuilt types!
            }
            QualifiedTypeName::Custom(custom_type) => custom_type.fmt_and_return_qualifier(f),
        }
    }
}

#[allow(dead_code)]
/// not using this now, but feel we'll need to again
pub fn serialize_optional_qualified_btreemap<T, V, S>(
    optional_map: &Option<BTreeMap<Qualified<T>, V>>,
    s: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    V: Serialize,
    T: Display + Serialize,
{
    match optional_map {
        Some(map) => serialize_qualified_btreemap(map, s),
        None => s.serialize_none(),
    }
}

pub fn serialize_qualified_btreemap<T, V, S>(
    map: &BTreeMap<Qualified<T>, V>,
    s: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    V: Serialize,
    T: Display + Serialize,
{
    let mut obj = s.serialize_map(Some(map.len()))?;
    for (name, value) in map {
        let stringified_key: String =
            serde_json::to_string(name).map_err(serde::ser::Error::custom)?;
        obj.serialize_entry(&stringified_key, value)?;
    }
    obj.end()
}

pub fn deserialize_qualified_btreemap<'de, D, T, V>(
    deserializer: D,
) -> Result<BTreeMap<Qualified<T>, V>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Display + Ord + DeserializeOwned,
    V: Deserialize<'de>,
{
    let map: BTreeMap<String, V> = Deserialize::deserialize(deserializer)?;
    let mut result = BTreeMap::new();
    for (key, value) in map {
        let qualified = serde_json::from_str(&key).map_err(serde::de::Error::custom)?;
        result.insert(qualified, value);
    }
    Ok(result)
}

pub fn serialize_non_string_key_btreemap<K, V, S>(
    map: &BTreeMap<K, V>,
    s: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    V: Serialize,
    K: Serialize,
{
    let mut obj = s.serialize_map(Some(map.len()))?;
    for (name, value) in map {
        let stringified_key: String =
            serde_json::to_string(name).map_err(serde::ser::Error::custom)?;
        obj.serialize_entry(&stringified_key, value)?;
    }
    obj.end()
}

pub fn deserialize_non_string_key_btreemap<'de, D, K, V>(
    deserializer: D,
) -> Result<BTreeMap<K, V>, D::Error>
where
    D: serde::Deserializer<'de>,
    K: Ord + DeserializeOwned,
    V: Deserialize<'de>,
{
    let map: BTreeMap<String, V> = Deserialize::deserialize(deserializer)?;
    let mut result = BTreeMap::new();
    for (key, value) in map {
        let qualified = serde_json::from_str(&key).map_err(serde::de::Error::custom)?;
        result.insert(qualified, value);
    }
    Ok(result)
}

pub(crate) fn mk_qualified_type_reference(
    type_reference: &TypeReference,
    subgraph: &str,
) -> QualifiedTypeReference {
    QualifiedTypeReference {
        nullable: type_reference.nullable,
        underlying_type: mk_qualified_base_type(&type_reference.underlying_type, subgraph),
    }
}

pub(crate) fn mk_qualified_base_type(base_type: &BaseType, subgraph: &str) -> QualifiedBaseType {
    match base_type {
        BaseType::List(type_reference) => QualifiedBaseType::List(Box::new(
            mk_qualified_type_reference(type_reference, subgraph),
        )),
        BaseType::Named(type_name) => {
            QualifiedBaseType::Named(mk_qualified_type_name(type_name, subgraph))
        }
    }
}

pub(crate) fn mk_qualified_type_name(type_name: &TypeName, subgraph: &str) -> QualifiedTypeName {
    match type_name {
        TypeName::Inbuilt(inbuilt_type) => QualifiedTypeName::Inbuilt(inbuilt_type.clone()),
        TypeName::Custom(type_name) => {
            QualifiedTypeName::Custom(Qualified::new(subgraph.to_string(), type_name.to_owned()))
        }
    }
}

#[test]
fn test_btree_map_serialize_deserialize() {
    #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
    struct ObjectForTest {
        #[serde(
            serialize_with = "serialize_qualified_btreemap",
            deserialize_with = "deserialize_qualified_btreemap"
        )]
        map: BTreeMap<Qualified<String>, String>,
    }

    let mut obj = ObjectForTest {
        map: BTreeMap::new(),
    };
    obj.map.insert(
        Qualified::new("subgraph".to_string(), "name".to_string()),
        "value".to_string(),
    );
    let obj_clone =
        serde_json::from_str::<ObjectForTest>(&serde_json::to_string(&obj).unwrap()).unwrap();
    assert_eq!(obj_clone, obj);
}
