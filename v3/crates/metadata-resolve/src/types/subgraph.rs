use std::collections::BTreeMap;
use std::fmt::Display;

use open_dds::types::{BaseType, CustomTypeName, InbuiltType, TypeName, TypeReference};
use serde::{de::DeserializeOwned, ser::SerializeMap, Deserialize, Serialize};
use serde_json;

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq, derive_more::Display, PartialOrd, Ord,
)]
#[display(fmt = "{name} (in subgraph {subgraph})")]
pub struct Qualified<T: Display> {
    subgraph: String,
    name: T,
}

impl<T: Display> Qualified<T> {
    pub fn new(subgraph: String, name: T) -> Self {
        Qualified { subgraph, name }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq)]
pub struct QualifiedTypeReference {
    pub underlying_type: QualifiedBaseType,
    pub nullable: bool,
}

impl Display for QualifiedTypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.underlying_type)?;
        if !self.nullable {
            write!(f, "!")?;
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: QualifiedTypeReference,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, derive_more::Display, PartialEq, Hash, Eq)]
pub enum QualifiedBaseType {
    Named(QualifiedTypeName),
    List(Box<QualifiedTypeReference>),
}

#[derive(Serialize, Deserialize, Clone, derive_more::Display, Debug, PartialEq, Hash, Eq)]
pub enum QualifiedTypeName {
    Inbuilt(InbuiltType),
    Custom(Qualified<CustomTypeName>),
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
        let qualified = serde_json::from_str(&key.to_owned()).map_err(serde::de::Error::custom)?;
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
        let qualified = serde_json::from_str(&key.to_owned()).map_err(serde::de::Error::custom)?;
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
