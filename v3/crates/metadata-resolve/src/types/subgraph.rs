use std::fmt::Display;
use std::{collections::BTreeMap, fmt::Write};

use open_dds::identifier::SubgraphName;
use open_dds::types::{BaseType, CustomTypeName, InbuiltType, TypeName, TypeReference};
use schemars::JsonSchema;
use serde::{de::DeserializeOwned, ser::SerializeMap, Deserialize, Serialize};
use serde_json;

#[derive(
    Serialize, Deserialize, JsonSchema, Clone, Debug, PartialEq, Hash, Eq, PartialOrd, Ord,
)]
pub struct Qualified<T: Display> {
    #[serde(default = "subgraph_default")]
    #[serde(skip_serializing_if = "is_subgraph_default")]
    pub subgraph: SubgraphName,
    pub name: T,
}

fn subgraph_default() -> SubgraphName {
    SubgraphName::new_inline_static("default")
}

fn is_subgraph_default(x: &SubgraphName) -> bool {
    static D: SubgraphName = SubgraphName::new_inline_static("default");
    *x == D
}

impl<T: Display> Display for Qualified<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{qualifier}")
    }
}

impl<T: Display> Qualified<T> {
    pub fn new(subgraph: SubgraphName, name: T) -> Self {
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq, JsonSchema)]
pub struct QualifiedTypeReference {
    pub underlying_type: QualifiedBaseType,
    #[serde(default = "serde_ext::ser_default::<bool>")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub nullable: bool,
}

impl Display for QualifiedTypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{qualifier}")
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

    /// Get the underlying type name by resolving Array and Nullable container types
    pub fn get_underlying_type_name(&self) -> &QualifiedTypeName {
        match &self.underlying_type {
            QualifiedBaseType::List(list_type) => list_type.get_underlying_type_name(),
            QualifiedBaseType::Named(type_name) => type_name,
        }
    }

    /// Check if this type is an array type
    pub fn is_array_type(&self) -> bool {
        matches!(self.underlying_type, QualifiedBaseType::List(_))
    }

    pub fn is_multidimensional_array_type(&self) -> bool {
        match &self.underlying_type {
            QualifiedBaseType::List(inner_type) => {
                matches!(inner_type.underlying_type, QualifiedBaseType::List(_))
            }
            QualifiedBaseType::Named(_) => false,
        }
    }
}

// should this argument be converted into an NDC expression
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ArgumentKind {
    NDCExpression,
    Other,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: QualifiedTypeReference,
    pub description: Option<String>,
    pub argument_kind: ArgumentKind,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq, JsonSchema)]
pub enum QualifiedBaseType {
    Named(QualifiedTypeName),
    List(Box<QualifiedTypeReference>),
}

impl Display for QualifiedBaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{qualifier}")
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

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, PartialOrd, Ord, Hash, Eq, JsonSchema,
)]
pub enum QualifiedTypeName {
    Inbuilt(InbuiltType),
    Custom(Qualified<CustomTypeName>),
}

impl Display for QualifiedTypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let qualifier = self.fmt_and_return_qualifier(f)?;
        write!(f, "{qualifier}")
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
                write!(f, "{inbuilt_type}")?;
                Ok(String::new()) // No qualifier for inbuilt types!
            }
            QualifiedTypeName::Custom(custom_type) => custom_type.fmt_and_return_qualifier(f),
        }
    }

    pub fn to_untagged(&self) -> UnTaggedQualifiedTypeName {
        match self {
            QualifiedTypeName::Inbuilt(inbuilt_type) => {
                UnTaggedQualifiedTypeName::Inbuilt(*inbuilt_type)
            }
            QualifiedTypeName::Custom(custom_type) => {
                UnTaggedQualifiedTypeName::Custom(custom_type.clone())
            }
        }
    }

    pub fn get_custom_type_name(&self) -> Option<&Qualified<CustomTypeName>> {
        match self {
            QualifiedTypeName::Inbuilt(_) => None,
            QualifiedTypeName::Custom(custom_type) => Some(custom_type),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Hash, Eq, JsonSchema)]
#[serde(untagged)]
pub enum UnTaggedQualifiedTypeName {
    Inbuilt(InbuiltType),
    Custom(Qualified<CustomTypeName>),
}

#[allow(dead_code)]
/// not using this now, but feel we'll need to again
pub fn serialize_optional_qualified_btreemap<T, V, S>(
    optional_map: Option<&BTreeMap<Qualified<T>, V>>,
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
    subgraph: &SubgraphName,
) -> QualifiedTypeReference {
    QualifiedTypeReference {
        nullable: type_reference.nullable,
        underlying_type: mk_qualified_base_type(&type_reference.underlying_type, subgraph),
    }
}

pub(crate) fn mk_qualified_base_type(
    base_type: &BaseType,
    subgraph: &SubgraphName,
) -> QualifiedBaseType {
    match base_type {
        BaseType::List(type_reference) => QualifiedBaseType::List(Box::new(
            mk_qualified_type_reference(type_reference, subgraph),
        )),
        BaseType::Named(type_name) => {
            QualifiedBaseType::Named(mk_qualified_type_name(type_name, subgraph))
        }
    }
}

pub(crate) fn mk_qualified_type_name(
    type_name: &TypeName,
    subgraph: &SubgraphName,
) -> QualifiedTypeName {
    match type_name {
        TypeName::Inbuilt(inbuilt_type) => QualifiedTypeName::Inbuilt(*inbuilt_type),
        TypeName::Custom(type_name) => {
            QualifiedTypeName::Custom(Qualified::new(subgraph.clone(), type_name.to_owned()))
        }
    }
}

#[test]
fn test_btree_map_serialize_deserialize() {
    use open_dds::subgraph_identifier;
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
        Qualified::new(subgraph_identifier!("subgraph"), "name".to_string()),
        "value".to_string(),
    );
    let obj_clone =
        serde_json::from_str::<ObjectForTest>(&serde_json::to_string(&obj).unwrap()).unwrap();
    assert_eq!(obj_clone, obj);
}
