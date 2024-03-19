use indexmap::{IndexMap, IndexSet};
use schemars::schema::{
    ArrayValidation, ObjectValidation, Schema, SchemaObject, SubschemaValidation,
};
use schemars::schema::{InstanceType, SingleOrVec};
use serde_json;
use serde_path_to_error;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::Hash;

use crate::{impl_OpenDd_default_for, map_impl, seq_impl};

mod macros;

/// Trait for deserializing OpenDD types from JSON values and generating json schema.
/// Using serde::de::Deserialize in conjuction with serde_path_to_error would not yield
/// proper error paths for untagged and internally tagged enums. It is known limitation
/// of serde. This trait is a workaround for that limitation.
/// Use `opendds_derive::OpenDd` derive macro to implement this trait for your types.
/// See the README.md in `opendds-derive` crate for more details.
/// Refs: https://github.com/serde-rs/serde/issues/1183, https://github.com/serde-rs/serde/issues/1495
pub trait OpenDd: Sized {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError>;

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema;

    fn _schema_name() -> String;

    fn _schema_is_referenceable() -> bool {
        false
    }
}

impl_OpenDd_default_for!(String);
impl_OpenDd_default_for!(bool);
impl_OpenDd_default_for!(i32);
impl_OpenDd_default_for!(u32);
impl_OpenDd_default_for!(());

impl<T: OpenDd> OpenDd for Option<T> {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError> {
        match json {
            serde_json::Value::Null => Ok(None),
            _ => Ok(Some(T::deserialize(json)?)),
        }
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        let mut schema = gen_subschema_for::<T>(gen);
        if gen.settings().option_add_null_type {
            schema = match schema {
                Schema::Bool(true) => Schema::Bool(true),
                Schema::Bool(false) => <()>::json_schema(gen),
                Schema::Object(SchemaObject {
                    instance_type: Some(ref mut instance_type),
                    ..
                }) => {
                    add_null_type(instance_type);
                    schema
                }
                schema => SchemaObject {
                    // TODO technically the schema already accepts null, so this may be unnecessary
                    subschemas: Some(Box::new(SubschemaValidation {
                        any_of: Some(vec![schema, <()>::json_schema(gen)]),
                        ..Default::default()
                    })),
                    ..Default::default()
                }
                .into(),
            }
        }
        if gen.settings().option_nullable {
            let mut schema_obj = schema.into_object();
            schema_obj
                .extensions
                .insert("nullable".to_owned(), serde_json::json!(true));
            schema = Schema::Object(schema_obj);
        };
        schema
    }

    fn _schema_name() -> String {
        format!("Nullable_{}", T::_schema_name())
    }
}

fn add_null_type(instance_type: &mut SingleOrVec<InstanceType>) {
    match instance_type {
        SingleOrVec::Single(ty) if **ty != InstanceType::Null => {
            *instance_type = vec![**ty, InstanceType::Null].into()
        }
        SingleOrVec::Vec(ty) if !ty.contains(&InstanceType::Null) => ty.push(InstanceType::Null),
        _ => {}
    };
}

seq_impl!(Vec<T>, false);
seq_impl!(IndexSet<T: Eq + Hash>, true);
seq_impl!(HashSet<T: Eq + Hash>, true);
seq_impl!(BTreeSet<T: Ord>, true);

map_impl!(IndexMap<K: Eq + Hash, V>);
map_impl!(HashMap<K: Eq + Hash, V>);
map_impl!(BTreeMap<K: Ord, V>);

/// Generate subschema for an OpenDd object
pub fn gen_subschema_for<T: OpenDd>(
    gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema {
    struct Wrapper<T>(T);

    impl<U: OpenDd> schemars::JsonSchema for Wrapper<U> {
        fn is_referenceable() -> bool {
            U::_schema_is_referenceable()
        }

        fn schema_name() -> String {
            U::_schema_name()
        }

        fn json_schema(i_gen: &mut schemars::gen::SchemaGenerator) -> Schema {
            U::json_schema(i_gen)
        }
    }

    gen.subschema_for::<Wrapper<T>>()
}

/// Generate Root Schema for an OpenDd object
pub fn gen_root_schema_for<T: OpenDd>(
    gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::RootSchema {
    struct Wrapper<T>(T);

    impl<U: OpenDd> schemars::JsonSchema for Wrapper<U> {
        fn is_referenceable() -> bool {
            U::_schema_is_referenceable()
        }

        fn schema_name() -> String {
            U::_schema_name()
        }

        fn json_schema(i_gen: &mut schemars::gen::SchemaGenerator) -> Schema {
            U::json_schema(i_gen)
        }
    }

    let mut root_schema = gen.root_schema_for::<Wrapper<T>>();
    // Generate `$id` metadata field for subschemas
    for (schema_name, schema) in root_schema.definitions.iter_mut() {
        if let schemars::schema::Schema::Object(ref mut object) = schema {
            // Don't set $id for references, the $id should be set on the referenced schema
            if !object.is_ref() {
                let metadata = object
                    .metadata
                    .get_or_insert_with(Box::<schemars::schema::Metadata>::default);

                // Only generate an $id if an $id is not already present and if a title has been set.
                // Downstream tooling doesn't handle $id without a title well and if a schema doesn't have a title,
                // then it likely isn't significant enough to warrant an $id.
                if metadata.id.is_none() && metadata.title.is_some() {
                    metadata.id = Some(format!(
                        "https://hasura.io/jsonschemas/metadata/{schema_name}"
                    ));
                }
            }
        }
    }
    root_schema
}

/// Represents a single element in a JSON path.
#[derive(Debug)]
pub enum JSONPathElement {
    Key(String),
    Index(usize),
}

/// Represents a JSON path.
#[derive(Debug)]
pub struct JSONPath(pub Vec<JSONPathElement>);

impl Default for JSONPath {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for JSONPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elems = self
            .0
            .iter()
            .map(|element| match element {
                JSONPathElement::Key(key) => format!(".{}", key),
                JSONPathElement::Index(index) => format!("[{}]", index),
            })
            .collect::<Vec<String>>();
        let mut path = vec!["$".to_string()];
        path.extend(elems);
        write!(f, "{}", path.join(""))
    }
}

impl JSONPath {
    pub fn new() -> Self {
        JSONPath(Vec::new())
    }

    pub fn new_key(key: &str) -> Self {
        JSONPath(vec![JSONPathElement::Key(key.to_string())])
    }

    pub fn new_index(index: usize) -> Self {
        JSONPath(vec![JSONPathElement::Index(index)])
    }

    pub fn prepend_key(self, key: String) -> Self {
        let mut new_path = vec![JSONPathElement::Key(key)];
        new_path.extend(self.0);
        JSONPath(new_path)
    }

    pub fn prepend_index(self, index: usize) -> Self {
        let mut new_path = vec![JSONPathElement::Index(index)];
        new_path.extend(self.0);
        JSONPath(new_path)
    }

    pub fn from_serde_path(path: &serde_path_to_error::Path) -> Self {
        JSONPath(
            path.iter()
                .filter_map(|segment| match segment {
                    serde_path_to_error::Segment::Seq { index } => {
                        Some(JSONPathElement::Index(*index))
                    }
                    serde_path_to_error::Segment::Map { key } => {
                        Some(JSONPathElement::Key(key.to_string()))
                    }
                    serde_path_to_error::Segment::Enum { variant } => {
                        Some(JSONPathElement::Key(variant.to_string()))
                    }
                    serde_path_to_error::Segment::Unknown => None,
                })
                .collect(),
        )
    }
}

/// Error type for deserializing OpenDd types from JSON values.
#[derive(Debug, thiserror::Error)]
#[error("{error} at path {path}")]
pub struct OpenDdDeserializeError {
    #[source]
    pub error: serde_json::Error,
    pub path: JSONPath,
}

#[cfg(test)]
mod tests {
    use crate::traits;
    use opendds_derive::OpenDd;
    use pretty_assertions::assert_eq;
    use serde_json;

    #[test]
    fn test_parse_versioned_enum() {
        #[derive(Debug, PartialEq, OpenDd)]
        struct MyStruct {
            name: String,
            age: i32,
        }

        #[derive(Debug, PartialEq, OpenDd)]
        #[opendd(as_versioned_with_definition)]
        enum MyEnum {
            V1(MyStruct),
        }
        let json = serde_json::json!({
            "version": "v1",
            "definition": {
                "name": "Foo",
                "age": 25
            }
        });
        let expected = MyEnum::V1(MyStruct {
            name: "Foo".to_string(),
            age: 25,
        });
        assert_eq!(expected, traits::OpenDd::deserialize(json).unwrap())
    }

    #[test]
    fn test_parse_kind_enum() {
        #[derive(Debug, PartialEq, OpenDd)]
        struct MyStruct {
            name: String,
            age: i32,
        }

        #[derive(Debug, PartialEq, OpenDd)]
        #[opendd(as_kind)]
        enum MyEnum {
            MyVariant(MyStruct),
        }
        let json = serde_json::json!({
            "kind": "MyVariant",
            "name": "Foo",
            "age": 25
        });
        let expected = MyEnum::MyVariant(MyStruct {
            name: "Foo".to_string(),
            age: 25,
        });
        assert_eq!(expected, traits::OpenDd::deserialize(json).unwrap())
    }

    #[test]
    fn test_enum_unexpected_variant() {
        #[derive(Debug, PartialEq, OpenDd)]
        struct MyStruct {
            name: String,
            age: i32,
        }

        #[derive(Debug, PartialEq, OpenDd)]
        #[opendd(as_kind)]
        enum MyEnum {
            MyVariant(MyStruct),
        }
        let json = serde_json::json!({
            "kind": "MyStruct",
            "name": "Foo",
            "age": 25
        });
        assert_eq!(
            "unknown variant `MyStruct`, expected `MyVariant`".to_string(),
            <MyEnum as traits::OpenDd>::deserialize(json)
                .unwrap_err()
                .error
                .to_string()
        )
    }

    #[test]
    fn test_enum_error_path() {
        #[derive(Debug, PartialEq, OpenDd)]
        struct MyStruct {
            name: String,
            age: i32,
        }

        #[derive(Debug, PartialEq, OpenDd)]
        #[opendd(as_versioned_with_definition)]
        enum MyEnum {
            V1(MyStruct),
        }
        let json = serde_json::json!({
            "version": "v1",
            "definition": {
                "name_": "Foo",
                "age": 25
            }
        });
        assert_eq!(
            "$.definition".to_string(),
            <MyEnum as traits::OpenDd>::deserialize(json)
                .unwrap_err()
                .path
                .to_string()
        )
    }

    // Untagged enum deserialize tests

    use strum_macros::EnumVariantNames;

    #[derive(Debug, PartialEq, OpenDd)]
    #[opendd(untagged_with_kind)]
    enum MyEnumUntagged {
        One(KindEnumOne),
        Two(KindEnumTwo),
    }

    #[derive(Debug, PartialEq, EnumVariantNames, OpenDd)]
    #[opendd(as_kind)]
    enum KindEnumOne {
        First(FirstStruct),
        Second(SecondStruct),
    }

    #[derive(Debug, PartialEq, EnumVariantNames, OpenDd)]
    #[opendd(as_kind)]
    enum KindEnumTwo {
        Third(ThirdStruct),
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct FirstStruct {
        first: String,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct SecondStruct {
        first: String,
        second: String,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct ThirdStruct {
        first: String,
        second: String,
        third: String,
    }

    #[test]
    fn test_parse_untagged_enum() {
        let json = serde_json::json!({
            "kind": "Second",
            "first": "First",
            "second": "Second"
        });
        let expected = MyEnumUntagged::One(KindEnumOne::Second(SecondStruct {
            first: "First".to_string(),
            second: "Second".to_string(),
        }));
        assert_eq!(expected, traits::OpenDd::deserialize(json).unwrap())
    }

    #[test]
    fn test_parse_untagged_enum_error() {
        let json = serde_json::json!({
            "kind": "Third",
            "first": "First",
            "second": "Second"
        });
        let err = <MyEnumUntagged as traits::OpenDd>::deserialize(json).unwrap_err();
        assert_eq!("missing field `third`", err.error.to_string());
    }

    #[test]
    fn test_parse_untagged_enum_error_unknown_variant() {
        let json = serde_json::json!({
            "kind": "Random",
            "first": "First",
            "second": "Second"
        });
        let err = <MyEnumUntagged as traits::OpenDd>::deserialize(json).unwrap_err();
        assert_eq!(
            "unexpected value: `Random` expecting First, Second, Third",
            err.error.to_string()
        );
        assert_eq!("$.kind", err.path.to_string());
    }

    // Types required to test struct deserializing

    #[derive(Debug, PartialEq, OpenDd)]
    struct Metadata {
        supergraph: Option<SuperGraph>,
        subgraphs: Vec<SubGraph>,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    #[opendd(as_versioned_with_definition)]
    enum SuperGraph {
        V1(SuperGraphV1),
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct SuperGraphV1 {
        super_graph_name: String,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    #[opendd(as_kind)]
    enum SubGraph {
        KindOne(SubGraphKindOne),
        KindTwo(SubGraphKindTwo),
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct SubGraphKindOne {
        sub_graph_one_name: String,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct SubGraphKindTwo {
        sub_graph_two_name: Option<String>,
    }

    #[test]
    fn test_parse_struct() {
        let json = serde_json::json!({
            "supergraph": {
                "version": "v1",
                "definition": {
                    "superGraphName": "Foo"
                }
            },
            "subgraphs": [
                {
                    "kind": "KindOne",
                    "subGraphOneName": "Bar"
                },
                {
                    "kind": "KindTwo",
                    "subGraphTwoName": "Baz"
                },
                {
                    "kind": "KindTwo"
                }
            ]
        });

        let expected = Metadata {
            supergraph: Some(SuperGraph::V1(SuperGraphV1 {
                super_graph_name: "Foo".to_string(),
            })),
            subgraphs: vec![
                SubGraph::KindOne(SubGraphKindOne {
                    sub_graph_one_name: "Bar".to_string(),
                }),
                SubGraph::KindTwo(SubGraphKindTwo {
                    sub_graph_two_name: Some("Baz".to_string()),
                }),
                SubGraph::KindTwo(SubGraphKindTwo {
                    sub_graph_two_name: None,
                }),
            ],
        };

        assert_eq!(expected, traits::OpenDd::deserialize(json).unwrap())
    }

    #[test]
    fn test_struct_missing_field_error() {
        let json = serde_json::json!({
            "supergraph": {
                "version": "v1",
                "definition": {} // Missing "superGraphName" field here
            },
            "subgraphs": [
                {
                    "kind": "KindOne",
                    "subGraphOneName": "Bar"
                },
                {
                    "kind": "KindTwo",
                    "subGraphTwoName": "Baz"
                },
                {
                    "kind": "KindTwo"
                }
            ]
        });
        assert_eq!(
            "missing field `superGraphName`",
            <Metadata as traits::OpenDd>::deserialize(json)
                .unwrap_err()
                .error
                .to_string()
        )
    }

    #[test]
    fn test_struct_parse_error_path() {
        let json = serde_json::json!({
            "supergraph": {
                "version": "v1",
                "definition": {
                    "superGraphName": "Foo"
                }
            },
            "subgraphs": [
                {
                    "kind": "KindOne",
                    "subGraphOneName": "Bar"
                },
                {
                    "kind": "KindTwo_", // Path should track here
                    "subGraphTwoName": "Baz"
                },
                {
                    "kind": "KindTwo"
                }
            ]
        });

        assert_eq!(
            "$.subgraphs[1].kind",
            <Metadata as traits::OpenDd>::deserialize(json)
                .unwrap_err()
                .path
                .to_string()
        )
    }

    #[test]
    fn test_unknown_fields_struct_error() {
        let json = serde_json::json!({
            "subGraphOneName": "Random",
            "unknownFieldOne": "Unknown",
            "unknownFieldTwo": "Unknown"
        });

        assert_eq!(
            "unexpected keys: unknownFieldTwo, unknownFieldOne; expecting: subGraphOneName",
            <SubGraphKindOne as traits::OpenDd>::deserialize(json)
                .unwrap_err()
                .error
                .to_string(),
        )
    }

    #[test]
    fn test_json_schema_structs() {
        #[derive(PartialEq, opendds_derive::OpenDd)]
        #[opendd(json_schema(title = "MyString", example = "MyString::_example"))]
        /// Some struct with a string
        struct MyString {
            some_value: String,
        }

        impl MyString {
            fn _example() -> serde_json::Value {
                serde_json::json!(
                    {
                        "someValue": "Random Value"
                    }
                )
            }
        }

        #[derive(PartialEq, opendds_derive::OpenDd)]
        /// Some struct with a struct and an int
        struct MyStruct {
            /// My String
            my_string: MyString,
            #[opendd(default)]
            my_int: i32,
        }

        let mut gen = schemars::gen::SchemaGenerator::default();
        let root_schema = traits::gen_root_schema_for::<MyStruct>(&mut gen);
        let exp = serde_json::json!(
            {
                "$schema": "http://json-schema.org/draft-07/schema#",
                "$id": "https://hasura.io/jsonschemas/metadata/MyStruct",
                "title": "MyStruct",
                "description": "Some struct with a struct and an int",
                "type": "object",
                "required": [
                    "myString"
                ],
                "properties": {
                    "myString": {
                        "description": "My String",
                        "allOf": [
                            {
                                "$ref": "#/definitions/MyString"
                            }
                        ]
                    },
                    "myInt": {
                        "default": 0,
                        "type": "integer",
                        "format": "int32"
                    }
                },
                "additionalProperties": false,
                "definitions": {
                    "MyString": {
                        "$id": "https://hasura.io/jsonschemas/metadata/MyString",
                        "title": "MyString",
                        "description": "Some struct with a string",
                        "examples": [
                            {
                                "someValue": "Random Value"
                            }
                        ],
                        "type": "object",
                        "required": [
                            "someValue"
                        ],
                        "properties": {
                            "someValue": {
                                "type": "string"
                            }
                        },
                        "additionalProperties": false
                    }
                }
            }
        );

        assert_eq!(
            serde_json::to_string_pretty(&exp).unwrap(),
            serde_json::to_string_pretty(&root_schema).unwrap()
        );
    }
}
