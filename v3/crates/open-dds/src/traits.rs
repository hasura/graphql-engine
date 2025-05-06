use indexmap::{IndexMap, IndexSet};
use schemars::schema::{
    ArrayValidation, ObjectValidation, Schema, SchemaObject, SubschemaValidation,
};
use schemars::schema::{InstanceType, SingleOrVec};
use serde_json;
use smol_str::SmolStr;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::Hash;

use crate::{impl_OpenDd_default_for, map_impl, seq_impl};
use jsonschema_tidying::deduplicate_definitions;

mod macros;

/// Trait for deserializing OpenDD types from JSON values and generating json schema.
/// Using serde::de::Deserialize in conjuction with serde_path_to_error would not yield
/// proper error paths for untagged and internally tagged enums. It is known limitation
/// of serde. This trait is a workaround for that limitation.
/// Use `opendds_derive::OpenDd` derive macro to implement this trait for your types.
/// See the README.md in `opendds-derive` crate for more details.
/// Refs: <https://github.com/serde-rs/serde/issues/1183>, <https://github.com/serde-rs/serde/issues/1495>
pub trait OpenDd: Sized {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError>;

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema;

    fn _schema_name() -> String;

    fn _schema_is_referenceable() -> bool {
        false
    }
}

pub fn deserialize_key<T: OpenDd>(
    json: serde_json::Value,
    path: jsonpath::JSONPath,
    key: String,
) -> Result<T, OpenDdDeserializeError> {
    OpenDd::deserialize(json, path.append_key(key.clone())).map_err(|e| OpenDdDeserializeError {
        error: e.error,
        path: e.path.prepend_key(key),
    })
}

pub fn deserialize_index<T: OpenDd>(
    json: serde_json::Value,
    path: jsonpath::JSONPath,
    index: usize,
) -> Result<T, OpenDdDeserializeError> {
    OpenDd::deserialize(json, path.append_index(index)).map_err(|e| OpenDdDeserializeError {
        error: e.error,
        path: e.path.prepend_index(index),
    })
}

impl_OpenDd_default_for!(String);
impl_OpenDd_default_for!(SmolStr);
impl_OpenDd_default_for!(bool);
impl_OpenDd_default_for!(i32);
impl_OpenDd_default_for!(u32);
impl_OpenDd_default_for!(u64);
impl_OpenDd_default_for!(());
impl_OpenDd_default_for!(serde_json::Value);

impl<T: OpenDd> OpenDd for Box<T> {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        T::deserialize(json, path).map(Box::new)
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        T::json_schema(generator)
    }

    fn _schema_name() -> String {
        T::_schema_name()
    }

    fn _schema_is_referenceable() -> bool {
        T::_schema_is_referenceable()
    }
}

impl<T> OpenDd for Cow<'static, T>
where
    T: ToOwned + ?Sized,
    T::Owned: OpenDd,
    // Concrete example: T is str and T::Owned is String - so we deserialize a String and store it as an Owned str in the Cow
{
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        <<T as ToOwned>::Owned>::deserialize(json, path).map(Cow::Owned)
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        String::json_schema(generator)
    }

    fn _schema_name() -> String {
        String::_schema_name()
    }
}

impl<T: OpenDd> OpenDd for Option<T> {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        match json {
            serde_json::Value::Null => Ok(None),
            _ => Ok(Some(T::deserialize(json, path)?)),
        }
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        let mut schema = gen_subschema_for::<T>(generator);
        if generator.settings().option_add_null_type {
            schema = match schema {
                Schema::Bool(true) => Schema::Bool(true),
                Schema::Bool(false) => <()>::json_schema(generator),
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
                        any_of: Some(vec![schema, <()>::json_schema(generator)]),
                        ..Default::default()
                    })),
                    ..Default::default()
                }
                .into(),
            }
        }
        if generator.settings().option_nullable {
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
            *instance_type = vec![**ty, InstanceType::Null].into();
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
    generator: &mut schemars::r#gen::SchemaGenerator,
) -> schemars::schema::Schema {
    struct Wrapper<T>(T);

    impl<U: OpenDd> schemars::JsonSchema for Wrapper<U> {
        fn is_referenceable() -> bool {
            U::_schema_is_referenceable()
        }

        fn schema_name() -> String {
            U::_schema_name()
        }

        fn json_schema(i_gen: &mut schemars::r#gen::SchemaGenerator) -> Schema {
            U::json_schema(i_gen)
        }
    }

    generator.subschema_for::<Wrapper<T>>()
}

/// Generate Root Schema for an OpenDd object
pub fn gen_root_schema_for<T: OpenDd>(
    generator: &mut schemars::r#gen::SchemaGenerator,
) -> schemars::schema::RootSchema {
    struct Wrapper<T>(T);

    impl<U: OpenDd> schemars::JsonSchema for Wrapper<U> {
        fn is_referenceable() -> bool {
            U::_schema_is_referenceable()
        }

        fn schema_name() -> String {
            U::_schema_name()
        }

        fn json_schema(i_gen: &mut schemars::r#gen::SchemaGenerator) -> Schema {
            U::json_schema(i_gen)
        }
    }

    let mut root_schema = generator.root_schema_for::<Wrapper<T>>();
    // Generate `$id` metadata field for subschemas
    for (schema_name, schema) in &mut root_schema.definitions {
        if let schemars::schema::Schema::Object(object) = schema {
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

    deduplicate_definitions(&mut root_schema);
    root_schema
}

/// Error type for deserializing OpenDd types from JSON values.
#[derive(Debug, thiserror::Error)]
#[error("{error} at path {path}")]
pub struct OpenDdDeserializeError {
    #[source]
    pub error: serde_json::Error,
    pub path: jsonpath::JSONPath,
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
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
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
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
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
            <MyEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err()
                .error
                .to_string()
        );
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
            <MyEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err()
                .path
                .to_string()
        );
    }

    // Untagged enum deserialize tests

    use strum_macros::VariantNames;

    #[derive(Debug, PartialEq, OpenDd)]
    #[opendd(untagged_with_kind)]
    enum MyEnumUntagged {
        One(KindEnumOne),
        Two(KindEnumTwo),
    }

    #[derive(Debug, PartialEq, VariantNames, OpenDd)]
    #[opendd(as_kind)]
    enum KindEnumOne {
        First(FirstStruct),
        Second(SecondStruct),
    }

    #[derive(Debug, PartialEq, VariantNames, OpenDd)]
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
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
    }

    #[test]
    fn test_parse_untagged_enum_error() {
        let json = serde_json::json!({
            "kind": "Third",
            "first": "First",
            "second": "Second"
        });
        let err = <MyEnumUntagged as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
            .unwrap_err();
        assert_eq!("missing field `third`", err.error.to_string());
    }

    #[test]
    fn test_parse_untagged_enum_error_unknown_variant() {
        let json = serde_json::json!({
            "kind": "Random",
            "first": "First",
            "second": "Second"
        });
        let err = <MyEnumUntagged as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
            .unwrap_err();
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

        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
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
            <Metadata as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err()
                .error
                .to_string()
        );
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
            <Metadata as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err()
                .path
                .to_string()
        );
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
            <SubGraphKindOne as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err()
                .error
                .to_string(),
        );
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

        let mut generator = schemars::r#gen::SchemaGenerator::default();
        let root_schema = traits::gen_root_schema_for::<MyStruct>(&mut generator);
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

    #[test]
    fn test_json_schema_enum_with_hidden_item() {
        #[derive(PartialEq, opendds_derive::OpenDd)]
        #[opendd(json_schema(title = "Dog"))]
        // a dog
        struct Dog {
            name: String,
        }

        #[derive(PartialEq, opendds_derive::OpenDd)]
        #[opendd(json_schema(title = "Cat"))]
        // a cat
        struct Cat {
            name: String,
        }

        #[derive(PartialEq, opendds_derive::OpenDd)]
        #[opendd(json_schema(title = "Horse"))]
        // a horse
        struct Horse {
            name: String,
        }

        #[derive(PartialEq, opendds_derive::OpenDd)]
        #[opendd(as_kind, json_schema(title = "Animal"))]
        /// The name of some kind of animal.
        enum Animal {
            Dog(Dog),
            Cat(Cat),
            #[opendd(hidden = true)]
            Horse(Horse),
        }

        let mut generator = schemars::r#gen::SchemaGenerator::default();
        let root_schema = traits::gen_root_schema_for::<Animal>(&mut generator);
        let exp = serde_json::json!(
            {
                "$schema": "http://json-schema.org/draft-07/schema#",
                "$id": "https://hasura.io/jsonschemas/metadata/Animal",
                "title": "Animal",
                "description": "The name of some kind of animal.",
                "oneOf": [
                    {
                        "$id": "https://hasura.io/jsonschemas/metadata/Dog",
                        "title": "Dog",
                        "type": "object",
                        "required": [
                            "kind",
                            "name"
                        ],
                        "properties": {
                            "kind": {
                                "type": "string",
                                "enum": [
                                    "Dog"
                                ]
                            },
                            "name": {
                                "type":"string"
                            }
                        },
                        "additionalProperties": false
                    },
                    {
                        "$id": "https://hasura.io/jsonschemas/metadata/Cat",
                        "title": "Cat",
                        "type": "object",
                        "required": [
                            "kind",
                            "name"
                        ],
                        "properties": {
                            "kind": {
                                "type": "string",
                                "enum": [
                                    "Cat"
                                ]
                            },
                            "name": {
                                "type":"string"
                            }
                        },
                        "additionalProperties": false
                    }
                ]
            }
        );

        assert_eq!(
            serde_json::to_string_pretty(&exp).unwrap(),
            serde_json::to_string_pretty(&root_schema).unwrap()
        );
    }

    // Tests for externally tagged enums

    #[derive(Debug, PartialEq, OpenDd)]
    #[opendd(externally_tagged)]
    #[allow(clippy::enum_variant_names)]
    /// An externally tagged enum
    enum ExternallyTaggedEnum {
        #[opendd(json_schema(title = "VariantOne"))]
        /// The first variant
        VariantOne(VariantOneStruct),
        #[opendd(json_schema(title = "VariantTwo"))]
        /// The second variant
        VariantTwo(VariantTwoStruct),
        #[opendd(hidden = true)]
        VariantThree(VariantThreeStruct),
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct VariantOneStruct {
        prop_a: String,
        prop_b: i32,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct VariantTwoStruct {
        prop_1: bool,
        prop_2: String,
    }

    #[derive(Debug, PartialEq, OpenDd)]
    struct VariantThreeStruct {
        prop_x: String,
        prop_y: String,
    }

    #[test]
    fn test_externally_tagged_enum() {
        let json = serde_json::json!({
            "variantTwo": {
                "prop1": true,
                "prop2": "testing"
            }
        });
        let expected = ExternallyTaggedEnum::VariantTwo(VariantTwoStruct {
            prop_1: true,
            prop_2: "testing".to_owned(),
        });
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
    }

    #[test]
    fn test_externally_tagged_enum_deserializes_hidden_variants() {
        let json = serde_json::json!({
            "variantThree": {
                "propX": "testing",
                "propY": "abcd",
            }
        });
        let expected = ExternallyTaggedEnum::VariantThree(VariantThreeStruct {
            prop_x: "testing".to_owned(),
            prop_y: "abcd".to_owned(),
        });
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
    }

    #[test]
    fn test_externally_tagged_enum_multiple_properties_error() {
        let json = serde_json::json!({
            "variantOne": {
                "propA": "test",
                "propB": 123,
            },
            "variantTwo": {
                "prop1": true,
                "prop2": "testing"
            }
        });
        let err =
            <ExternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "invalid type: found multiple object properties, expected object with only one of the following properties: variantOne, variantTwo".to_string(),
            err
                .error
                .to_string()
        );
        assert_eq!("$", err.path.to_string());
    }

    #[test]
    fn test_externally_tagged_enum_empty_object_error() {
        let json = serde_json::json!({});
        let err =
            <ExternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "invalid type: found empty object, expected object with only one of the following properties: variantOne, variantTwo".to_string(),
            err.error.to_string()
        );
        assert_eq!("$", err.path.to_string());
    }

    #[test]
    fn test_externally_tagged_enum_content_not_object_error() {
        let json = serde_json::json!({
            "variantOne": "wrong"
        });
        let err =
            <ExternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "invalid type: not an object, expected object".to_string(),
            err.error.to_string()
        );
        assert_eq!("$.variantOne", err.path.to_string());
    }

    #[test]
    fn test_externally_tagged_enum_unexpected_variant() {
        let json = serde_json::json!({
            "variantUnknown": {
                "propA": "test",
                "propB": 123,
            }
        });
        let err =
            <ExternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "unknown variant `variantUnknown`, expected `variantOne, variantTwo`".to_string(),
            err.error.to_string()
        );
        assert_eq!("$", err.path.to_string());
    }

    #[test]
    fn test_externally_tagged_nested_error() {
        let json = serde_json::json!({
            "variantTwo": {
                "prop1": "wrong type",
                "prop2": "testing"
            }
        });
        let err =
            <ExternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "invalid type: string \"wrong type\", expected a boolean".to_string(),
            err.error.to_string()
        );
        assert_eq!("$.variantTwo.prop1", err.path.to_string());
    }

    #[test]
    fn test_externally_tagged_enum_json_schema() {
        let mut generator = schemars::r#gen::SchemaGenerator::default();
        let root_schema = traits::gen_root_schema_for::<ExternallyTaggedEnum>(&mut generator);
        let exp = serde_json::json!(
            {
                "$schema": "http://json-schema.org/draft-07/schema#",
                "$id": "https://hasura.io/jsonschemas/metadata/ExternallyTaggedEnum",
                "title": "ExternallyTaggedEnum",
                "description": "An externally tagged enum",
                "oneOf": [
                    {
                        "title": "VariantOne",
                        "description": "The first variant",
                        "type": "object",
                        "required": [
                            "variantOne"
                        ],
                        "properties": {
                            "variantOne": {
                                "$ref": "#/definitions/VariantOneStruct"
                            }
                        },
                        "additionalProperties": false
                    },
                    {
                        "title": "VariantTwo",
                        "description": "The second variant",
                        "type": "object",
                        "required": [
                            "variantTwo"
                        ],
                        "properties": {
                            "variantTwo": {
                                "$ref": "#/definitions/VariantTwoStruct"
                            }
                        },
                        "additionalProperties": false
                    }
                ],
                "definitions": {
                    "VariantOneStruct": {
                        "$id": "https://hasura.io/jsonschemas/metadata/VariantOneStruct",
                        "title": "VariantOneStruct",
                        "type": "object",
                        "required": [
                            "propA",
                            "propB"
                        ],
                        "properties": {
                            "propA": {
                                "type": "string"
                            },
                            "propB": {
                                "type": "integer",
                                "format": "int32"
                            }
                        },
                        "additionalProperties": false
                    },
                    "VariantTwoStruct": {
                        "$id": "https://hasura.io/jsonschemas/metadata/VariantTwoStruct",
                        "title": "VariantTwoStruct",
                        "type": "object",
                        "required": [
                            "prop1",
                            "prop2"
                        ],
                        "properties": {
                            "prop1": {
                                "type": "boolean"
                            },
                            "prop2": {
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

    // Tests for internally tagged enums

    #[derive(Debug, PartialEq, OpenDd)]
    #[opendd(internally_tagged(tag = "custom_tag"))]
    #[allow(clippy::enum_variant_names)]
    /// An internally tagged enum
    enum InternallyTaggedEnum {
        #[opendd(json_schema(title = "VariantOne"))]
        /// The first variant
        VariantOne(VariantOneStruct),
        #[opendd(rename = "variant_2", json_schema(title = "VariantTwo"))]
        /// The second variant
        VariantTwo(VariantTwoStruct),
        #[opendd(hidden = true)]
        VariantThree(VariantThreeStruct),
    }

    #[test]
    fn test_internally_tagged_enum() {
        let json = serde_json::json!({
            "custom_tag": "variant_2",
            "prop1": true,
            "prop2": "testing"
        });
        let expected = InternallyTaggedEnum::VariantTwo(VariantTwoStruct {
            prop_1: true,
            prop_2: "testing".to_owned(),
        });
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
    }

    #[test]
    fn test_internally_tagged_enum_deserializes_hidden_variants() {
        let json = serde_json::json!({
            "custom_tag": "variantThree",
            "propX": "testing",
            "propY": "abcd",
        });
        let expected = InternallyTaggedEnum::VariantThree(VariantThreeStruct {
            prop_x: "testing".to_owned(),
            prop_y: "abcd".to_owned(),
        });
        assert_eq!(
            expected,
            traits::OpenDd::deserialize(json, jsonpath::JSONPath::new()).unwrap()
        );
    }

    #[test]
    fn test_internally_tagged_enum_empty_object_error() {
        let json = serde_json::json!({});
        let err =
            <InternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "missing field `custom_tag`".to_string(),
            err.error.to_string()
        );
        assert_eq!("$", err.path.to_string());
    }

    #[test]
    fn test_internally_tagged_enum_unexpected_variant() {
        let json = serde_json::json!({
            "custom_tag": "variantUnknown",
            "propA": "test",
            "propB": 123,
        });
        let err =
            <InternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "unknown variant `variantUnknown`, expected `variantOne, variant_2`".to_string(),
            err.error.to_string()
        );
        assert_eq!("$.custom_tag", err.path.to_string());
    }

    #[test]
    fn test_internally_tagged_nested_error() {
        let json = serde_json::json!({
            "custom_tag": "variant_2",
            "prop1": "wrong type",
            "prop2": "testing"
        });
        let err =
            <InternallyTaggedEnum as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
                .unwrap_err();
        assert_eq!(
            "invalid type: string \"wrong type\", expected a boolean".to_string(),
            err.error.to_string()
        );
        assert_eq!("$.prop1", err.path.to_string());
    }

    #[test]
    fn test_internally_tagged_enum_json_schema() {
        let mut generator = schemars::r#gen::SchemaGenerator::default();
        let root_schema = traits::gen_root_schema_for::<InternallyTaggedEnum>(&mut generator);
        let exp = serde_json::json!(
            {
                "$schema": "http://json-schema.org/draft-07/schema#",
                "$id": "https://hasura.io/jsonschemas/metadata/InternallyTaggedEnum",
                "title": "InternallyTaggedEnum",
                "description": "An internally tagged enum",
                "oneOf": [
                    {
                        "$id": "https://hasura.io/jsonschemas/metadata/VariantOneStruct",
                        "title": "VariantOneStruct",
                        "type": "object",
                        "required": [
                            "custom_tag",
                            "propA",
                            "propB"
                        ],
                        "properties": {
                            "custom_tag": {
                                "type": "string",
                                "enum": [
                                    "variantOne"
                                ]
                            },
                            "propA": {
                                "type": "string"
                            },
                            "propB": {
                                "type": "integer",
                                "format": "int32"
                            }
                        },
                        "additionalProperties": false
                    },
                    {
                        "$id": "https://hasura.io/jsonschemas/metadata/VariantTwoStruct",
                        "title": "VariantTwoStruct",
                        "type": "object",
                        "required": [
                            "custom_tag",
                            "prop1",
                            "prop2"
                        ],
                        "properties": {
                            "custom_tag": {
                                "type": "string",
                                "enum": [
                                    "variant_2"
                                ]
                            },
                            "prop1": {
                                "type": "boolean"
                            },
                            "prop2": {
                                "type": "string"
                            }
                        },
                        "additionalProperties": false
                    }
                ]
            }
        );

        assert_eq!(
            serde_json::to_string_pretty(&exp).unwrap(),
            serde_json::to_string_pretty(&root_schema).unwrap()
        );
    }
}
