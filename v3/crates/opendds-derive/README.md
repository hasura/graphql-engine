## Table Of Contents
- [Derive Macros for `OpenDd` Trait](#derive-macros-for--opendd--trait)
- [The Trait](#the-trait)
- [Struct](#struct)
  * [Attributes](#attributes)
    + [Type Level](#type-level)
    + [Field Level](#field-level)
- [Enum](#enum)
  * [Attributes](#attributes-1)
    + [Type Level](#type-level-1)
    + [Variant Level](#variant-level)
- [Common JSON Schema attributes](#common-json-schema-attributes)
  * [Type Level](#type-level-2)
  * [Field Level](#field-level-1)
- [Notes](#notes)

## Derive Macros for `OpenDd` Trait

This crate provides a derive macro for implementing the `OpenDd` trait from the `open-dds` crate. To utilize it, simply add
`opendds-derive` as a dependency in your `Cargo.toml` file, alongside the `open-dds`crate. Then, include `opendds_derive::OpenDd`
in the `#[derive(..)]` attributes list. Please note that at present, only `struct` and `enum` types are supported for derivation.
Use `#[opendd()]` to specify type level and field or variant level attributes

## The Trait

The `OpenDd` trait is defined as follows,

```rust
pub trait OpenDd: Sized {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError>;

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema;

    fn _schema_name() -> String;

    fn _schema_is_referenceable() -> bool {
        false
    }
}
```

All types used to express the open data domain specification need to have the `OpenDd` trait implemented. At present, the `OpenDd` trait
exposes methods for deserializing the type from a JSON value and generating a json schema for the type.

## Struct

Limitations:
- Only structs with named fields and single unnamed field (newtype) are allowed to use the derive macro
```rust
#[derive(opendds_derive::OpenDd)]
struct NamedFieldStruct {
  named_field_1: Type1,
  named_field_2: Type2,
}

#[derive(opendds_derive::OpenDd)]
struct NewTypeStruct(Field);
```
- Structs with multiple unnamed fields or no fields are *not supported*.
```rust
struct UnitStruct;
struct UnnamedFieldStruct(Type1, Type2);
```

Common Behavior:
- All fields of struct are deserialized with their *camel-cased* fields in the JSON object.
- Having unknown fields in the object results in parse error.

### Attributes

#### Type Level

Only [json schema](#type-level-2) related attributes applicable here.

#### Field Level

- `#[opendd(default)]`

    If the value is not present when deserializing, use the `Default::default()`.

- `#[opendd(rename = "name")]`

    Deserialize this field with the given name instead of *camel-cased* Rust field name.

## Enum

Limitations:

- All variants should carry exactly one unnamed field.
```rust
#[derive(opendds_derive::OpenDd)]
enum MyEnum {
    VariantOne(TypeOne),
    VariantTwo(TypeTwo),
}
```

- Variants with no fields, multiple unnamed fields or named fields are *not supported*

```rust
enum Unsupported {
    NoFields,
    MultipleUnnamed(TypeOne, TypeTwo),
    Named{field_1: TypeOne, field_1: TypeTwo},
}
```

### Attributes

#### Type Level

Enum types are deserialized from JSON objects using a specific key-value pair. The key, also referred to as the tag, must be a
`String` value. It determines the variant of the enum to deserialize from the rest of the JSON object's content. The following
type-level attributes provide various ways for deserializing the JSON object along with [json schema](#type-level-2)
related attributes.

- `#[opendd(as_versioned_internally_tagged)]`

    Using `version` key as tag. The tag value is matched with *camel-cased* variant name. Rest of the object's content is deserialized
    as the variant value.

    Example:
    ``` rust
    #[derive(opendds_derive::OpenDd)]
    #[opendd(as_versioned_internally_tagged)]
    enum VersionedEnum {
        V1(VersionOne),
        V2(VersionTwo),
    }

    #[derive(opendds_derive::OpenDd)]
    struct VersionOne {
        #[opendd(use_serde_json)]
        field_one: String
    }
    ```
    The following object is parsed into `V1(VersionOne)`
    ```json
    {
        "version": "v1",
        "fieldOne": "some_value"
    }
    ```

- `#[opendd(as_versioned_with_definition)]`

    Using `version` key as tag. The tag value is matched with *camel-cased* variant name. The variant value is deserialized with
    the `definition` key value from the object.

    Example:
    ``` rust
    #[derive(opendds_derive::OpenDd)]
    #[opendd(as_versioned_with_definition)]
    enum VersionedEnum {
        V1(VersionOne),
        V2(VersionTwo),
    }

    #[derive(opendds_derive::OpenDd)]
    struct VersionTwo {
        #[opendd(use_serde_json)]
        field_two: String
    }
    ```
    The following object is parsed into `V2(VersionTwo)`
    ```json
    {
        "version": "v2",
        "definition": {
            "fieldTwo": "some_value"
        }
    }
    ```

- `#[opendd(as_kind)]`

    Using `kind` key as tag. The tag value is matched with the *exact* variant name. Rest of the object's content is deserialized
    as the variant value.

    Example:
    ```rust
    #[derive(opendds_derive::OpenDd)]
    #[opendd(as_kind)]
    enum KindEnum {
        KindOne(KindOneStruct),
        KindTwo(KindTwoStruct),
    }

    #[derive(opendds_derive::OpenDd)]
    #[opendd(use_serde_json)] // All field values are deserialized using serde_path_to_error::deserialize()
    struct KindOneStruct{
        field_one: i32,
        field_two: bool,
        field_three: String,
    }
    ```
    The following object is parsed into `KindOne(KindOneStruct)`
    ```json
    {
        "kind": "KindOne",
        "fieldOne": 111,
        "fieldTwo": false,
        "fieldThree": "three"
    }
    ```

- `#[opendd(untagged_with_kind)]`

    The JSON object is not tagged with any variant. Each variant should hold a enum type with `#[opendd(as_kind)]` (see above)
    implementation. Using `kind` as tag and its value is matched with internal enum variants. The internal enum variants need to
    have `strum_macros::EnumVariantNames` implementation.

    Example:
    ```rust
    #[derive(opendds_derive::OpenDd)]
    #[opendd(as_kind)]
    enum KindEnumOne {
        VariantOne(OneStruct),
        VaraintTwo(TwoStruct),
    }

    #[derive(opendds_derive::OpenDd)]
    #[opendd(as_kind)]
    enum KindEnumTwo {
        VariantThree(ThreeStruct),
        VaraintFour(FourStruct)
    }

    #[derive(opendds_derive::OpenDd)]
    #[opendd(untagged_with_kind)]
    enum UntaggedEnum {
        KindOne(KindEnumOne),
        KindTwo(KindEnumTwo),
    }

    #[derive(opendds_derive::OpenDd)]
    struct FourStruct {
        #[opendd(use_serde_json)]
        field_four: String,
    }
    ```
    The following object is parsed into `UntaggedEnum::KindTwo(KindEnumTwo::VariantFour(FourStruct{field_four: "four"}))`
    ```json
    {
        "kind": "VariantFour",
        "fieldFour": "four"
    }
    ```

#### Variant Level

- `#[opendd(rename = "name")]`

    Deserialize this variant with the given name and use it to generate enum value for the tag in the json schema.

- `#[opendd(alias = "name")]`

    Deserialize this variant from the given name *or* from derived Rust name.

## Common JSON Schema attributes

The following json schema related attributes are applicable for both structs and enums.

### Type Level

- `#[opendd(json_schema(rename = "rename string value"))]`

    Use the given name in the generated schema instead of the Rust name.

- `#[opendd(json_schema(title = "title string value"))]`

    Set the generated schema's title.

- `#[opendd(json_schema(example = "some::function"))]`

    Include the result of the given function in the generated schema's `examples`.

### Field Level

- `#[opendd(json_schema(default_exp = "some::function()"))]`

    To be used in conjuction with [#[opendd(default)]](#field-level). The given function should return a json value which
    is included in the generated schema's `default`. Not needed when the field type has `serde::Serialize` trait implemented.
    The default JSON value will be inferred using `serde_json::json!(Default::default())`.

## Notes
- Please make sure the following crates/modules are accessible in the module where the derive macro is used.
  - `opendds` - as module or crate with `derive` in the path. For eg. the macro refers the trait with `opendds::derive::OpenDd`
  - `strum` - to access `strum::VariantNames`
