## Table Of Contents

- [Derive Macros for `OpenDd` Trait](#derive-macros-for--opendd--trait)
- [The Trait](#the-trait)
- [Struct](#struct)
  - [Attributes](#attributes)
    - [Type Level](#type-level)
    - [Field Level](#field-level)
- [Enum](#enum)
  - [Attributes](#attributes-1)
    - [Type Level](#type-level-1)
    - [Variant Level](#variant-level)
- [Common JSON Schema attributes](#common-json-schema-attributes)
- [Notes](#notes)

## Derive Macros for `OpenDd` Trait

This crate provides a derive macro for implementing the `OpenDd` trait from the
`open-dds` crate. To utilize it, simply add `opendds-derive` as a dependency in
your `Cargo.toml` file, alongside the `open-dds`crate. Then, include
`opendds_derive::OpenDd` in the `#[derive(..)]` attributes list. Please note
that at present, only `struct` and `enum` types are supported for derivation.
Use `#[opendd()]` to specify type level and field or variant level attributes

## The Trait

The `OpenDd` trait is defined as follows,

```rust
pub trait OpenDd: Sized {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError>;

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema;

    fn _schema_name() -> String;

    fn _schema_is_referenceable() -> bool {
        false
    }
}
```

All types used to express the open data domain specification need to have the
`OpenDd` trait implemented. At present, the `OpenDd` trait exposes methods for
deserializing the type from a JSON value and generating a json schema for the
type.

## Struct

Limitations:

- Only structs with named fields and single unnamed field (newtype) are allowed
  to use the derive macro

```rust
#[derive(opendds_derive::OpenDd)]
struct NamedFieldStruct {
  named_field_1: Type1,
  named_field_2: Type2,
}

#[derive(opendds_derive::OpenDd)]
struct NewTypeStruct(Field);
```

- Structs with multiple unnamed fields or no fields are _not supported_.

```rust
struct UnitStruct;
struct UnnamedFieldStruct(Type1, Type2);
```

Common Behavior:

- All fields of struct are deserialized with their _camel-cased_ fields in the
  JSON object.
- Having unknown fields in the object results in parse error.

### Attributes

#### Type Level

Only [json schema](#common-json-schema-attributes) related attributes applicable
here.

#### Field Level

- `#[opendd(default)]`

  If the value is not present when deserializing, use the `Default::default()`.

- `#[opendd(default = <value>)]`

  If the value is not present when deserializing, use the value provided.

- `#[opendd(rename = "name")]`

  Deserialize this field with the given name instead of _camel-cased_ Rust field
  name.

- `[opendd(hidden = true)]`

  Hide the field from the json schema, useful for keeping work in progress out
  of the public API.

- `#[opendd(deserialize_with = "function_name")]`

  Use a custom deserializer function for this field. The function should have
  the signature
  `fn(serde_json::Value, jsonpath::JSONPath) -> Result<T, OpenDdDeserializeError>`
  where `T` is the field type.

- `#[opendd(json_schema(default_exp = "some::function()"))]`

  To be used in conjuction with [#[opendd(default)]](#field-level). The given
  function should return a json value which is included in the generated
  schema's `default`. Not needed when the field type has `serde::Serialize`
  trait implemented. The default JSON value will be inferred using
  `serde_json::json!(Default::default())`.

- `#[opendd(json_schema(title = "title string value"))]`

  Set the generated JSON schema's title.

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

- Variants with no fields, multiple unnamed fields or named fields are _not
  supported_

```rust
enum Unsupported {
    NoFields,
    MultipleUnnamed(TypeOne, TypeTwo),
    Named{field_1: TypeOne, field_1: TypeTwo},
}
```

### Attributes

#### Type Level

Enum types are deserialized from JSON objects using a specific key-value pair.
The key, also referred to as the tag, must be a `String` value. It determines
the variant of the enum to deserialize from the rest of the JSON object's
content. The following type-level attributes provide various ways for
deserializing the JSON object along with [json schema](#type-level-2) related
attributes.

- `#[opendd(as_versioned_internally_tagged)]`

  Using `version` key as tag. The tag value is matched with _camel-cased_
  variant name. Rest of the object's content is deserialized as the variant
  value.

  Example:

  ```rust
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

  Using `version` key as tag. The tag value is matched with _camel-cased_
  variant name. The variant value is deserialized with the `definition` key
  value from the object.

  Example:

  ```rust
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

  Using `kind` key as tag. The tag value is matched with the _exact_ variant
  name. Rest of the object's content is deserialized as the variant value.

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

  The JSON object is not tagged with any variant. Each variant should hold a
  enum type with `#[opendd(as_kind)]` (see above) implementation. Using `kind`
  as tag and its value is matched with internal enum variants. The internal enum
  variants need to have `strum_macros::VariantNames` implementation.

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

  The following object is parsed into
  `UntaggedEnum::KindTwo(KindEnumTwo::VariantFour(FourStruct { field_four: "four" }))`

  ```json
  {
    "kind": "VariantFour",
    "fieldFour": "four"
  }
  ```

- `#[opendd(externally_tagged)]`

  The JSON object is externally tagged, where the name of the variant is used as
  the property name and the variant contents is that property's value.

  Example:

  ```rust
  #[derive(OpenDd)]
  #[opendd(externally_tagged)]
  enum ExternallyTaggedEnum {
      VariantOne(VariantOneStruct),
      VariantTwo(VariantTwoStruct),
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
  ```

  The following JSON object is parsed into
  `ExternallyTaggedEnum::VariantTwo(VariantTwoStruct { prop_1: true, prop_2: "testing" })`:

  ```json
  {
    "variantTwo": {
      "prop1": true,
      "prop2": "testing"
    }
  }
  ```

#### Variant Level

- `#[opendd(rename = "name")]`

  Deserialize this variant with the given name and use it to generate enum value
  for the tag in the json schema.

- `#[opendd(alias = "name")]`

  Deserialize this variant from the given name _or_ from derived Rust name.

- `[opendd(hidden = true)]`

  Hide this variant from the json schema, useful for keeping work in progress
  out of the public API.

- `#[opendd(json_schema(title = "title string value"))]`

  Set the generated JSON schema's title. This applies exclusively to enums with
  the `as_versioned_with_definition` and `externally_tagged` attribute.

- `#[opendd(json_schema(example = "some::function"))]`

  Include the result of the given function in the generated JSON schema's
  `examples`. This applies exclusively to enums with the
  `as_versioned_with_definition` attribute.

## Common JSON Schema attributes

The following json schema related attributes are applicable for both structs and
enums.

- `#[opendd(json_schema(rename = "rename string value"))]`

  Use the given name in the generated schema instead of the Rust name.

- `#[opendd(json_schema(title = "title string value"))]`

  Set the generated schema's title.

- `#[opendd(json_schema(id = "json schema id"))]`

  Set the generated schema's
  [`$id`](https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-02#section-8.2.2).

- `#[opendd(json_schema(example = "some::function"))]`

  Include the result of the given function in the generated schema's `examples`.

## Notes

- Please make sure the following crates/modules are accessible in the module
  where the derive macro is used.
  - `opendds` - as module or crate with `derive` in the path. For eg. the macro
    refers the trait with `opendds::derive::OpenDd`
  - `strum` - to access `strum::VariantNames`
