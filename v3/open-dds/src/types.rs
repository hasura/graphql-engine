use std::fmt::Display;

use schemars::JsonSchema;
use serde::{
    de::value::{StrDeserializer, StringDeserializer},
    Deserialize, Serialize,
};

use crate::{
    data_connector::DataConnectorName, impl_JsonSchema_with_OpenDd_for, impl_OpenDd_default_for,
};

#[derive(
    Serialize,
    Deserialize,
    Hash,
    Clone,
    Debug,
    PartialEq,
    Eq,
    JsonSchema,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
#[serde(untagged)]
// TODO: This serde untagged causes bad error messages when the type name is invalid.
// Either manually deserialize it or use a library to make the error messages better.
pub enum TypeName {
    Inbuilt(InbuiltType),
    Custom(CustomTypeName),
}

/// The name of a user-defined type.
#[derive(
    Serialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    PartialOrd,
    Ord,
    opendds_derive::OpenDd,
)]
pub struct CustomTypeName(pub String);

impl_JsonSchema_with_OpenDd_for!(CustomTypeName);

impl CustomTypeName {
    fn new(s: String) -> Result<CustomTypeName, String> {
        // First character should be alphabetic or underscore
        let first_char_valid =
            matches!(s.chars().next(), Some(c) if c.is_ascii_alphabetic() || c == '_');
        // All characters should be alphanumeric or underscore
        let all_chars_valid = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
        // Should not be an inbuilt type
        let not_an_inbuilt_type =
            InbuiltType::deserialize(StrDeserializer::<serde::de::value::Error>::new(s.as_str()))
                .is_err();
        if first_char_valid && all_chars_valid && not_an_inbuilt_type {
            Ok(CustomTypeName(s))
        } else {
            Err(format!("invalid custom type name: {s}"))
        }
    }
}

impl<'de> Deserialize<'de> for CustomTypeName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        CustomTypeName::new(s).map_err(serde::de::Error::custom)
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
/// Reference to an Open DD type including nullable values and arrays.
/// Suffix `!` to indicate a non-nullable reference, and wrap in `[]` to indicate an array.
/// Eg: `[String!]!` is a non-nullable array of non-nullable strings.
pub struct TypeReference {
    pub underlying_type: BaseType,
    pub nullable: bool,
}

impl_OpenDd_default_for!(TypeReference);

impl Display for TypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.underlying_type,
            if self.nullable { "" } else { "!" }
        )
    }
}

impl Serialize for TypeReference {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str(&format!("{self}"))
    }
}

impl<'de> Deserialize<'de> for TypeReference {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let mut chars = s.chars();
        Ok(if chars.next_back() == Some('!') {
            TypeReference {
                underlying_type: BaseType::deserialize(StrDeserializer::new(chars.as_str()))?,
                nullable: false,
            }
        } else {
            TypeReference {
                underlying_type: BaseType::deserialize(StringDeserializer::new(s))?,
                nullable: true,
            }
        })
    }
}

const TYPE_REFERENCE_DESCRIPTION: &str = r#"A reference to an Open DD type including nullable values and arrays.
Suffix '!' to indicate a non-nullable reference, and wrap in '[]' to indicate an array.
Eg: '[String!]!' is a non-nullable array of non-nullable strings."#;

impl JsonSchema for TypeReference {
    fn schema_name() -> String {
        "TypeReference".into()
    }

    // TODO: Add description / examples to the json schema
    fn json_schema(_gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::Schema::Object(schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some(Self::schema_name()),
                description: Some(TYPE_REFERENCE_DESCRIPTION.into()),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::SingleOrVec::Single(Box::new(
                schemars::schema::InstanceType::String,
            ))),
            ..Default::default()
        })
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq, derive_more::Display)]
pub enum BaseType {
    #[display(fmt = "{_0}")]
    Named(TypeName),
    #[display(fmt = "[{_0}]")]
    List(Box<TypeReference>),
}

impl JsonSchema for BaseType {
    fn schema_name() -> String {
        "BaseType".into()
    }

    // TODO: Add description / examples to the json schema
    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        String::json_schema(gen)
    }
}

impl Serialize for BaseType {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str(&format!("{self}"))
    }
}

impl<'de> Deserialize<'de> for BaseType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let mut chars = s.chars();
        Ok(
            if chars.next() == Some('[') && chars.next_back() == Some(']') {
                BaseType::List(Box::new(TypeReference::deserialize(StrDeserializer::new(
                    chars.as_str(),
                ))?))
            } else {
                BaseType::Named(TypeName::deserialize(StringDeserializer::new(s))?)
            },
        )
    }
}

#[derive(
    Serialize, Deserialize, Hash, Clone, Debug, PartialEq, Eq, JsonSchema, derive_more::Display,
)]
#[schemars(title = "InbuiltType")]
/// An inbuilt primitive OpenDD type.
pub enum InbuiltType {
    ID,
    Int,
    Float,
    Boolean,
    String,
}

#[derive(
    Serialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    JsonSchema,
    opendds_derive::OpenDd,
)]
pub struct GraphQlTypeName(pub String);

/// The name of a GraphQL object field.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    JsonSchema,
    opendds_derive::OpenDd,
)]
pub struct GraphQlFieldName(pub String);

/// GraphQL configuration of an Open DD object type.
#[derive(Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ObjectTypeGraphQLConfiguration"))]
pub struct ObjectTypeGraphQLConfiguration {
    /// The name to use for the GraphQL type representation of this object type when used in an output context.
    pub type_name: Option<GraphQlTypeName>,
    /// The name to use for the GraphQL type representation of this object type when used in an input context.
    pub input_type_name: Option<GraphQlTypeName>,
    // TODO: Add type_kind if we want to allow making objects interfaces.
}

/// Definition of a user-defined Open DD object type.
#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(as_versioned_with_definition, json_schema(title = "ObjectType"))]
pub enum ObjectType {
    V1(ObjectTypeV1),
}

impl ObjectType {
    pub fn upgrade(self) -> ObjectTypeV1 {
        match self {
            ObjectType::V1(v1) => v1,
        }
    }
}

#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ObjectTypeV1", example = "ObjectTypeV1::example"))]
/// Definition of a user-defined Open DD object type.
pub struct ObjectTypeV1 {
    /// The name to give this object type, used to refer to it elsewhere in the metadata.
    /// Must be unique across all types defined in this subgraph.
    pub name: CustomTypeName,

    /// The list of fields defined for this object type.
    pub fields: Vec<FieldDefinition>,

    /// The subset of fields that uniquely identify this object in the domain. Setting
    /// this property will automatically implement the GraphQL Relay Node interface for this
    /// object type and add an `id` global ID field. If setting this property, there must
    /// not be a field named `id` already present.
    pub global_id_fields: Option<Vec<FieldName>>,

    /// Configuration for how this object type should appear in the GraphQL schema.
    pub graphql: Option<ObjectTypeGraphQLConfiguration>,
    /// The description of the object.
    /// Gets added to the description of the object's definition in the graphql schema.
    pub description: Option<String>,
}

impl ObjectTypeV1 {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "name": "Author",
                "fields": [
                    {
                        "name": "author_id",
                        "type": "Int!",
                        "description": "The id of the author"
                    },
                    {
                        "name": "first_name",
                        "type": "String",
                        "description": "The first name of the author"
                    },
                    {
                        "name": "last_name",
                        "type": "String",
                        "description": "The last name of the author"
                    }
                ],
                "globalIdFields": [
                    "author_id"
                ],
                "graphql": {
                    "typeName": "Author",
                    "inputTypeName": null
                },
                "description": "An author of a book"
            }
        )
    }
}

/// The name of a field in a user-defined object type.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    PartialOrd,
    Ord,
    opendds_derive::OpenDd,
)]
pub struct FieldName(pub String);

impl_JsonSchema_with_OpenDd_for!(FieldName);

/// The definition of a field in a user-defined object type.
#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ObjectFieldDefinition"))]
pub struct FieldDefinition {
    /// The name of the field. This name is used both when referring to the field elsewhere in the metadata
    /// and when creating the corresponding GraphQl type.
    pub name: FieldName,

    /// The type of this field. This uses the GraphQL syntax to represent field types and must refer
    /// to one of the inbuilt OpenDd types or another user-defined type.
    #[opendd(rename = "type")]
    pub field_type: TypeReference,
    /// The description of this field.
    /// Gets added to the description of the field's definition in the graphql schema.
    pub description: Option<String>,
}

/// GraphQL configuration of an Open DD scalar type
#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ScalarTypeGraphQLConfiguration"))]
pub struct ScalarTypeGraphQLConfiguration {
    /// The name of the GraphQl type to use for this scalar.
    pub type_name: GraphQlTypeName,
    // TODO: add a representation field if we want to give semantics to this
    // scalar type.
}

/// Definition of a user-defined scalar type that that has opaque semantics.
#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(as_versioned_with_definition, json_schema(title = "ScalarType"))]
pub enum ScalarType {
    V1(ScalarTypeV1),
}

impl ScalarType {
    pub fn upgrade(self) -> ScalarTypeV1 {
        match self {
            ScalarType::V1(v1) => v1,
        }
    }
}

#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ScalarTypeV1", example = "ScalarTypeV1::example"))]
/// Definition of a user-defined scalar type that that has opaque semantics.
pub struct ScalarTypeV1 {
    /// The name to give this scalar type, used to refer to it elsewhere in the metadata.
    /// Must be unique across all types defined in this subgraph.
    pub name: CustomTypeName,
    /// Configuration for how this scalar type should appear in the GraphQL schema.
    pub graphql: Option<ScalarTypeGraphQLConfiguration>,
    /// The description of this scalar.
    /// Gets added to the description of the scalar's definition in the graphql schema.
    pub description: Option<String>,
}

impl ScalarTypeV1 {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "name": "CustomString",
                "graphql": {
                    "typeName": "CustomString"
                },
                "description": "A custom string type"
            }
        )
    }
}

/// GraphQL configuration of a data connector scalar
#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "DataConnectorScalarGraphQLConfiguration"))]
pub struct DataConnectorScalarGraphQLConfiguration {
    pub comparison_expression_type_name: Option<GraphQlTypeName>,
}

/// The representation of a data connector scalar in terms of Open DD types
#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "DataConnectorScalarRepresentation")
)]
pub enum DataConnectorScalarRepresentation {
    V1(DataConnectorScalarRepresentationV1),
}

impl DataConnectorScalarRepresentation {
    pub fn upgrade(self) -> DataConnectorScalarRepresentationV1 {
        match self {
            DataConnectorScalarRepresentation::V1(v1) => v1,
        }
    }
}

#[derive(Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(
    title = "DataConnectorScalarRepresentationV1",
    example = "DataConnectorScalarRepresentationV1::example"
))]
/// The representation of a data connector scalar in terms of Open DD types
pub struct DataConnectorScalarRepresentationV1 {
    /// The name of the data connector that this scalar type comes from.
    pub data_connector_name: DataConnectorName,
    /// The name of the scalar type coming from the data connector.
    pub data_connector_scalar_type: String,
    /// The name of the Open DD type that this data connector scalar type should be represented as.
    pub representation: TypeName,
    /// Configuration for how this scalar's operators should appear in the GraphQL schema.
    pub graphql: Option<DataConnectorScalarGraphQLConfiguration>,
}

impl DataConnectorScalarRepresentationV1 {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "dataConnectorName": "data_connector",
                "dataConnectorScalarType": "varchar",
                "representation": "String",
                "graphql": {
                    "comparisonExpressionTypeName": "String_Comparison_Exp"
                }
            }
        )
    }
}
