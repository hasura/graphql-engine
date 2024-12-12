use std::ops::Deref;

use indexmap::IndexMap;
use schemars::JsonSchema;
use serde::{
    de::value::{StrDeserializer, StringDeserializer},
    Deserialize, Serialize,
};

use crate::commands::ArgumentMapping;
use crate::{
    arguments::ArgumentName,
    data_connector::{
        DataConnectorColumnName, DataConnectorName, DataConnectorObjectType,
        DataConnectorScalarType,
    },
    identifier::Identifier,
    impl_JsonSchema_with_OpenDd_for, impl_OpenDd_default_for,
    models::EnableAllOrSpecific,
    str_newtype,
};

#[derive(
    Serialize,
    Deserialize,
    Hash,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    JsonSchema,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
#[serde(untagged)]
#[schemars(title = "TypeName")]
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
pub struct CustomTypeName(pub Identifier);

impl_JsonSchema_with_OpenDd_for!(CustomTypeName);

impl CustomTypeName {
    fn new(s: String) -> Result<CustomTypeName, String> {
        let identifier = Identifier::new(s)?;
        // Should not be an inbuilt type
        if InbuiltType::deserialize(StrDeserializer::<serde::de::value::Error>::new(
            identifier.as_str(),
        ))
        .is_ok()
        {
            Err(format!(
                "custom types cannot have the same name as an inbuilt type: {identifier}"
            ))
        } else {
            Ok(CustomTypeName(identifier))
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

impl std::fmt::Display for TypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.underlying_type)?;
        if !self.nullable {
            write!(f, "!")?;
        }
        Ok(())
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

const TYPE_REFERENCE_DESCRIPTION: &str = r"A reference to an Open DD type including nullable values and arrays.
Suffix '!' to indicate a non-nullable reference, and wrap in '[]' to indicate an array.
Eg: '[String!]!' is a non-nullable array of non-nullable strings.";

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
    #[display("{_0}")]
    Named(TypeName),
    #[display("[{_0}]")]
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
    Serialize,
    Deserialize,
    Hash,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    JsonSchema,
    strum_macros::EnumIter,
    derive_more::Display,
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

str_newtype!(GraphQlTypeName | doc "The name of a GraphQL type.");
str_newtype!(GraphQlFieldName | doc "The name of a GraphQL object field.");

/// GraphQL configuration of an Open DD object type.
#[derive(Serialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ObjectTypeGraphQLConfiguration"))]
pub struct ObjectTypeGraphQLConfiguration {
    /// The name to use for the GraphQL type representation of this object type when used in an output context.
    pub type_name: Option<GraphQlTypeName>,
    /// The name to use for the GraphQL type representation of this object type when used in an input context.
    pub input_type_name: Option<GraphQlTypeName>,
    /// Configuration for exposing apollo federation related types and directives.
    pub apollo_federation: Option<ObjectApolloFederationConfig>,
    // TODO: Add type_kind if we want to allow making objects interfaces.
}

/// Definition of a user-defined Open DD object type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "ObjectType", example = "ObjectType::example")
)]
pub enum ObjectType {
    V1(ObjectTypeV1),
}

impl ObjectType {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "ObjectType",
                "version": "v1",
                "definition": {
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
                        },
                        {
                            "name": "biography",
                            "type": "String",
                            "description": "AI generated biography for the author",
                            "arguments": [
                                {
                                    "name": "ai_model",
                                    "argumentType": "String!",
                                    "description": "The AI model to use for generating the biography"
                                }
                            ]
                        }
                    ],
                    "description": "An author of a book",
                    "globalIdFields": [
                        "author_id"
                    ],
                    "graphql": {
                        "typeName": "Author"
                    },
                    "dataConnectorTypeMapping": [
                        {
                            "dataConnectorName": "my_db",
                            "dataConnectorObjectType": "author",
                            "fieldMapping": {
                                "author_id": {
                                    "column": {
                                        "name": "id"
                                    }
                                }
                            }
                        },
                        {
                            "dataConnectorName": "my_vector_db",
                            "dataConnectorObjectType": "author",
                            "fieldMapping": {
                                "biography": {
                                    "column": {
                                        "name": "biography",
                                        "argumentMapping": {
                                            "ai_model": "model"
                                        }
                                    }
                                }
                            }
                        }
                    ]
                }
            }
        )
    }

    pub fn upgrade(self) -> ObjectTypeV1 {
        match self {
            ObjectType::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ObjectTypeV1"))]
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

    /// Mapping of this object type to corresponding object types in various data connectors.
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub data_connector_type_mapping: Vec<DataConnectorTypeMapping>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "DataConnectorTypeMapping"))]
/// This defines the mapping of the fields of an object type to the
/// corresponding columns of an object type in a data connector.
pub struct DataConnectorTypeMapping {
    pub data_connector_name: DataConnectorName,
    pub data_connector_object_type: DataConnectorObjectType,
    #[opendd(default)]
    pub field_mapping: FieldMappings,
}

#[derive(Serialize, Default, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
/// Mapping of object fields to their source columns in the data connector.
// We wrap maps into newtype structs so that we have a type and title for them in the JSONSchema which
// makes it easier to auto-generate documentation.
pub struct FieldMappings(pub IndexMap<FieldName, FieldMapping>);

impl Deref for FieldMappings {
    type Target = IndexMap<FieldName, FieldMapping>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "FieldMapping")]
pub enum FieldMapping {
    /// Source field directly maps to some column in the data connector.
    Column(ColumnFieldMapping),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ColumnFieldMapping")]
/// The target column in a data connector object that a source field maps to.
pub struct ColumnFieldMapping {
    /// The name of the target column
    pub name: DataConnectorColumnName, // TODO: Map field arguments

    /// Arguments to the column field
    pub argument_mapping: Option<ArgumentMapping>,
}

str_newtype!(DataConnectorArgumentName | doc "The name of an argument as defined by a data connector.");
str_newtype!(FieldName over Identifier | doc "The name of a field in a user-defined object type.");

/// The definition of a field in a user-defined object type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ObjectFieldDefinition"))]
pub struct FieldDefinition {
    /// The name of the field. This name is used both when referring to the field elsewhere in the metadata
    /// and when creating the corresponding GraphQl type.
    pub name: FieldName,

    /// The type of this field. This uses the GraphQL syntax to represent field types and must refer
    /// to one of the inbuilt OpenDd types or another user-defined type.
    #[serde(rename = "type")]
    #[opendd(rename = "type")]
    pub field_type: TypeReference,
    /// The description of this field.
    /// Gets added to the description of the field's definition in the graphql schema.
    pub description: Option<String>,

    /// Whether this field is deprecated.
    /// If set, the deprecation status is added to the field's graphql schema.
    pub deprecated: Option<Deprecated>,

    /// The arguments for the field
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub arguments: Vec<FieldArgumentDefinition>,
}

/// GraphQL configuration of an Open DD scalar type
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ScalarTypeGraphQLConfiguration"))]
pub struct ScalarTypeGraphQLConfiguration {
    /// The name of the GraphQl type to use for this scalar.
    pub type_name: GraphQlTypeName,
    // TODO: add a representation field if we want to give semantics to this
    // scalar type.
}

/// Definition of a user-defined scalar type that that has opaque semantics.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "ScalarType", example = "ScalarType::example")
)]
pub enum ScalarType {
    V1(ScalarTypeV1),
}

impl ScalarType {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "ScalarType",
                "version": "v1",
                "name": "CustomString",
                "graphql": {
                    "typeName": "CustomString"
                },
                "description": "A custom string type"
            }
        )
    }

    pub fn upgrade(self) -> ScalarTypeV1 {
        match self {
            ScalarType::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ScalarTypeV1"))]
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

/// GraphQL configuration of a data connector scalar
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "DataConnectorScalarGraphQLConfiguration"))]
pub struct DataConnectorScalarGraphQLConfiguration {
    pub comparison_expression_type_name: Option<GraphQlTypeName>,
}

/// The representation of a data connector scalar in terms of Open DD types
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
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

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(
    title = "DataConnectorScalarRepresentationV1",
    example = "DataConnectorScalarRepresentationV1::example"
))]
/// The representation of a data connector scalar in terms of Open DD types. Deprecated in favour
/// of `BooleanExpressionType`.
pub struct DataConnectorScalarRepresentationV1 {
    /// The name of the data connector that this scalar type comes from.
    pub data_connector_name: DataConnectorName,
    /// The name of the scalar type coming from the data connector.
    pub data_connector_scalar_type: DataConnectorScalarType,
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

/// Definition of a type representing a boolean expression on an Open DD object type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "ObjectBooleanExpressionType",
        example = "ObjectBooleanExpressionType::example"
    )
)]
pub enum ObjectBooleanExpressionType {
    V1(ObjectBooleanExpressionTypeV1),
}

impl ObjectBooleanExpressionType {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "ObjectBooleanExpressionType",
                "version": "v1",
                "definition": {
                    "name": "AuthorBoolExp",
                    "objectType": "Author",
                    "dataConnectorName": "my_db",
                    "dataConnectorObjectType": "author",
                    "comparableFields": [
                        {
                            "fieldName": "article_id",
                            "operators": {
                                "enableAll": true
                            }
                        },
                        {
                            "fieldName": "title",
                            "operators": {
                                "enableAll": true
                            }
                        },
                        {
                            "fieldName": "author_id",
                            "operators": {
                                "enableAll": true
                            }
                        }
                    ],
                    "graphql": {
                        "typeName": "Author_bool_exp"
                    }
                }
            }
        )
    }

    pub fn upgrade(self) -> ObjectBooleanExpressionTypeV1 {
        match self {
            ObjectBooleanExpressionType::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ComparableField"))]
/// A field of an object type that can be used for comparison when evaluating a boolean expression.
pub struct ComparableField {
    pub field_name: FieldName,
    pub operators: EnableAllOrSpecific<OperatorName>,
}

str_newtype!(OperatorName | doc "The name of an operator");

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ObjectBooleanExpressionTypeV1",))]
/// Definition of a type representing a boolean expression on an Open DD object type. Deprecated in
/// favour of `BooleanExpressionType`.
pub struct ObjectBooleanExpressionTypeV1 {
    /// The name to give this object boolean expression type, used to refer to it elsewhere in the metadata.
    /// Must be unique across all types defined in this subgraph.
    pub name: CustomTypeName,

    /// The name of the object type that this boolean expression applies to.
    pub object_type: CustomTypeName,

    /// The data connector this boolean expression type is based on.
    pub data_connector_name: DataConnectorName,

    /// The object type in the data connector's schema this boolean expression type is based on.
    pub data_connector_object_type: DataConnectorObjectType,

    /// The list of fields of the object type that can be used for comparison when evaluating this boolean expression.
    pub comparable_fields: Vec<ComparableField>,

    /// Configuration for how this object type should appear in the GraphQL schema.
    pub graphql: Option<ObjectBooleanExpressionTypeGraphQlConfiguration>,
}

/// GraphQL configuration of an Open DD boolean expression type.
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ObjectBooleanExpressionTypeGraphQlConfiguration"))]
pub struct ObjectBooleanExpressionTypeGraphQlConfiguration {
    /// The name to use for the GraphQL type representation of this boolean expression type.
    pub type_name: GraphQlTypeName,
}

/// OpenDd configuration to indicate whether an object type field, relationship, model
/// root field or command root field is deprecated.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[schemars(title = "Deprecated")]
pub struct Deprecated {
    /// The reason for deprecation.
    pub reason: Option<String>,
}

impl_OpenDd_default_for!(Deprecated);

/// Configuration for apollo federation related types and directives.
#[derive(Serialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd, Eq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "ObjectApolloFederationConfig"))]
pub struct ObjectApolloFederationConfig {
    pub keys: Vec<ApolloFederationObjectKey>,
}

/// The definition of a key for an apollo federation object.
#[derive(Serialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd, Eq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "ApolloFederationObjectKey"))]
pub struct ApolloFederationObjectKey {
    pub fields: Vec<FieldName>,
}

/// The definition of an argument for a field in a user-defined object type.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "FieldArgumentDefinition"))]
pub struct FieldArgumentDefinition {
    pub name: ArgumentName,
    pub argument_type: TypeReference,
    pub description: Option<String>,
}
