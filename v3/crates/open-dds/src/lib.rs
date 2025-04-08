extern crate self as open_dds;

use std::borrow::Cow;

use open_dds::spanned::Spanned;
use schemars::{JsonSchema, schema::Schema::Object as SchemaObjectVariant};
use serde::{Deserialize, Serialize};

pub mod accessor;
pub mod aggregates;
pub mod arguments;
pub mod boolean_expression;
pub mod commands;
pub mod data_connector;
pub mod flags;
pub mod graphql_config;
pub mod identifier;
pub mod models;
pub mod order_by_expression;
pub mod permissions;
pub mod plugins;
pub mod query;
pub mod relationships;
pub mod session_variables;
pub mod spanned;
pub mod test_utils;
pub mod traits;
pub mod types;

// In the user facing configuration, the connection string can either be a literal or a reference
// to a secret, so we advertize either in the JSON schema. However, when building the configuration,
// we expect the metadata build service to have resolved the secret reference so we deserialize
// only to a literal value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
pub struct EnvironmentValue {
    pub value: String,
}

// Requires a custom OpenDd impl since the json schema generation is based on 'EnvironmentValueImpl'
impl traits::OpenDd for EnvironmentValue {
    fn deserialize(
        json: serde_json::Value,
        _path: jsonpath::JSONPath,
    ) -> Result<Self, traits::OpenDdDeserializeError> {
        serde_path_to_error::deserialize(json).map_err(|e| traits::OpenDdDeserializeError {
            path: jsonpath::JSONPath::from_serde_path(e.path()),
            error: e.into_inner(),
        })
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        // This is copied from ndc-sdk to avoid establishing a dependency.
        let mut s = EnvironmentValueImpl::json_schema(generator);
        if let SchemaObjectVariant(o) = &mut s {
            if let Some(m) = &mut o.metadata {
                m.id = Some("https://hasura.io/jsonschemas/EnvironmentValue".into());
            }
        }
        s
    }

    fn _schema_name() -> String {
        "EnvironmentValue".to_owned()
    }

    fn _schema_is_referenceable() -> bool {
        true
    }
}

impl_JsonSchema_with_OpenDd_for!(EnvironmentValue);

// This is copied from ndc-sdk to avoid establishing a dependency.
#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "EnvironmentValue")]
/// Either a literal string or a reference to a Hasura secret
enum EnvironmentValueImpl {
    Value(String),
    ValueFromEnv(String),
}

#[derive(
    Serialize, Clone, Debug, PartialEq, strum_macros::VariantNames, opendds_derive::OpenDd,
)]
#[serde(tag = "kind")]
#[opendd(as_kind)]
pub enum OpenDdSubgraphObject {
    // Data connector
    DataConnectorLink(Spanned<data_connector::DataConnectorLink>),

    // GraphQL "super-graph" level config
    // This is boxed because it bloats the enum's size
    // See: https://rust-lang.github.io/rust-clippy/master/index.html#large_enum_variant
    GraphqlConfig(Spanned<graphql_config::GraphqlConfig>),

    // Types
    ObjectType(Spanned<types::ObjectType>),
    ScalarType(Spanned<types::ScalarType>),
    ObjectBooleanExpressionType(Spanned<types::ObjectBooleanExpressionType>),
    BooleanExpressionType(Spanned<boolean_expression::BooleanExpressionType>),

    // OrderBy Expressions
    OrderByExpression(Spanned<order_by_expression::OrderByExpression>),

    // Data Connector Scalar Representation
    DataConnectorScalarRepresentation(Spanned<types::DataConnectorScalarRepresentation>),

    // Aggregate Expressions
    AggregateExpression(Spanned<aggregates::AggregateExpression>),

    // Models
    Model(Spanned<models::Model>),

    // Commands
    Command(Spanned<commands::Command>),

    // Relationships
    Relationship(Spanned<relationships::Relationship>),

    // Permissions
    TypePermissions(Spanned<permissions::TypePermissions>),
    ModelPermissions(Spanned<permissions::ModelPermissions>),
    CommandPermissions(Spanned<permissions::CommandPermissions>),

    // Plugin
    LifecyclePluginHook(Spanned<plugins::LifecyclePluginHook>),
}

/// All of the metadata required to run Hasura v3 engine.
#[derive(Serialize, Clone, Debug, PartialEq)]
#[serde(untagged)]
pub enum Metadata {
    // We didn't introduce versioning before namespaces, so we still need to handle that case as untagged.
    WithoutNamespaces(Vec<OpenDdSubgraphObject>),
    Versioned(MetadataWithVersion),
}

impl traits::OpenDd for Metadata {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, traits::OpenDdDeserializeError> {
        match json {
            serde_json::Value::Array(arr) => Ok(Self::WithoutNamespaces(<Vec<
                OpenDdSubgraphObject,
            > as traits::OpenDd>::deserialize(
                serde_json::Value::Array(arr),
                path,
            )?)),
            serde_json::Value::Object(obj) => Ok(Self::Versioned(
                <MetadataWithVersion as traits::OpenDd>::deserialize(
                    serde_json::Value::Object(obj),
                    path,
                )?,
            )),
            _ => Err(traits::OpenDdDeserializeError {
                path: jsonpath::JSONPath::new(),
                error: serde::de::Error::invalid_type(
                    serde::de::Unexpected::Other("not a sequence or map"),
                    &"a sequence or map",
                ),
            }),
        }
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        let mut schema_object = schemars::schema::SchemaObject {
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                any_of: Some(vec![
                    traits::gen_subschema_for::<Vec<OpenDdSubgraphObject>>(generator),
                    traits::gen_subschema_for::<MetadataWithVersion>(generator),
                ]),
                ..Default::default()
            })),
            ..Default::default()
        };
        let metadata = schema_object.metadata();
        metadata.title = Some(Self::_schema_name());
        metadata.description =
            Some("All of the metadata required to run Hasura v3 engine.".to_owned());
        schemars::schema::Schema::Object(schema_object)
    }

    fn _schema_name() -> String {
        "OpenDdMetadata".to_owned()
    }

    fn _schema_is_referenceable() -> bool {
        true
    }
}

impl Metadata {
    pub fn from_json_str(s: &str) -> Result<Self, traits::OpenDdDeserializeError> {
        // First deserialize the JSON string into a serde_json::Value using serde_path_to_error
        // to record the path to the error in case of a parse error.

        let json_deserializer = &mut serde_json::Deserializer::from_str(s);

        let mut track = serde_path_to_error::Track::new();
        let json_deserializer_with_path =
            serde_path_to_error::Deserializer::new(json_deserializer, &mut track);

        match serde_json::Value::deserialize(json_deserializer_with_path) {
            Ok(json) => {
                // Then deserialize the serde_json::Value into the OpenDd type using the OpenDd trait.
                <Metadata as traits::OpenDd>::deserialize(json, jsonpath::JSONPath::new())
            }
            Err(e) => Err(traits::OpenDdDeserializeError {
                path: jsonpath::JSONPath::from_serde_path(&track.path()),
                error: e,
            }),
        }
    }

    pub fn get_flags(&self) -> Cow<flags::OpenDdFlags> {
        match self {
            Metadata::WithoutNamespaces(_) => Cow::Owned(flags::OpenDdFlags::default()),
            Metadata::Versioned(metadata) => match metadata {
                MetadataWithVersion::V1(metadata) => Cow::Borrowed(&metadata.flags),
                MetadataWithVersion::V2(metadata) => Cow::Borrowed(&metadata.flags),
                MetadataWithVersion::V3(metadata) => Cow::Borrowed(&metadata.flags),
            },
        }
    }
}

/// Metadata with versioning.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", rename_all = "camelCase")]
#[opendd(
    as_versioned_internally_tagged,
    json_schema(rename = "OpenDdMetadataWithVersion")
)]
pub enum MetadataWithVersion {
    #[serde(alias = "V1")]
    #[opendd(alias = "V1")]
    V1(MetadataV1),
    #[serde(alias = "V2")]
    #[opendd(alias = "V2")]
    V2(MetadataV2),
    #[serde(alias = "V3")]
    #[opendd(alias = "V3")]
    V3(MetadataV3),
}

/// The v1 metadata.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdMetadataV1"))]
pub struct MetadataV1 {
    pub namespaces: Vec<NamespacedObjects>,
    #[opendd(
        default,
        json_schema(default_exp = "serde_json::to_value(flags::OpenDdFlags::default()).unwrap()")
    )]
    pub flags: flags::OpenDdFlags,
}

/// A collection of objects that are related to each other.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
pub struct NamespacedObjects {
    pub name: String,
    pub objects: Vec<OpenDdSubgraphObject>,
}

/// The v2 metadata.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdMetadataV2"))]
pub struct MetadataV2 {
    #[opendd(default, json_schema(default_exp = "Supergraph::default_json()"))]
    pub supergraph: Supergraph,
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub subgraphs: Vec<Subgraph>,
    #[opendd(
        default,
        json_schema(default_exp = "serde_json::to_value(flags::OpenDdFlags::default()).unwrap()")
    )]
    pub flags: flags::OpenDdFlags,
}

/// The v3 metadata.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdMetadataV3"))]
pub struct MetadataV3 {
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub subgraphs: Vec<Subgraph>,
    #[opendd(
        default,
        json_schema(default_exp = "serde_json::to_value(flags::OpenDdFlags::default()).unwrap()")
    )]
    pub flags: flags::OpenDdFlags,
}

#[derive(
    Serialize, Clone, Debug, PartialEq, strum_macros::VariantNames, opendds_derive::OpenDd,
)]
#[serde(tag = "kind")]
#[opendd(as_kind)]
pub enum OpenDdSupergraphObject {
    /// GraphQL schema configuration
    GraphqlConfig(graphql_config::GraphqlConfig),
}

/// A collection of objects that apply to the entire supergraph.
#[derive(Serialize, Default, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdSupergraph"))]
pub struct Supergraph {
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub objects: Vec<OpenDdSupergraphObject>,
}

impl Supergraph {
    pub fn default_json() -> serde_json::Value {
        serde_json::json!({
            "objects": []
        })
    }
}

/// A subgraph is a collection of objects that belong to the same data domain.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdSubgraph"))]
pub struct Subgraph {
    pub name: identifier::SubgraphNameInput,
    pub objects: Vec<OpenDdSubgraphObject>,
}

#[cfg(test)]
pub mod tests {
    use crate::{
        test_utils::{JsonSchemaValidationConfig, validate_root_json_schema},
        traits::gen_root_schema_for,
    };
    use goldenfile::Mint;
    use pretty_assertions::assert_eq;
    use std::{io::Write, path::PathBuf};

    #[test]
    fn test_metadata_schema() {
        let mut mint = Mint::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
        let mut expected = mint.new_goldenfile("metadata.jsonschema").unwrap();
        let mut schema = gen_root_schema_for::<super::Metadata>(
            &mut schemars::r#gen::SchemaGenerator::default(),
        );
        schema.definitions.sort_keys();
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&schema).unwrap()
        )
        .unwrap();
    }

    #[test]
    /// This test is a round trip test for the Metadata type. It reads a reference metadata file,
    /// deserializes it into a Metadata type, serializes it back to JSON, deserializes it back into
    /// a Metadata type, and compares the two Metadata types to ensure they are equal.
    fn test_serialize_reference_metadata() {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/reference.json");
        let metadata =
            open_dds::Metadata::from_json_str(&std::fs::read_to_string(path).unwrap()).unwrap();
        let metadata_json = serde_json::to_value(metadata.clone()).unwrap();
        let metadata_from_json = <super::Metadata as super::traits::OpenDd>::deserialize(
            metadata_json,
            jsonpath::JSONPath::new(),
        )
        .unwrap();
        assert_eq!(metadata, metadata_from_json);
    }

    #[test]
    /// Runs various checks on the generated JSONSchema to ensure it follows certain conventions.
    fn test_validate_json_schema() {
        validate_root_json_schema(
            gen_root_schema_for::<super::Metadata>(&mut schemars::r#gen::SchemaGenerator::default()),
            &JsonSchemaValidationConfig::new(),
        );
    }
}
