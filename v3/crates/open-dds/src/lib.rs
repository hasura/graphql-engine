extern crate self as open_dds;

use schemars::{schema::Schema::Object as SchemaObjectVariant, JsonSchema};
use serde::{Deserialize, Serialize};

pub mod accessor;
pub mod arguments;
pub mod commands;
pub mod data_connector;
pub mod flags;
pub mod graphql_config;
pub mod identifier;
pub mod models;
pub mod permissions;
pub mod relationships;
pub mod session_variables;
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
    fn deserialize(json: serde_json::Value) -> Result<Self, traits::OpenDdDeserializeError> {
        serde_path_to_error::deserialize(json).map_err(|e| traits::OpenDdDeserializeError {
            path: traits::JSONPath::from_serde_path(e.path()),
            error: e.into_inner(),
        })
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        // This is copied from ndc-sdk to avoid establishing a dependency.
        let mut s = EnvironmentValueImpl::json_schema(gen);
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
    Serialize, Clone, Debug, PartialEq, strum_macros::EnumVariantNames, opendds_derive::OpenDd,
)]
#[serde(tag = "kind")]
#[opendd(as_kind)]
pub enum OpenDdSubgraphObject {
    // Data connector
    DataConnectorLink(data_connector::DataConnectorLink),

    // Types
    ObjectType(types::ObjectType),
    ScalarType(types::ScalarType),
    ObjectBooleanExpressionType(types::ObjectBooleanExpressionType),

    // Data Connector Scalar Representation
    DataConnectorScalarRepresentation(types::DataConnectorScalarRepresentation),

    // Models
    Model(models::Model),

    // Commands
    Command(commands::Command),

    // Relationships
    Relationship(relationships::Relationship),

    // Permissions
    TypePermissions(permissions::TypePermissions),
    ModelPermissions(permissions::ModelPermissions),
    CommandPermissions(permissions::CommandPermissions),
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
    fn deserialize(json: serde_json::Value) -> Result<Self, traits::OpenDdDeserializeError> {
        match json {
            serde_json::Value::Array(arr) => Ok(Self::WithoutNamespaces(<Vec<
                OpenDdSubgraphObject,
            > as traits::OpenDd>::deserialize(
                serde_json::Value::Array(arr),
            )?)),
            serde_json::Value::Object(obj) => Ok(Self::Versioned(
                <MetadataWithVersion as traits::OpenDd>::deserialize(serde_json::Value::Object(
                    obj,
                ))?,
            )),
            _ => Err(traits::OpenDdDeserializeError {
                path: traits::JSONPath::new(),
                error: serde::de::Error::invalid_type(
                    serde::de::Unexpected::Other("not a sequence or map"),
                    &"a sequence or map",
                ),
            }),
        }
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        let schema = schemars::schema::Schema::Object(schemars::schema::SchemaObject {
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                any_of: Some(vec![
                    traits::gen_subschema_for::<Vec<OpenDdSubgraphObject>>(gen),
                    traits::gen_subschema_for::<MetadataWithVersion>(gen),
                ]),
                ..Default::default()
            })),
            ..Default::default()
        });
        schemars::_private::apply_metadata(
            schema,
            schemars::schema::Metadata {
                title: Some(Self::_schema_name()),
                description: Some(
                    "All of the metadata required to run Hasura v3 engine.".to_owned(),
                ),
                ..Default::default()
            },
        )
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
                <Metadata as traits::OpenDd>::deserialize(json)
            }
            Err(e) => Err(traits::OpenDdDeserializeError {
                path: traits::JSONPath::from_serde_path(&track.path()),
                error: e,
            }),
        }
    }
}

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
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdMetadataV1"))]
pub struct MetadataV1 {
    pub namespaces: Vec<NamespacedObjects>,
    #[opendd(default, json_schema(default_exp = "flags::Flags::default_json()"))]
    pub flags: flags::Flags,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
pub struct NamespacedObjects {
    pub name: String,
    pub objects: Vec<OpenDdSubgraphObject>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdMetadataV2"))]
pub struct MetadataV2 {
    #[opendd(default, json_schema(default_exp = "Supergraph::default_json()"))]
    pub supergraph: Supergraph,
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub subgraphs: Vec<Subgraph>,
    #[opendd(default, json_schema(default_exp = "flags::Flags::default_json()"))]
    pub flags: flags::Flags,
}

#[derive(
    Serialize, Clone, Debug, PartialEq, strum_macros::EnumVariantNames, opendds_derive::OpenDd,
)]
#[serde(tag = "kind")]
#[opendd(as_kind)]
pub enum OpenDdSupergraphObject {
    // GraphQL schema configuration
    GraphqlConfig(graphql_config::GraphqlConfig),
}

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

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdSubgraph"))]
pub struct Subgraph {
    pub name: identifier::Identifier,
    pub objects: Vec<OpenDdSubgraphObject>,
}

#[cfg(test)]
pub mod tests {
    use crate::{
        test_utils::{validate_root_json_schema, JsonSchemaValidationConfig},
        traits::gen_root_schema_for,
    };
    use goldenfile::Mint;
    use pretty_assertions::assert_eq;
    use std::{io::Write, path::PathBuf};

    #[test]
    fn test_metadata_schema() {
        let mut mint = Mint::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
        let mut expected = mint.new_goldenfile("metadata.jsonschema").unwrap();
        let schema =
            gen_root_schema_for::<super::Metadata>(&mut schemars::gen::SchemaGenerator::default());
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
        let metadata_from_json =
            <super::Metadata as super::traits::OpenDd>::deserialize(metadata_json).unwrap();
        assert_eq!(metadata, metadata_from_json);
    }

    #[test]
    /// Runs various checks on the generated JSONSchema to ensure it follows certain conventions.
    fn test_validate_json_schema() {
        validate_root_json_schema(
            gen_root_schema_for::<super::Metadata>(&mut schemars::gen::SchemaGenerator::default()),
            &JsonSchemaValidationConfig::new(),
        );
    }
}
