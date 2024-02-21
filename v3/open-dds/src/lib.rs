use schemars::{
    schema::{
        RootSchema, Schema::Object as SchemaObjectVariant, SchemaObject, SubschemaValidation,
    },
    schema_for, JsonSchema,
};
use serde::{Deserialize, Serialize};
use serde_untagged::UntaggedEnumVisitor;

pub mod accessor;
pub mod arguments;
pub mod commands;
pub mod data_connector;
pub mod flags;
pub mod graphql_config;
pub mod models;
pub mod permissions;
pub mod relationships;
pub mod session_variables;
pub mod types;

// In the user facing configuration, the connection string can either be a literal or a reference
// to a secret, so we advertize either in the JSON schema. However, when building the configuration,
// we expect the metadata build service to have resolved the secret reference so we deserialize
// only to a literal value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
pub struct EnvironmentValue {
    pub value: String,
}

impl JsonSchema for EnvironmentValue {
    fn schema_name() -> String {
        "EnvironmentValue".into()
    }

    fn schema_id() -> std::borrow::Cow<'static, str> {
        "https://hasura.io/jsonschemas/EnvironmentValue".into()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        // This is copied from ndc-sdk to avoid establishing a dependency.
        let mut s = EnvironmentValueImpl::json_schema(gen);
        if let SchemaObjectVariant(o) = &mut s {
            if let Some(m) = &mut o.metadata {
                m.id = Some(Self::schema_id().into());
            }
        }
        s
    }
}

// This is copied from ndc-sdk to avoid establishing a dependency.
#[derive(Serialize, Deserialize, Debug, Clone, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "EnvironmentValue")]
/// Either a literal string or a reference to a Hasura secret
enum EnvironmentValueImpl {
    Value(String),
    ValueFromEnv(String),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, strum_macros::EnumVariantNames)]
#[serde(tag = "kind")]
pub enum OpenDdSubgraphObject {
    // Data connector
    DataConnectorLink(data_connector::DataConnectorLink),

    // Types
    ObjectType(types::ObjectType),
    ScalarType(types::ScalarType),

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

impl JsonSchema for OpenDdSubgraphObject {
    fn schema_name() -> String {
        "OpenDdSubgraphObject".into()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        json_schema_for_object_enum::<OpenDdSubgraphObject>(vec![
            json_schema_with_kind::<data_connector::DataConnectorLink>(gen),
            json_schema_with_kind::<types::ObjectType>(gen),
            json_schema_with_kind::<types::ScalarType>(gen),
            json_schema_with_kind::<types::DataConnectorScalarRepresentation>(gen),
            json_schema_with_kind::<models::Model>(gen),
            json_schema_with_kind::<commands::Command>(gen),
            json_schema_with_kind::<relationships::Relationship>(gen),
            json_schema_with_kind::<permissions::TypePermissions>(gen),
            json_schema_with_kind::<permissions::ModelPermissions>(gen),
            json_schema_with_kind::<permissions::CommandPermissions>(gen),
        ])
    }
}

/// All of the metadata required to run Hasura v3 engine.
#[derive(Serialize, Clone, Debug, PartialEq, JsonSchema)]
#[schemars(rename = "OpenDdMetadata")]
#[serde(untagged)]
pub enum Metadata {
    // We didn't introduce versioning before namespaces, so we still need to handle that case as untagged.
    WithoutNamespaces(Vec<OpenDdSubgraphObject>),
    Versioned(MetadataWithVersion),
}

// An automatically-derived Deserialize impl would not give any useful information on parse errors
// if the input is not either a sequence, or a map with a "version" field with the exact value
// "v1". This custom impl fixes that.
impl<'de> Deserialize<'de> for Metadata {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        UntaggedEnumVisitor::new()
            .seq(|input_seq| input_seq.deserialize().map(Metadata::WithoutNamespaces))
            .map(|input_map| input_map.deserialize().map(Metadata::Versioned))
            .deserialize(deserializer)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version", rename_all = "camelCase")]
#[schemars(rename = "OpenDdMetadataWithVersion")]
pub enum MetadataWithVersion {
    #[serde(alias = "V1")]
    V1(MetadataV1),
    #[serde(alias = "V2")]
    V2(MetadataV2),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[schemars(rename = "OpenDdMetadataV1")]
pub struct MetadataV1 {
    pub namespaces: Vec<NamespacedObjects>,
    #[serde(default)]
    pub flags: flags::Flags,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
pub struct NamespacedObjects {
    pub name: String,
    pub objects: Vec<OpenDdSubgraphObject>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[schemars(rename = "OpenDdMetadataV2")]
pub struct MetadataV2 {
    #[serde(default)]
    pub supergraph: Supergraph,
    #[serde(default)]
    pub subgraphs: Vec<Subgraph>,
    #[serde(default)]
    pub flags: flags::Flags,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, strum_macros::EnumVariantNames)]
#[serde(tag = "kind")]
pub enum OpenDdSupergraphObject {
    // GraphQL schema configuration
    GraphqlConfig(graphql_config::GraphqlConfig),
}

impl JsonSchema for OpenDdSupergraphObject {
    fn schema_name() -> String {
        "OpenDdSupergraphObject".into()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        json_schema_for_object_enum::<OpenDdSupergraphObject>(vec![json_schema_with_kind::<
            graphql_config::GraphqlConfig,
        >(gen)])
    }
}

#[derive(Serialize, Deserialize, Default, Clone, Debug, PartialEq, JsonSchema)]
#[schemars(rename = "OpenDdSupergraph")]
pub struct Supergraph {
    #[serde(default)]
    pub objects: Vec<OpenDdSupergraphObject>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[schemars(rename = "OpenDdSubgraph")]
pub struct Subgraph {
    pub name: String,
    pub objects: Vec<OpenDdSubgraphObject>,
}

fn add_kind_to_json_schema(schema: &mut schemars::schema::Schema, kind: String) {
    if let SchemaObjectVariant(SchemaObject {
        object: Some(object_schema),
        ..
    }) = schema
    {
        let inserted_index = object_schema.properties.len();
        object_schema.properties.insert(
            "kind".to_string(),
            SchemaObjectVariant(SchemaObject {
                instance_type: Some(schemars::schema::InstanceType::String.into()),
                enum_values: Some(vec![serde_json::Value::String(kind)]),
                ..Default::default()
            }),
        );
        // Move the kind property to the front of the object, so that tooling displays
        // it first.
        object_schema.properties.move_index(inserted_index, 0);
        object_schema.required.insert("kind".into());
    } else {
        panic!("Unexpected schema");
    }
}

pub fn json_schema_with_kind<T>(
    gen: &mut schemars::gen::SchemaGenerator,
) -> (String, schemars::schema::Schema)
where
    T: JsonSchema,
{
    let kind = T::schema_name();
    let mut schema = T::json_schema(gen);
    add_id_to_schema(kind.as_str(), &mut schema);
    if let SchemaObjectVariant(SchemaObject {
        subschemas: Some(subschema),
        ..
    }) = &mut schema
    {
        if let Some(ref mut one_ofs) = subschema.one_of {
            for one_of in one_ofs.iter_mut() {
                add_kind_to_json_schema(one_of, kind.clone());
            }
        } else {
            panic!("Unexpected schema");
        }
    } else if let SchemaObjectVariant(SchemaObject {
        object: Some(_), ..
    }) = schema
    {
        add_kind_to_json_schema(&mut schema, kind.clone());
    } else {
        panic!("Unexpected schema");
    }

    (kind, schema)
}

pub fn json_schema_for_object_enum<EnumType>(
    subschemas: Vec<(String, schemars::schema::Schema)>,
) -> schemars::schema::Schema
where
    EnumType: strum::VariantNames,
{
    if subschemas.len() != EnumType::VARIANTS.len() {
        panic!("Different number of variants in enum and JSON schema");
    }
    for (subschema, variant_name) in subschemas.iter().zip(EnumType::VARIANTS.iter()) {
        if subschema.0 != *variant_name {
            panic!("Different variant tag used in enum vs JSON schema");
        }
    }
    SchemaObjectVariant(SchemaObject {
        subschemas: Some(Box::new(SubschemaValidation {
            one_of: Some(subschemas.into_iter().map(|(_, schema)| schema).collect()),
            ..Default::default()
        })),
        ..Default::default()
    })
}

/// Takes the enum T which would generate a oneOf JSONSchema and
/// edits each oneOf subschema to allow additional properties.
/// This is used when #[serde(flatten)]'ing the
pub fn one_of_with_additional_properties<T>(
    gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema
where
    T: JsonSchema,
{
    let mut schema = T::json_schema(gen);
    if let SchemaObjectVariant(SchemaObject {
        subschemas: Some(subschema),
        ..
    }) = &mut schema
    {
        if let Some(subschemas) = &mut subschema.one_of {
            for subschema in subschemas {
                if let SchemaObjectVariant(SchemaObject {
                    object: Some(ref mut object_schema),
                    ..
                }) = subschema
                {
                    object_schema.additional_properties = None;
                } else {
                    // Okay to panic since this is only used at compile time.
                    panic!("unexpected subschema");
                }
            }
            return schema;
        }
    }
    // Okay to panic since this is only used at compile time.
    panic!("Unexpected schema");
}

fn add_id_to_schema(schema_name: &str, schema: &mut schemars::schema::Schema) {
    if let SchemaObjectVariant(object) = schema {
        // Don't set $id for references, the $id should be set on the referenced schema
        if object.is_ref() {
            return;
        }
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

// Automatically generates the `$id` field for all sub-schemas if they also have a title.
// Ideally, the $id should be added when generating the sub-schema itself,
// but schemars doesn't have a derive attribute to do that yet.
pub fn json_schema_with_ids<T>() -> RootSchema
where
    T: JsonSchema,
{
    let mut root_schema = schema_for!(T);
    for (schema_name, schema) in root_schema.definitions.iter_mut() {
        add_id_to_schema(schema_name, schema);
    }
    root_schema
}

#[cfg(test)]
mod tests {
    use super::json_schema_with_ids;
    use goldenfile::Mint;
    use std::{io::Write, path::PathBuf};

    #[test]
    fn test_metadata_schema() {
        let mut mint = Mint::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
        let mut expected = mint.new_goldenfile("metadata.jsonschema").unwrap();
        let schema = json_schema_with_ids::<super::Metadata>();
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&schema).unwrap()
        )
        .unwrap();
    }

    #[test]
    fn test_parse_reference_metdata() {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/reference.json");
        let metadata = std::fs::read_to_string(path).unwrap();
        serde_json::from_str::<super::Metadata>(&metadata).unwrap();
    }
}
