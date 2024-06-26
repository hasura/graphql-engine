use std::ops::Deref;

use schemars::schema::{Schema::Object as SchemaObjectVariant, SchemaObject, StringValidation};
use serde::{de::Error, Deserialize};
use smol_str::SmolStr;

use crate::{
    impl_JsonSchema_with_OpenDd_for,
    traits::{JSONPath, OpenDd, OpenDdDeserializeError},
};

// Macro to produce a validated identifier using a string literal that crashes
// if the literal is invalid. Does not work for non-literal strings to avoid
// use on user supplied input.
#[macro_export]
macro_rules! identifier {
    ($name:literal) => {
        open_dds::identifier::Identifier::new($name.to_string()).unwrap()
    };
}

/// Type capturing an identifier used within the metadata. The wrapped String
/// is guaranteed to be a valid identifier, i.e.
/// - starts with an alphabet or underscore
/// - all characters are either alphanumeric or underscore
#[derive(
    Clone, Debug, derive_more::Display, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize,
)]
pub struct Identifier(SmolStr);

impl Identifier {
    pub fn new(value: impl AsRef<str>) -> Result<Identifier, &'static str> {
        let value = value.as_ref();
        if let Some(c) = value.chars().next() {
            if !c.is_ascii_alphabetic() && c != '_' {
                return Err("must start with an alphabet or underscore");
            }
        } else {
            return Err("cannot be an empty string");
        }
        if !value.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
            return Err("must contain only alphanumeric characters or underscore");
        }
        Ok(Identifier(SmolStr::new(value)))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

impl Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl OpenDd for Identifier {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError> {
        let string: String =
            serde_json::from_value(json).map_err(|error| OpenDdDeserializeError {
                error,
                path: JSONPath::default(),
            })?;
        Identifier::new(string).map_err(|e| OpenDdDeserializeError {
            error: serde_json::Error::custom(e),
            path: JSONPath::default(),
        })
    }

    fn json_schema(_gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        SchemaObjectVariant(SchemaObject {
            instance_type: Some(schemars::schema::SingleOrVec::Single(Box::new(
                schemars::schema::InstanceType::String,
            ))),
            string: Some(Box::new(StringValidation {
                pattern: Some("^[_a-zA-Z][_a-zA-Z0-9]*$".to_string()),
                ..Default::default()
            })),
            ..Default::default()
        })
    }

    fn _schema_name() -> String {
        "Identifier".to_string()
    }

    fn _schema_is_referenceable() -> bool {
        // This is a tiny leaf schema so just make it non-referenceable to avoid a layer of
        // indirection in the overall JSONSchema.
        false
    }
}

impl_JsonSchema_with_OpenDd_for!(Identifier);

impl<'de> Deserialize<'de> for Identifier {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        OpenDd::deserialize(serde_json::Value::deserialize(deserializer)?).map_err(D::Error::custom)
    }
}

// Macro to produce a validated subgraph identifier using a string literal that crashes if the
// literal is invalid. Does not work for non-literal strings to avoid use on user supplied input.
#[macro_export]
macro_rules! subgraph_identifier {
    ($name:literal) => {
        open_dds::identifier::SubgraphIdentifier::new($name.to_string()).unwrap()
    };
}

/// Type capturing a subgraph identifier used within the metadata. The wrapped String
/// is guaranteed to be a valid identifier, i.e.
/// - does not start with __
/// - starts with an alphabet or underscore
/// - all characters are either alphanumeric or underscore
#[derive(
    Clone, Debug, derive_more::Display, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize,
)]
pub struct SubgraphIdentifier(SmolStr);

impl SubgraphIdentifier {
    pub fn new(value: impl AsRef<str>) -> Result<SubgraphIdentifier, &'static str> {
        let value = value.as_ref();
        let Identifier(inner) = Identifier::new(value)?;
        if inner.starts_with("__") {
            return Err("__ is a reserved prefix for subgraph names");
        }
        Ok(SubgraphIdentifier(SmolStr::new(value)))
    }

    /// Creates a new subgraph identifier, skipping validation.
    pub fn new_without_validation(value: impl AsRef<str>) -> SubgraphIdentifier {
        SubgraphIdentifier(SmolStr::new(value))
    }

    /// Creates a new subgraph identifier from a static string, skipping
    /// validation.
    ///
    /// Panics if the string is more than 23 characters.
    pub const fn new_inline_static(value: &'static str) -> SubgraphIdentifier {
        SubgraphIdentifier(SmolStr::new_inline(value))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Deref for SubgraphIdentifier {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl OpenDd for SubgraphIdentifier {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError> {
        let string: String =
            serde_json::from_value(json).map_err(|error| OpenDdDeserializeError {
                error,
                path: JSONPath::default(),
            })?;
        SubgraphIdentifier::new(string).map_err(|e| OpenDdDeserializeError {
            error: serde_json::Error::custom(e),
            path: JSONPath::default(),
        })
    }

    fn json_schema(_gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        SchemaObjectVariant(SchemaObject {
            instance_type: Some(schemars::schema::SingleOrVec::Single(Box::new(
                schemars::schema::InstanceType::String,
            ))),
            string: Some(Box::new(StringValidation {
                pattern: Some("^(?!__)[_a-zA-Z][_a-zA-Z0-9]*$".to_string()),
                ..Default::default()
            })),
            ..Default::default()
        })
    }

    fn _schema_name() -> String {
        "SubgraphIdentifier".to_string()
    }

    fn _schema_is_referenceable() -> bool {
        // This is a tiny leaf schema so just make it non-referenceable to avoid a layer of
        // indirection in the overall JSONSchema.
        false
    }
}
