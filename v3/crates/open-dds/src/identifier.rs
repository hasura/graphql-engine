use std::ops::Deref;

use schemars::schema::{Schema::Object as SchemaObjectVariant, SchemaObject, StringValidation};
use serde::{Deserialize, de::Error};
use smol_str::SmolStr;

use crate::{
    impl_JsonSchema_with_OpenDd_for,
    traits::{OpenDd, OpenDdDeserializeError},
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

impl std::borrow::Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.0.into()
    }
}

impl OpenDd for Identifier {
    fn deserialize(
        json: serde_json::Value,
        _path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        let string: String =
            serde_json::from_value(json).map_err(|error| OpenDdDeserializeError {
                error,
                path: jsonpath::JSONPath::default(),
            })?;
        Identifier::new(string).map_err(|e| OpenDdDeserializeError {
            error: serde_json::Error::custom(e),
            path: jsonpath::JSONPath::default(),
        })
    }

    fn json_schema(_gen: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
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
        OpenDd::deserialize(
            <serde_json::Value as serde::Deserialize>::deserialize(deserializer)?,
            jsonpath::JSONPath::new(),
        )
        .map_err(D::Error::custom)
    }
}

/// Type capturing a subgraph identifier for a user defined subgraph.
/// The wrapped String is guaranteed to be a valid identifier, i.e.
/// - does not start with __
/// - starts with an alphabet or underscore
/// - all characters are either alphanumeric or underscore
#[derive(
    Clone, Debug, derive_more::Display, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize,
)]
pub struct SubgraphNameInput(SmolStr);

impl SubgraphNameInput {
    pub fn new(value: impl AsRef<str>) -> Result<SubgraphNameInput, &'static str> {
        let value = value.as_ref();
        let Identifier(inner) = Identifier::new(value)?;
        if inner.starts_with("__") {
            return Err("__ is a reserved prefix for subgraph names");
        }
        Ok(SubgraphNameInput(SmolStr::new(value)))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Deref for SubgraphNameInput {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::borrow::Borrow<str> for SubgraphNameInput {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl OpenDd for SubgraphNameInput {
    fn deserialize(
        json: serde_json::Value,
        _path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        let string: String =
            serde_json::from_value(json).map_err(|error| OpenDdDeserializeError {
                error,
                path: jsonpath::JSONPath::default(),
            })?;
        SubgraphNameInput::new(string).map_err(|e| OpenDdDeserializeError {
            error: serde_json::Error::custom(e),
            path: jsonpath::JSONPath::default(),
        })
    }

    fn json_schema(_gen: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
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
        "SubgraphNameInput".to_string()
    }

    fn _schema_is_referenceable() -> bool {
        // This is a tiny leaf schema so just make it non-referenceable to avoid a layer of
        // indirection in the overall JSONSchema.
        false
    }
}

impl<'de> Deserialize<'de> for SubgraphNameInput {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        OpenDd::deserialize(
            <serde_json::Value as serde::Deserialize>::deserialize(deserializer)?,
            jsonpath::JSONPath::new(),
        )
        .map_err(D::Error::custom)
    }
}

impl_JsonSchema_with_OpenDd_for!(SubgraphNameInput);

/// The name of a subgraph.
///
/// This is different from 'SubgraphNameInput' which is more restrictive.
/// A SubgraphName may refer to a user defined subgraph (through 'SubgraphNameInput') or
/// a restricted namespace such as '__globals' or '__unknown_namespace'
///
/// Further, it may not be a valid identifier as v2 version of Metadata did not do any validation
/// on namespaces. When we deprecate v2 version of metadata, this can switch to str_newtype over
/// Identifier
///
/// This is also not meant to be used in any of the user facing metadata types, hence there is on
/// 'OpenDD' trait implementation. However, there is a 'JsonSchema' trait implementation to help
/// with query analytics
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
    schemars::JsonSchema,
)]
pub struct SubgraphName(SmolStr);

impl From<SubgraphNameInput> for SubgraphName {
    fn from(SubgraphNameInput(value): SubgraphNameInput) -> Self {
        SubgraphName(value)
    }
}

impl From<&SubgraphNameInput> for SubgraphName {
    fn from(SubgraphNameInput(value): &SubgraphNameInput) -> Self {
        SubgraphName(value.clone())
    }
}

impl SubgraphName {
    pub fn try_new(value: impl AsRef<str>) -> Result<SubgraphName, &'static str> {
        let value = value.as_ref();
        let Identifier(_inner) = Identifier::new(value)?;
        Ok(SubgraphName(SmolStr::new(value)))
    }

    /// Creates a new subgraph identifier, skipping validation.
    pub fn new_without_validation(value: impl AsRef<str>) -> SubgraphName {
        SubgraphName(SmolStr::new(value))
    }

    /// Creates a new subgraph identifier from a static string, skipping
    /// validation.
    ///
    /// Panics if the string is more than 23 characters.
    pub const fn new_inline_static(value: &'static str) -> SubgraphName {
        SubgraphName(SmolStr::new_inline(value))
    }
}

// Macro to produce a validated subgraph identifier using a string literal that crashes if the
// literal is invalid. Does not work for non-literal strings to avoid use on user supplied input.
#[macro_export]
macro_rules! subgraph_identifier {
    ($name:literal) => {
        open_dds::identifier::SubgraphName::try_new($name.to_string()).unwrap()
    };
}
