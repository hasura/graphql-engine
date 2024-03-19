use schemars::schema::{Schema::Object as SchemaObjectVariant, SchemaObject, StringValidation};
use serde::{de::Error, Deserialize, Serialize};
use std::ops::Deref;

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
#[derive(Serialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(string: String) -> Result<Identifier, &'static str> {
        if let Some(c) = string.chars().next() {
            if !c.is_ascii_alphabetic() && c != '_' {
                return Err("must start with an alphabet or underscore");
            }
        } else {
            return Err("cannot be an empty string");
        }
        if !string
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            return Err("must contain only alphanumeric characters or underscore");
        }
        Ok(Identifier(string))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Deref for Identifier {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl OpenDd for Identifier {
    fn deserialize(json: serde_json::Value) -> Result<Self, OpenDdDeserializeError> {
        let string: String =
            serde_json::from_value(json).map_err(|error| OpenDdDeserializeError {
                error,
                path: Default::default(),
            })?;
        Identifier::new(string).map_err(|e| OpenDdDeserializeError {
            error: serde_json::Error::custom(e),
            path: Default::default(),
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
