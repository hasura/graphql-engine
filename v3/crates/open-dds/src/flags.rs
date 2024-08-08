use serde::Serialize;

/// Flags to configure the OpenDD metadata build.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdFlags"))]
pub struct Flags {
    #[opendd(default, rename = "require_graphql_config")]
    // By default, OpenDd assumes camel-cased fields, rename to use snake-case
    pub require_graphql_config: bool,
}

impl Flags {
    // This is separated from `Default` so we can use a `const` implementation.
    pub const fn new() -> Self {
        Self {
            require_graphql_config: false,
        }
    }

    pub fn default_json() -> serde_json::Value {
        serde_json::to_value(Self::new()).unwrap()
    }
}

impl Default for Flags {
    fn default() -> Self {
        Self::new()
    }
}
