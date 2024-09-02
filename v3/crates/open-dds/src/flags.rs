use serde::Serialize;

/// Flags to configure the OpenDD metadata build.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdFlags"))]
pub struct Flags {
    #[opendd(default, rename = "require_graphql_config")]
    // By default, OpenDd assumes camel-cased fields, rename to use snake-case
    pub require_graphql_config: bool,

    #[opendd(default, rename = "require_valid_ndc_v01_version")]
    pub require_valid_ndc_v01_version: bool,

    #[opendd(default, rename = "bypass_relation_comparisons_ndc_capability")]
    pub bypass_relation_comparisons_ndc_capability: bool,
}

impl Flags {
    // This is separated from `Default` so we can use a `const` implementation.
    pub const fn new() -> Self {
        Self {
            require_graphql_config: false,
            require_valid_ndc_v01_version: false,
            bypass_relation_comparisons_ndc_capability: false,
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
