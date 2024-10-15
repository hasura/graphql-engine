use serde::Serialize;

/// Flags to configure the OpenDD metadata build.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdFlags"))]
#[allow(clippy::struct_excessive_bools)]
pub struct Flags {
    #[opendd(default, rename = "require_graphql_config")]
    // By default, OpenDd assumes camel-cased fields, rename to use snake-case
    pub require_graphql_config: bool,

    #[opendd(default, rename = "require_valid_ndc_v01_version")]
    pub require_valid_ndc_v01_version: bool,

    #[opendd(default, rename = "bypass_relation_comparisons_ndc_capability")]
    pub bypass_relation_comparisons_ndc_capability: bool,

    #[opendd(default, rename = "require_nested_array_filtering_capability")]
    pub require_nested_array_filtering_capability: bool,

    #[opendd(
        default,
        rename = "disallow_scalar_type_names_conflicting_with_inbuilt_types"
    )]
    pub disallow_scalar_type_names_conflicting_with_inbuilt_types: bool,

    #[opendd(default, rename = "propagate_boolean_expression_deprecation_status")]
    pub propagate_boolean_expression_deprecation_status: bool,

    #[opendd(default, rename = "require_unique_command_graphql_names")]
    pub require_unique_command_graphql_names: bool,

    #[opendd(default, rename = "allow_partial_supergraph")]
    pub allow_partial_supergraph: bool,
}

impl Flags {
    // This is separated from `Default` so we can use a `const` implementation.
    pub const fn new() -> Self {
        Self {
            require_graphql_config: false,
            require_valid_ndc_v01_version: false,
            bypass_relation_comparisons_ndc_capability: false,
            require_nested_array_filtering_capability: false,
            disallow_scalar_type_names_conflicting_with_inbuilt_types: false,
            propagate_boolean_expression_deprecation_status: false,
            require_unique_command_graphql_names: false,
            allow_partial_supergraph: false,
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
