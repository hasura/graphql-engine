/// Configuration for the metadata-resolve step.
///
/// Deserialization is only intended to be used for testing and is not reliable.
#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct Configuration {
    pub unstable_features: UnstableFeatures,
}

/// internal feature flags used in metadata resolve steps
///
/// Deserialization is only intended to be used for testing and is not reliable.
#[derive(Debug, Clone, Copy, Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
#[allow(clippy::struct_excessive_bools)]
pub struct UnstableFeatures {
    pub enable_order_by_expressions: bool,
    pub enable_ndc_v02_support: bool,
    pub enable_jsonapi: bool,
    pub enable_aggregation_predicates: bool,
}
