/// Configuration for the metadata-resolve step.
///
/// Deserialization is only intended to be used for testing and is not reliable.
#[derive(Debug, Clone, Copy, Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct Configuration {
    pub allow_unknown_subgraphs: bool,
    pub unstable_features: UnstableFeatures,
    pub warnings_to_raise: WarningsToRaise,
}

/// internal feature flags used in metadata resolve steps
///
/// Deserialization is only intended to be used for testing and is not reliable.
#[derive(Debug, Clone, Copy, Default, serde::Deserialize)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct UnstableFeatures {
    pub enable_order_by_expressions: bool,
    pub enable_ndc_v02_support: bool,
}

/// struct of warnings that we'd like to raise to errors, based on CompatibilityConfig
///
/// Deserialization is only intended to be used for testing and is not reliable.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, serde::Deserialize)]
pub struct WarningsToRaise {}
