//! internal feature flags exposed with `UNSTABLE_FEATURES` environment variable

/// Set of features in development that we want to switch on in development
/// If we want to start offering user control of these, they should move out of here and into the
/// flags in Metadata, nothing here should be depended on.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    clap::ValueEnum,
    serde::Deserialize,
    serde::Serialize,
)]
#[serde(rename_all = "snake_case")]
pub enum UnstableFeature {
    EnableNdcV02Support,
    EnableAggregationPredicates,
}

pub fn resolve_unstable_features(
    unstable_features: &[UnstableFeature],
) -> metadata_resolve::configuration::UnstableFeatures {
    let mut features = metadata_resolve::configuration::UnstableFeatures::default();

    for unstable_feature in unstable_features {
        match unstable_feature {
            UnstableFeature::EnableNdcV02Support => {
                features.enable_ndc_v02_support = true;
            }
            UnstableFeature::EnableAggregationPredicates => {
                features.enable_aggregation_predicates = true;
            }
        }
    }
    features
}
