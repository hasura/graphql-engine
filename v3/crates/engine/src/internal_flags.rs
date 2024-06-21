//! internal feature flags exposed with `UNSTABLE_FEATURES` environment variable

use clap::ValueEnum;
use metadata_resolve::MetadataResolveFlagsInternal;
use serde::Deserialize;

/// Set of features in development that we want to switch on in development
/// If we want to start offering user control of these, they should move out of here and into the
/// flags in Metadata, nothing here should be depended on.
#[derive(Debug, Clone, ValueEnum, PartialEq, Eq, PartialOrd, Ord, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UnstableFeature {
    EnableBooleanExpressionTypes,
    EnableAggregateRelationships,
}

pub fn resolve_unstable_features(
    unstable_features: &Vec<UnstableFeature>,
) -> MetadataResolveFlagsInternal {
    let mut metadata_resolve_flags = MetadataResolveFlagsInternal::default();

    for unstable_feature in unstable_features {
        match unstable_feature {
            UnstableFeature::EnableBooleanExpressionTypes => {
                metadata_resolve_flags.enable_boolean_expression_types = true;
            }
            UnstableFeature::EnableAggregateRelationships => {
                metadata_resolve_flags.enable_aggregate_relationships = true;
            }
        }
    }

    metadata_resolve_flags
}
