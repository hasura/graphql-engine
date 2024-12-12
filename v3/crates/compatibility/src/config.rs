use super::compatibility_date::CompatibilityDate;

#[derive(Clone, Debug, PartialEq, serde::Serialize, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(id = "v1/CompatibilityConfig"))]
/// The compatibility configuration of the Hasura metadata.
pub struct CompatibilityConfigV1 {
    /// Any backwards incompatible changes made to Hasura DDN after this date won't impact the metadata.
    pub date: CompatibilityDate,
    // TODO: add flags.
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(id = "v2/CompatibilityConfig"))]
/// The compatibility configuration of the Hasura metadata.
pub struct CompatibilityConfigV2 {
    /// Any backwards incompatible changes made to Hasura DDN after this date won't impact the metadata.
    pub date: CompatibilityDate,
    // TODO: add flags.
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum CompatibilityError {
    #[error("no compatibility config found")]
    NoCompatibilityConfigFound,
    #[error("duplicate compatibility config found")]
    DuplicateCompatibilityConfig,
    #[error("compatibility date {specified} is too old, oldest supported date is {oldest}")]
    DateTooOld {
        specified: CompatibilityDate,
        oldest: CompatibilityDate,
    },
    #[error("compatibility date {0} is in the future")]
    DateInTheFuture(CompatibilityDate),
}
