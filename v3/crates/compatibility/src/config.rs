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
