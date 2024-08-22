use super::compatibility_date::CompatibilityDate;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(id = "v1/CompatibilityConfig"))]
/// The compatibility configuration of the Hasura metadata.
pub struct CompatibilityConfigV1 {
    /// Any backwards incompatible changes made to Hasura DDN after this date won't impact the metadata.
    pub date: CompatibilityDate,
    // TODO: add flags.
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(id = "v2/CompatibilityConfig"))]
/// The compatibility configuration of the Hasura metadata.
pub struct CompatibilityConfigV2 {
    /// Any backwards incompatible changes made to Hasura DDN after this date won't impact the metadata.
    pub date: CompatibilityDate,
    // TODO: add flags.
}

#[derive(Serialize, Debug, Clone, PartialEq, opendds_derive::OpenDd, Deserialize)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(as_versioned_with_definition)]
#[opendd(json_schema(title = "CompatibilityConfig"))]
/// Definition of the authentication configuration used by the API server.
pub enum CompatibilityConfig {
    /// Definition of the authentication configuration v1, used by the API server.
    #[opendd(json_schema(title = "CompatibilityConfigV1"))]
    V1(CompatibilityConfigV1),
    /// Definition of the authentication configuration v2, used by the API server.
    #[opendd(json_schema(title = "CompatibilityConfigV2"))]
    V2(CompatibilityConfigV2),
}
