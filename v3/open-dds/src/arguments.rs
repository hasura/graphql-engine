use serde::{Deserialize, Serialize};

use crate::{impl_JsonSchema_with_OpenDd_for, types::TypeReference};

#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
pub struct ArgumentName(pub String);

impl_JsonSchema_with_OpenDd_for!(ArgumentName);

/// The definition of an argument for a field, command, or model.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ArgumentDefinition"))]
pub struct ArgumentDefinition {
    pub name: ArgumentName,
    #[serde(rename = "type")]
    #[opendd(rename = "type")]
    pub argument_type: TypeReference,
    pub description: Option<String>,
}
