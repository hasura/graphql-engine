use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::types::TypeReference;

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display, JsonSchema,
)]
pub struct ArgumentName(pub String);

/// The definition of an argument for a field, command, or model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(deny_unknown_fields)]
#[schemars(title = "ArgumentDefinition")]
pub struct ArgumentDefinition {
    pub name: ArgumentName,
    #[serde(rename = "type")]
    pub argument_type: TypeReference,
}
