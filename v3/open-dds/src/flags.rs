use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, JsonSchema)]
#[schemars(rename = "OpenDdFlags")]
pub struct Flags {
    #[serde(default)]
    pub require_ndc_capabilities: bool,
    #[serde(default)]
    pub require_graphql_config: bool,
}
