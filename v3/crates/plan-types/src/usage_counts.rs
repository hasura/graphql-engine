//! Track the models that were used in query.

use metadata_resolve::Qualified;
use open_dds::{commands::CommandName, models::ModelName};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UsagesCounts {
    pub models_used: Vec<ModelCount>,
    pub commands_used: Vec<CommandCount>,
}

impl Default for UsagesCounts {
    fn default() -> Self {
        Self::new()
    }
}

impl UsagesCounts {
    pub fn new() -> Self {
        UsagesCounts {
            models_used: Vec::new(),
            commands_used: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct CommandCount {
    pub command: Qualified<CommandName>,
    pub count: usize,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ModelCount {
    pub model: Qualified<ModelName>,
    pub count: usize,
}
