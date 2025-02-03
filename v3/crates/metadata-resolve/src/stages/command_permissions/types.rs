use hasura_authn_core::Role;
use serde::{Deserialize, Serialize};

use crate::stages::commands;
use crate::types::permission::ValueExpressionOrPredicate;
use crate::types::subgraph::QualifiedTypeReference;
use open_dds::arguments::ArgumentName;

use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandWithPermissions {
    pub command: commands::Command,
    pub permissions: BTreeMap<Role, CommandPermission>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandPermission {
    pub allow_execution: bool,
    pub argument_presets:
        BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
}
