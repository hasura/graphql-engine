use hasura_authn_core::Role;
use indexmap::IndexMap;
use open_dds::commands::CommandName;
use serde::{Deserialize, Serialize};

use crate::helpers::typecheck;
use crate::stages::commands;
use crate::types::error::ShouldBeAnError;
use crate::types::permission::ValueExpressionOrPredicate;
use crate::types::subgraph::QualifiedTypeReference;
use crate::Qualified;
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

#[derive(Debug, thiserror::Error)]
pub enum CommandPermissionIssue {
    #[error("Type error in preset argument {argument_name:} for role {role:} in command {command_name:}: {typecheck_issue:}")]
    CommandArgumentPresetTypecheckIssue {
        role: Role,
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
        typecheck_issue: typecheck::TypecheckIssue,
    },
}

impl ShouldBeAnError for CommandPermissionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            CommandPermissionIssue::CommandArgumentPresetTypecheckIssue {
                typecheck_issue, ..
            } => typecheck_issue.should_be_an_error(flags),
        }
    }
}

/// The output of the command permissions stage.
pub struct CommandPermissionsOutput {
    pub permissions: IndexMap<Qualified<CommandName>, CommandWithPermissions>,
    pub issues: Vec<CommandPermissionIssue>,
}
