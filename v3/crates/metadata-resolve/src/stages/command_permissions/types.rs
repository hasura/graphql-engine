use error_context::Context;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use open_dds::commands::CommandName;
use open_dds::types::CustomTypeName;
use serde::{Deserialize, Serialize};

use crate::helpers::typecheck;
use crate::stages::commands;
use crate::types::error::{ContextualError, ShouldBeAnError};
use crate::types::permission::ValueExpressionOrPredicate;
use crate::types::subgraph::QualifiedTypeReference;
use crate::{ArgumentInfo, ConditionHash, ModelPredicate, Qualified, ValueExpression};
use open_dds::arguments::ArgumentName;

use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandWithPermissions {
    pub command: Command,
    pub permissions: CommandPermissions,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandPermissions {
    pub by_role: BTreeMap<Role, CommandPermission>,
    pub authorization_rules: Vec<CommandAuthorizationRule>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Command {
    pub name: Qualified<CommandName>,
    pub output_type: QualifiedTypeReference,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: Option<commands::CommandGraphQlApi>,
    pub source: Option<Arc<commands::CommandSource>>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandPermission {
    pub allow_execution: bool,
    pub argument_presets:
        BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
}

#[derive(Debug, thiserror::Error)]
#[allow(clippy::enum_variant_names)]
pub enum CommandPermissionIssue {
    #[error(
        "Type error in preset argument {argument_name:} {}in command {command_name:}: {typecheck_issue:}", 
            {match role { Some(role) => format!("for role {role} "), None => String::new()}}) 
    ]
    CommandArgumentPresetTypecheckIssue {
        role: Option<Role>,
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
        typecheck_issue: typecheck::TypecheckIssue,
    },
    #[error(
        "the object type {data_type} used as a return type for command {command_name} uses rules-based authorization so will not appear in the GraphQL schema"
    )]
    CommandReturnTypeUsesRulesBasedAuthorization {
        command_name: Qualified<CommandName>,
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "the command {command_name} uses rules-based authorization so will not appear in the GraphQL schema"
    )]
    CommandUsesRulesBasedAuthorization {
        command_name: Qualified<CommandName>,
    },
    #[error(
        "the object type {argument_type} used in arguments for the command {command_name} uses rules-based authorization so any presets will not be applied in the GraphQL schema"
    )]
    CommandArgumentTypeUsesRulesBasedAuthorization {
        command_name: Qualified<CommandName>,
        argument_type: Qualified<CustomTypeName>,
    },
}

impl ShouldBeAnError for CommandPermissionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            CommandPermissionIssue::CommandArgumentPresetTypecheckIssue {
                typecheck_issue, ..
            } => typecheck_issue.should_be_an_error(flags),
            CommandPermissionIssue::CommandReturnTypeUsesRulesBasedAuthorization { .. }
            | CommandPermissionIssue::CommandUsesRulesBasedAuthorization { .. }
            | CommandPermissionIssue::CommandArgumentTypeUsesRulesBasedAuthorization { .. } => {
                false
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CommandPermissionError {}

impl ContextualError for CommandPermissionError {
    fn create_error_context(&self) -> Option<Context> {
        None
    }
}

/// The output of the command permissions stage.
pub struct CommandPermissionsOutput {
    pub permissions: IndexMap<Qualified<CommandName>, CommandWithPermissions>,
    pub issues: Vec<CommandPermissionIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum AllowOrDeny {
    Allow,
    Deny,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum CommandAuthorizationRule {
    // are we allowed to call this function?
    Access {
        condition: Option<ConditionHash>,
        allow_or_deny: AllowOrDeny,
    },
    // value for an argument preset. the last value wins where multiple items are used.
    ArgumentPresetValue {
        condition: Option<ConditionHash>,
        argument_name: ArgumentName,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
    },
    // boolean expression for an argument preset. if multiple items are provided for one argument
    // then we "and" them together
    ArgumentAuthPredicate {
        condition: Option<ConditionHash>,
        argument_name: ArgumentName,
        predicate: ModelPredicate,
    },
}
