use std::collections::BTreeMap;

use crate::stages::boolean_expressions;
use crate::types::error::ContextualError;
use crate::types::subgraph::ArgumentKind;
use crate::{Qualified, QualifiedTypeReference};
use indexmap::IndexMap;
use open_dds::arguments::ArgumentName;
use open_dds::commands::CommandName;
use open_dds::models::ModelName;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, thiserror::Error, Eq, Ord, PartialEq, PartialOrd)]
pub enum ArgumentSource {
    #[error("model '{0}'")]
    Model(Qualified<ModelName>),
    #[error("command '{0}'")]
    Command(Qualified<CommandName>),
}

pub struct ArgumentsOutput {
    pub arguments: BTreeMap<ArgumentSource, IndexMap<ArgumentName, ArgumentInfo>>,
    pub issues: Vec<ArgumentIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: QualifiedTypeReference,
    pub description: Option<String>,
    pub argument_kind: ArgumentKind,
    pub type_representation: Option<ndc_models::TypeRepresentation>,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum ArgumentIssue {
    #[error("argument {argument_name:?} has an issue: {issue:?}")]
    BooleanExpressionIssue {
        argument_name: ArgumentName,
        issue: boolean_expressions::BooleanExpressionIssue,
    },
}

impl ContextualError for ArgumentIssue {
    fn create_error_context(&self) -> Option<error_context::Context> {
        match self {
            ArgumentIssue::BooleanExpressionIssue {
                argument_name: _,
                issue,
            } => issue.create_error_context(),
        }
    }
}
