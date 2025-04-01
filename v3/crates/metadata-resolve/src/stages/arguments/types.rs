use crate::stages::boolean_expressions;
use crate::types::error::ContextualError;
use open_dds::arguments::ArgumentName;

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
