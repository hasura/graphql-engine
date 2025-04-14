use crate::Qualified;
use crate::stages::boolean_expressions;
use crate::types::error::ContextualError;
use open_dds::{
    arguments::ArgumentName, commands::CommandName, models::ModelName, types::CustomTypeName,
};

#[derive(Debug, Clone, thiserror::Error)]
pub enum ArgumentSource {
    #[error("model '{0}'")]
    Model(Qualified<ModelName>),
    #[error("command '{0}'")]
    Command(Qualified<CommandName>),
}

#[derive(Debug, thiserror::Error)]
#[error("argument '{argument_name}' in {source} has an error: {error}")]
pub struct NamedArgumentError {
    pub source: ArgumentSource,
    pub argument_name: ArgumentName,
    pub error: ArgumentError,
}

impl ContextualError for NamedArgumentError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        self.error.create_error_context()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ArgumentError {
    #[error("{0}")]
    BooleanExpressionError(#[from] boolean_expressions::BooleanExpressionError),
    #[error("Unknown type: {type_name}")]
    UnknownType {
        type_name: Qualified<CustomTypeName>,
    },
}

impl ContextualError for ArgumentError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        match self {
            Self::BooleanExpressionError(error) => error.create_error_context(),
            Self::UnknownType { .. } => None,
        }
    }
}
