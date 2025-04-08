use open_dds::{models::ModelName, types::FieldName};

use crate::{
    Qualified,
    stages::{boolean_expressions, graphql_config, models},
    types::error::ContextualError,
};

#[derive(Debug, thiserror::Error)]
pub enum ModelGraphqlError {
    #[error("unknown field {field_name:} in unique identifier defined for model {model_name:}")]
    UnknownFieldInUniqueIdentifier {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("duplicate field {field_name:} in unique identifier defined for model {model_name:}")]
    DuplicateFieldInUniqueIdentifier {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error(
        "filter input type name graphql configuration must be specified for model {model_name:} because aggregates are used with it"
    )]
    MissingFilterInputTypeNameGraphqlConfiguration { model_name: Qualified<ModelName> },

    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),
    #[error("{0}")]
    ModelsError(#[from] models::ModelsError),
    #[error("{0}")]
    BooleanExpressionError(#[from] boolean_expressions::BooleanExpressionError),
}

impl ContextualError for ModelGraphqlError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        match self {
            Self::ModelsError(error) => error.create_error_context(),
            Self::GraphqlConfigError(error) => error.create_error_context(),
            Self::BooleanExpressionError(error) => error.create_error_context(),
            _ => None,
        }
    }
}
