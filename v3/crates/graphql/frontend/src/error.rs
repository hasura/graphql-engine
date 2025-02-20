use engine_types::ExposeInternalErrors;
use gql::http::GraphQLError;
use lang_graphql as gql;
use tracing_util::{ErrorVisibility, TraceableError};

/// Request errors are raised before execution of root fields begins.
/// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Request-errors>
#[derive(Debug, thiserror::Error)]
pub enum RequestError {
    #[error("parsing failed: {0}")]
    ParseFailure(#[from] gql::ast::spanning::Positioned<gql::parser::Error>),

    #[error("validation failed: {0}")]
    ValidationFailed(#[from] gql::validation::Error),

    #[error("{0}")]
    IRConversionError(#[from] graphql_ir::Error),

    #[error("{0}")]
    GraphQlPlanError(#[from] graphql_ir::PlanError),

    #[error("explain error: {0}")]
    ExplainError(String),
}

impl RequestError {
    pub fn to_graphql_error(&self, expose_internal_errors: ExposeInternalErrors) -> GraphQLError {
        let message = match (self, expose_internal_errors) {
            // Error messages for internal errors from IR conversion and Plan generations are masked.
            (
                Self::IRConversionError(graphql_ir::Error::Internal(_)),
                ExposeInternalErrors::Censor,
            ) => "internal error".into(),
            (e, _) => e.to_string(),
        };
        // We are using the visibility of the error to determine if it is an internal error or not. We are assuming that
        // if we are showing the error message to the user, it is something that they can fix on their end.
        let is_internal = self.visibility() == ErrorVisibility::Internal;
        GraphQLError {
            message,
            path: None,
            extensions: None,
            is_internal,
        }
    }
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::IRConversionError(ir_error) => ir_error.visibility(),
            Self::GraphQlPlanError(plan_error) => plan_error.visibility(),
            // Rest all errors are visible to users via traces
            Self::ParseFailure(_) | Self::ValidationFailed(_) | Self::ExplainError(_) => {
                ErrorVisibility::User
            }
        }
    }
}
