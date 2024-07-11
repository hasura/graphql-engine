use gql::{ast::common as ast, http::GraphQLError};
use lang_graphql as gql;
use reqwest::StatusCode;
use serde_json as json;
use thiserror::Error;
use tracing_util::{ErrorVisibility, TraceableError};
use transitive::Transitive;

use crate::ndc::client as ndc_client;

use super::ir;
use super::plan;
use schema::Annotation;

/// Request errors are raised before execution of root fields begins.
/// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Request-errors>
#[derive(Debug, thiserror::Error)]
pub enum RequestError {
    #[error("parsing failed: {0}")]
    ParseFailure(#[from] gql::ast::spanning::Positioned<gql::parser::Error>),

    #[error("validation failed: {0}")]
    ValidationFailed(#[from] gql::validation::Error),

    #[error("{0}")]
    IRConversionError(#[from] ir::error::Error),

    #[error("error while generating plan: {0}")]
    PlanError(#[from] plan::error::Error),

    #[error("explain error: {0}")]
    ExplainError(String),
}

impl RequestError {
    pub fn to_graphql_error(
        &self,
        expose_internal_errors: crate::ExposeInternalErrors,
    ) -> GraphQLError {
        let message = match (self, expose_internal_errors) {
            // Error messages for internal errors from IR conversion and Plan generations are masked.
            (
                Self::IRConversionError(ir::error::Error::Internal(_))
                | Self::PlanError(plan::error::Error::Internal(_)),
                crate::ExposeInternalErrors::Censor,
            ) => "internal error".into(),
            (e, _) => e.to_string(),
        };
        GraphQLError {
            message,
            path: None,
            extensions: None,
        }
    }
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::IRConversionError(ir_error) => ir_error.visibility(),
            Self::PlanError(plan_error) => plan_error.visibility(),
            // Rest all errors are visible to users via traces
            Self::ParseFailure(_) | Self::ValidationFailed(_) | Self::ExplainError(_) => {
                ErrorVisibility::User
            }
        }
    }
}

/// Field errors are raised during execution from a root field
/// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Field-errors>
#[allow(clippy::duplicated_attributes)] // suppress spurious warnings from Clippy
#[derive(Error, Debug, Transitive)]
#[transitive(from(json::Error, FieldInternalError))]
#[transitive(from(NDCUnexpectedError, FieldInternalError))]
#[transitive(from(gql::normalized_ast::Error, FieldInternalError))]
#[transitive(from(gql::introspection::Error, FieldInternalError))]
pub enum FieldError {
    #[error("error from data source: {}", connector_error.error_response.message())]
    NDCExpected {
        connector_error: ndc_client::ConnectorError,
    },

    #[error("field '{field_name:} not found in _Service")]
    FieldNotFoundInService { field_name: String },

    #[error("internal error: {0}")]
    InternalError(#[from] FieldInternalError),
}

impl FieldError {
    fn get_details(&self) -> Option<serde_json::Value> {
        match self {
            Self::NDCExpected { connector_error } => {
                Some(connector_error.error_response.details().clone())
            }
            Self::InternalError(internal) => internal.get_details(),
            Self::FieldNotFoundInService { .. } => None,
        }
    }

    pub fn to_graphql_error(
        &self,
        expose_internal_errors: crate::ExposeInternalErrors,
        path: Option<Vec<gql::http::PathSegment>>,
    ) -> GraphQLError {
        let details = self.get_details();
        match (self, expose_internal_errors) {
            (Self::InternalError(_internal), crate::ExposeInternalErrors::Censor) => GraphQLError {
                message: "internal error".into(),
                path,
                // Internal errors showing up in the API response is not desirable.
                // Hence, extensions are masked for internal errors.
                extensions: None,
            },
            (e, _) => GraphQLError {
                message: e.to_string(),
                path,
                extensions: details.map(|details| gql::http::Extensions { details }),
            },
        }
    }
}

impl TraceableError for FieldError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::NDCExpected { .. } | Self::FieldNotFoundInService { .. } => ErrorVisibility::User,
            Self::InternalError(internal_error) => internal_error.visibility(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FieldInternalError {
    #[error("ndc_unexpected: {0}")]
    NDCUnexpected(#[from] NDCUnexpectedError),

    #[error("Mapping for the Global ID typename {type_name:} not found")]
    GlobalIdTypenameMappingNotFound { type_name: ast::TypeName },

    #[error("introspection error: {0}")]
    IntrospectionError(#[from] gql::introspection::Error),

    #[error("error from normalized AST: {0}")]
    NormalizedAstError(#[from] gql::normalized_ast::Error),

    #[error("JSON serialization error: {0}")]
    JsonSerialization(#[from] json::Error),

    #[error("unexpected annotation: {annotation}")]
    UnexpectedAnnotation { annotation: Annotation },

    #[error("internal error: {description}")]
    InternalGeneric { description: String },
}

impl FieldInternalError {
    pub fn get_details(&self) -> Option<serde_json::Value> {
        match self {
            Self::NDCUnexpected(ndc_unexpected) => ndc_unexpected.get_details(),
            _ => None,
        }
    }
}

impl TraceableError for FieldInternalError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::NDCUnexpected(_) | Self::GlobalIdTypenameMappingNotFound { .. } => {
                ErrorVisibility::User
            }
            Self::UnexpectedAnnotation { .. }
            | Self::JsonSerialization(_)
            | Self::InternalGeneric { .. }
            | Self::NormalizedAstError(_)
            | Self::IntrospectionError(_) => ErrorVisibility::Internal,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum NDCUnexpectedError {
    #[error("ndc_client error: {0}")]
    NDCClientError(ndc_client::Error),

    #[error("unexpected response from data connector: {summary}")]
    BadNDCResponse { summary: String },
}

impl NDCUnexpectedError {
    pub fn get_details(&self) -> Option<serde_json::Value> {
        match self {
            Self::NDCClientError(ndc_client::Error::Connector(ce)) => {
                Some(ce.error_response.details().clone())
            }
            _ => None,
        }
    }
}

// Convert NDC errors
impl From<ndc_client::Error> for FieldError {
    fn from(ndc_client_error: ndc_client::Error) -> FieldError {
        if let ndc_client::Error::Connector(err) = &ndc_client_error {
            if matches!(
                err.status,
                // We forward the errors with status code 200 (OK), 403(FORBIDDEN), 409(CONFLICT) and 422(UNPROCESSABLE_ENTITY)
                StatusCode::OK
                    | StatusCode::FORBIDDEN
                    | StatusCode::CONFLICT
                    | StatusCode::UNPROCESSABLE_ENTITY
            ) {
                return FieldError::NDCExpected {
                    connector_error: err.clone(),
                };
            }
        }
        FieldError::InternalError(FieldInternalError::NDCUnexpected(
            NDCUnexpectedError::NDCClientError(ndc_client_error),
        ))
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Query usage analytics encoding failed: {0}")]
/// Error occurs while generating query usage analytics JSON.
/// Wraps JSON encoding error, the only error currently encountered.
pub struct QueryUsageAnalyzeError(#[from] serde_json::Error);

impl TraceableError for QueryUsageAnalyzeError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}
