use engine_types::ExposeInternalErrors;
use gql::{ast::common as ast, http::GraphQLError};
use lang_graphql as gql;
use open_dds::relationships::RelationshipName;
use plan_types::RemotePredicateKey;
use serde_json as json;
use thiserror::Error;
use tracing_util::{ErrorVisibility, TraceableError};
use transitive::Transitive;

use crate::{execute, ndc::client as ndc_client};

use graphql_schema::Annotation;

/// An intermediate type to represent a field error response to be returned
/// to an API client (GraphQL or JSON:API)
pub struct FieldErrorResponse {
    pub message: String,
    pub details: Option<serde_json::Value>,
    pub is_internal: bool,
}

/// Field errors are raised during execution from a root field
/// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Field-errors>
#[allow(clippy::duplicated_attributes)] // suppress spurious warnings from Clippy
#[derive(Error, Debug, Transitive)]
#[transitive(from(json::Error, FieldInternalError))]
#[transitive(from(NDCUnexpectedError, FieldInternalError))]
#[transitive(from(gql::normalized_ast::Error, FieldInternalError))]
#[transitive(from(gql::introspection::Error, FieldInternalError))]
#[transitive(from(FilterPredicateError, FieldInternalError))]
pub enum FieldError {
    #[error("error from data source: {}", connector_error.error_response.message())]
    NDCExpected {
        connector_error: ndc_client::ConnectorError,
    },

    #[error("field '{field_name:} not found in _Service")]
    FieldNotFoundInService { field_name: String },

    #[error("subscription are not supported over HTTP")]
    SubscriptionsNotSupported,

    #[error(
        "Relationship '{name}' is either remote or not having 'relation_comparisons' NDC capability; not supported for filtering"
    )]
    RelationshipPredicatesNotSupported { name: RelationshipName },

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
            Self::FieldNotFoundInService { .. }
            | Self::SubscriptionsNotSupported
            | Self::RelationshipPredicatesNotSupported { .. } => None,
        }
    }

    pub fn to_error_response(
        &self,
        expose_internal_errors: ExposeInternalErrors,
    ) -> FieldErrorResponse {
        let details = self.get_details();
        match (self, expose_internal_errors) {
            (Self::InternalError(internal), ExposeInternalErrors::Censor) => FieldErrorResponse {
                message: "internal error".into(),
                details: None,
                is_internal: internal.is_engine_internal_error(),
            },
            (e, _) => FieldErrorResponse {
                message: e.to_string(),
                details,
                is_internal: false,
            },
        }
    }

    pub fn to_graphql_error(
        &self,
        expose_internal_errors: ExposeInternalErrors,
        path: Option<Vec<gql::http::PathSegment>>,
    ) -> GraphQLError {
        // Convert the error to an error response
        let FieldErrorResponse {
            message,
            details,
            is_internal,
        } = self.to_error_response(expose_internal_errors);
        // Build a GraphQL error from the error response
        GraphQLError {
            message,
            path,
            extensions: details.map(|details| gql::http::Extensions { details }),
            is_internal,
        }
    }
}

impl TraceableError for FieldError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::NDCExpected { .. }
            | Self::FieldNotFoundInService { .. }
            | Self::RelationshipPredicatesNotSupported { .. }
            | Self::SubscriptionsNotSupported => ErrorVisibility::User,
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

    #[error("unable to execute remote filter predicate: {0}")]
    UnableToResolveFilterPredicate(#[from] FilterPredicateError),

    #[error("failed to serialise an Expression to JSON: {0}")]
    ExpressionSerializationError(serde_json::Error),

    #[error("ndc compatibility error: {0}")]
    NdcV01CompatibilityError(#[from] execute::NdcV01CompatibilityError),

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

    pub fn is_engine_internal_error(&self) -> bool {
        !matches!(
            self,
            Self::NDCUnexpected(
                NDCUnexpectedError::BadNDCResponse { .. }
                    | NDCUnexpectedError::NDCClientError(
                        ndc_client::Error::InvalidConnector(_) | ndc_client::Error::Connector(_)
                    )
            )
        )
    }
}

impl TraceableError for FieldInternalError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::NDCUnexpected(_) | Self::GlobalIdTypenameMappingNotFound { .. } => {
                ErrorVisibility::User
            }
            Self::UnableToResolveFilterPredicate(filter_predicate_error) => {
                filter_predicate_error.visibility()
            }
            Self::UnexpectedAnnotation { .. }
            | Self::JsonSerialization(_)
            | Self::InternalGeneric { .. }
            | Self::NormalizedAstError(_)
            | Self::ExpressionSerializationError(_)
            | Self::IntrospectionError(_)
            | Self::NdcV01CompatibilityError(_) => ErrorVisibility::Internal,
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum FilterPredicateError {
    #[error("not a single row set returned from remote connector request: {0}")]
    NotASingleRowSet(String),

    #[error("could not find remote predicate result for {0}")]
    CouldNotFindRemotePredicate(RemotePredicateKey),
}

impl TraceableError for FilterPredicateError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::CouldNotFindRemotePredicate(_) | Self::NotASingleRowSet(_) => {
                ErrorVisibility::Internal
            }
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
        match ndc_client_error {
            // We forward the errors with status code 200 (OK), 403(FORBIDDEN), 409(CONFLICT) and 422(UNPROCESSABLE_ENTITY)
            ndc_client::Error::Connector(err)
                if matches!(
                    err.status,
                    reqwest::StatusCode::OK
                        | reqwest::StatusCode::FORBIDDEN
                        | reqwest::StatusCode::CONFLICT
                        | reqwest::StatusCode::UNPROCESSABLE_ENTITY
                ) =>
            {
                FieldError::NDCExpected {
                    connector_error: err,
                }
            }
            // Rest all errors are internal
            ndc_client_error => FieldError::InternalError(FieldInternalError::NDCUnexpected(
                NDCUnexpectedError::NDCClientError(ndc_client_error),
            )),
        }
    }
}
