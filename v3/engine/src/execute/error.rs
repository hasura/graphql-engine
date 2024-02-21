use gql::{ast::common as ast, http::GraphQLError};
use lang_graphql as gql;
use open_dds::{
    relationships::RelationshipName,
    session_variables::SessionVariable,
    types::{CustomTypeName, FieldName},
};
use reqwest::{header::InvalidHeaderValue, StatusCode};
use serde_json as json;
use thiserror::Error;
use tracing_util::{ErrorVisibility, TraceableError};
use transitive::Transitive;

use crate::metadata::resolved::subgraph::Qualified;

use super::types::Annotation;

#[derive(Error, Debug)]
pub enum InternalDeveloperError {
    #[error("No source data connector specified for field {field_name} of type {type_name}")]
    NoSourceDataConnector {
        type_name: ast::TypeName,
        field_name: ast::Name,
    },

    #[error("No function/procedure specified for command field {field_name} of type {type_name}")]
    NoFunctionOrProcedure {
        type_name: ast::TypeName,
        field_name: ast::Name,
    },

    #[error("No argument source specified for argument {argument_name} of field {field_name}")]
    NoArgumentSource {
        field_name: ast::Name,
        argument_name: ast::Name,
    },

    #[error("Required session variable not found in the request: {session_variable}")]
    MissingSessionVariable { session_variable: SessionVariable },

    #[error("Unable to typecast session variable. Expected: {expected:}, but found: {found:}")]
    VariableTypeCast { expected: String, found: String },

    #[error("Typecasting to array is not supported.")]
    VariableArrayTypeCast,

    #[error("Mapping for the Global ID typename {type_name:} not found")]
    GlobalIDTypenameMappingNotFound { type_name: ast::TypeName },

    #[error("Type mapping not found for the type name {type_name:} while executing the relationship {relationship_name:}")]
    TypeMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error("Field mapping not found for the field {field_name:} of type {type_name:} while executing the relationship {relationship_name:}")]
    FieldMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        field_name: FieldName,
    },

    #[error("{}", render_ndc_error(.0))]
    GDCClientError(ndc_client::apis::Error),

    #[error("unexpected response from data connector: {summary}")]
    BadGDCResponse { summary: String },

    // Since explain and execute follow the same code-path. The types allow the following illegal responses. However,
    // these should never happen.
    #[error("illegal response: execute query returned explain response")]
    ExecuteReturnedExplainResponse,

    #[error("illegal response: explain query returned execute response")]
    ExplainReturnedExecuteResponse,
}

#[derive(Error, Debug)]
pub enum InternalEngineError {
    #[error("introspection error: {0}")]
    IntrospectionError(#[from] gql::introspection::Error),

    #[error("serialization error: {0}")]
    SerializationError(#[from] json::Error),

    #[error("IR conversion error: {0}")]
    IRConversionError(#[from] gql::normalized_ast::Error),

    #[error("unexpected annotation: {annotation}")]
    UnexpectedAnnotation { annotation: Annotation },

    #[error("subscription shouldn't have been validated")]
    SubscriptionsNotSupported,

    #[error("Mapping for source column {source_column} already exists in the relationship {relationship_name}")]
    MappingExistsInRelationship {
        source_column: FieldName,
        relationship_name: RelationshipName,
    },

    #[error("remote relationships should have been handled separately")]
    RemoteRelationshipsAreNotSupported,

    #[error("expected filter predicate but filter predicate namespaced annotation not found")]
    FilterPermissionAnnotationNotFound,

    #[error("expected namespace annotation type {namespace_annotation_type} but not found")]
    // Running into this error means that the GDS field was not annotated with the correct
    // namespace annotation while building the metadata.
    ExpectedNamespaceAnnotationNotFound { namespace_annotation_type: String },

    #[error("internal error: {description}")]
    InternalGeneric { description: String },
}

#[derive(Error, Debug, Transitive)]
#[transitive(from(json::Error, InternalEngineError))]
#[transitive(from(gql::normalized_ast::Error, InternalEngineError))]
#[transitive(from(gql::introspection::Error, InternalEngineError))]
pub enum InternalError {
    #[error("{0}")]
    Developer(#[from] InternalDeveloperError),
    #[error("{0}")]
    Engine(#[from] InternalEngineError),
}

impl InternalError {
    fn get_details(&self) -> Option<serde_json::Value> {
        match self {
            Self::Developer(InternalDeveloperError::GDCClientError(
                ndc_client::apis::Error::ConnectorError(ce),
            )) => Some(ce.error_response.details.clone()),
            _ => None,
        }
    }
}

#[derive(Error, Debug, Transitive)]
#[transitive(from(json::Error, InternalError))]
#[transitive(from(gql::normalized_ast::Error, InternalError))]
#[transitive(from(gql::introspection::Error, InternalError))]
#[transitive(from(InternalEngineError, InternalError))]
#[transitive(from(InternalDeveloperError, InternalError))]
pub enum Error {
    #[error("parsing failed: {0}")]
    ParseFailure(#[from] gql::ast::spanning::Positioned<gql::parser::Error>),
    #[error("validation failed: {0}")]
    ValidationFailed(#[from] gql::validation::Error),

    #[error("The global ID {encoded_value:} couldn't be decoded due to {decoding_error:}")]
    ErrorInDecodingGlobalId {
        encoded_value: String,
        decoding_error: String,
    },

    #[error("'{name:}' is not a valid GraphQL name.")]
    TypeFieldInvalidGraphQlName { name: String },

    #[error("ndc: {}", connector_error.error_response.message)]
    NDCExpected {
        connector_error: ndc_client::apis::ConnectorError,
    },
    #[error("{0}")]
    InternalError(#[from] InternalError),
    #[error("explain error: {0}")]
    ExplainError(String),
    #[error("invalid header value characters in project_id: {0}")]
    ProjectIdConversionError(InvalidHeaderValue),
}

impl Error {
    fn get_details(&self) -> Option<serde_json::Value> {
        match self {
            Error::InternalError(internal) => internal.get_details(),
            Error::NDCExpected { connector_error } => {
                Some(connector_error.error_response.details.clone())
            }
            _ => None,
        }
    }

    pub fn to_graphql_error(self, path: Option<Vec<gql::http::PathSegment>>) -> GraphQLError {
        let details = self.get_details();
        match self {
            Error::InternalError(_internal) => GraphQLError {
                message: "internal error".into(),
                path,
                extensions: None, // Internal errors showing up in the API response is not desirable. Hence, extensions are masked for internal errors
            },
            e => GraphQLError {
                message: e.to_string(),
                path,
                extensions: details.map(|details| gql::http::Extensions { details }),
            },
        }
    }
}

// Convert NDC errors
impl From<ndc_client::apis::Error> for Error {
    fn from(ndc_error: ndc_client::apis::Error) -> Error {
        if let ndc_client::apis::Error::ConnectorError(err) = &ndc_error {
            if matches!(
                err.status,
                // We forward the errors with status code 200 (OK), 403(FORBIDDEN), 409(CONFLICT) and 422(UNPROCESSABLE_ENTITY)
                StatusCode::OK
                    | StatusCode::FORBIDDEN
                    | StatusCode::CONFLICT
                    | StatusCode::UNPROCESSABLE_ENTITY
            ) {
                return Error::NDCExpected {
                    connector_error: err.clone(),
                };
            }
        }
        Error::InternalError(InternalError::Developer(
            InternalDeveloperError::GDCClientError(ndc_error),
        ))
    }
}

fn render_ndc_error(error: &ndc_client::apis::Error) -> String {
    match error {
        ndc_client::apis::Error::Reqwest(err) => match err.status() {
            Some(code) => format!("request to connector failed with status code {0}", code),
            None => format!("request to connector failed: {}", err),
        },
        ndc_client::apis::Error::Serde(err) => {
            format!("unable to decode JSON response from connector: {0}", err)
        }
        ndc_client::apis::Error::Io(_err) => "internal IO error".into(),
        ndc_client::apis::Error::ConnectorError(err) => format!(
            "connector returned status code {0} with message: {1}",
            err.status, err.error_response.message,
        ),
        ndc_client::apis::Error::InvalidBaseURL => "invalid connector base URL".to_string(),
    }
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Error::InternalError(internal) => match internal {
                InternalError::Developer(_) => ErrorVisibility::User,
                InternalError::Engine(_) => ErrorVisibility::Internal,
            },
            _ => ErrorVisibility::User,
        }
    }
}
