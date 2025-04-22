use crate::parse;
use hasura_authn_core::Role;
use metadata_resolve::Qualified;
use open_dds::{
    identifier::SubgraphName, models::ModelName, relationships::RelationshipType,
    types::CustomTypeName,
};
use std::collections::BTreeMap;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, Clone)]
pub enum Warning {
    Role { role: Role, warning: RoleWarning },
}

#[derive(Debug, Clone)]
pub enum RoleWarning {
    Model {
        model_name: Qualified<ModelName>,
        warning: ModelWarning,
    },
    ObjectType {
        object_type_name: Qualified<CustomTypeName>,
        warning: ObjectTypeWarning,
    },
}

// if we exclude something, let's say why
#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ObjectTypeWarning {
    NoObjectTypePermission {},
    NestedObjectNotFound {
        object_type_name: Qualified<CustomTypeName>,
    },
}

// if we exclude something, let's say why
#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ModelWarning {
    NoSelectPermission,
    NoObjectTypeFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    NoModelSource,
}

#[derive(Debug, derive_more::Display)]
pub enum RequestError {
    NotFound,
    BadRequest(String),
    InternalError(InternalError),
    PlanError(plan::PlanError),
    ExecuteError(execute::FieldError),
    ParseError(parse::ParseError),
}

impl RequestError {
    pub fn into_http_error(
        self,
        expose_internal_errors: engine_types::ExposeInternalErrors,
    ) -> JsonApiHttpError {
        let (status_code, message) = match self {
            RequestError::BadRequest(err) => (axum::http::StatusCode::BAD_REQUEST, err),
            RequestError::ParseError(err) => (axum::http::StatusCode::BAD_REQUEST, err.to_string()),
            RequestError::NotFound => (
                axum::http::StatusCode::NOT_FOUND,
                "invalid route or path".to_string(),
            ),
            RequestError::PlanError(plan::PlanError::Permission(_err)) => (
                axum::http::StatusCode::FORBIDDEN,
                "Access forbidden".to_string(), // need to decide how much
                                                // we tell the user, for
                                                // now default to nothing
            ),
            RequestError::InternalError(InternalError::EmptyQuerySet)
            | RequestError::PlanError(
                plan::PlanError::Internal(_)
                | plan::PlanError::InternalError(_)
                | plan::PlanError::Relationship(_)
                | plan::PlanError::OrderBy(_)
                | plan::PlanError::BooleanExpression(_)
                | plan::PlanError::ArgumentPresetExecutionError(_),
            ) => (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                "Internal error".to_string(),
            ),
            RequestError::ExecuteError(field_error) => (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                // Fetch the error message from the error response
                field_error
                    .to_error_response(expose_internal_errors)
                    .message,
            ),
        };
        JsonApiHttpError {
            status: status_code,
            error: message,
        }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum InternalError {
    EmptyQuerySet,
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// JSON:API error over HTTP. Readily convertible to an HTTP response
/// using `axum::response::IntoResponse`'s `.into_response()`.
pub struct JsonApiHttpError {
    pub status: axum::http::StatusCode,
    pub error: String,
}

impl JsonApiHttpError {
    // Converts the error into a JSON:API error document
    pub fn into_document_error(self) -> jsonapi_library::api::DocumentError {
        let jsonapi_error = jsonapi_library::api::JsonApiError {
            // The spec mandates the compulsory inclusion of status code
            // Ref: https://jsonapi.org/format/#error-objects
            status: Some(self.status.as_u16().to_string()),
            detail: Some(self.error),
            ..Default::default()
        };
        jsonapi_library::api::DocumentError {
            errors: vec![jsonapi_error],
            ..Default::default()
        }
    }

    pub fn from_middleware_error(error: engine_types::MiddlewareError) -> Self {
        let message = if error.is_internal {
            "Internal error".to_string()
        } else {
            error.message
        };
        Self {
            status: error.status,
            error: message,
        }
    }
}

impl axum::response::IntoResponse for JsonApiHttpError {
    fn into_response(self) -> axum::response::Response {
        (self.status, axum::Json(self.into_document_error())).into_response()
    }
}

/// Model related info derived from URI path
#[allow(dead_code)]
pub struct ModelInfo {
    pub subgraph: SubgraphName,
    pub name: ModelName,
    /// value of the unique identifier of the model.
    // TODO: this won't be a string always
    pub unique_identifier: Option<String>,
    /// path to a relationship; like `["artist", "albums", "tracks"]`
    pub relationship: Vec<String>,
}

/// A tree of relationships, used in processing of relationships in the JSON:API response creation
#[derive(Default)]
pub struct RelationshipTree {
    pub relationships: BTreeMap<String, RelationshipNode>,
}

pub struct RelationshipNode {
    pub object_type: Qualified<CustomTypeName>,
    pub relationship_type: RelationshipType,
    pub is_command_relationship: bool,
    pub nested: RelationshipTree,
}
