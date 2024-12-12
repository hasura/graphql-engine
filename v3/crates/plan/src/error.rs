//! this has been copied from `graphql_ir`. not quite sure where the cuts will live yet.
//!
use metadata_resolve::Qualified;
use open_dds::{
    relationships::RelationshipName,
    session_variables::SessionVariableName,
    types::{CustomTypeName, FieldName},
};
use serde_json as json;

#[allow(clippy::duplicated_attributes)] // suppress spurious warnings from Clippy
#[derive(thiserror::Error, Debug)]
pub enum InternalError {
    #[error("{0}")]
    Developer(#[from] InternalDeveloperError),
    #[error("{0}")]
    Engine(#[from] InternalEngineError),
}

#[derive(Debug, thiserror::Error)]
pub enum InternalDeveloperError {
    #[error("Required session variable not found in the request: {session_variable}")]
    MissingSessionVariable {
        session_variable: SessionVariableName,
    },

    #[error("The session variables {session_variable} is not encoded as a string. JSON-typed session variables are not supported unless you update your compatibility date")]
    VariableJsonNotSupported {
        session_variable: SessionVariableName,
    },

    #[error("Session variable {session_variable} value is of an unexpected type. Expected: {expected}, but found: {found}")]
    VariableTypeCast {
        session_variable: SessionVariableName,
        expected: String,
        found: String,
    },

    #[error("Typecasting session variable {session_variable} to an array is not supported. Update your compatibility date to enable JSON session variables")]
    VariableArrayTypeCastNotSupported {
        session_variable: SessionVariableName,
    },

    #[error("Expected session variable {session_variable} to be a valid JSON value, but encountered a JSON parsing error: {parse_error}")]
    VariableExpectedJson {
        session_variable: SessionVariableName,
        parse_error: serde_json::Error,
    },

    #[error("Type mapping not found for the type name {type_name:}")]
    TypeMappingNotFound {
        type_name: Qualified<CustomTypeName>,
    },

    #[error("Field mapping not found for the field {field_name:} of type {type_name:}")]
    FieldMappingNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("{0}")]
    RelationshipFieldMappingError(#[from] metadata_resolve::RelationshipFieldMappingError),

    #[error("Could not convert the provided header value to string as it contains non-visible ASCII characters")]
    IllegalCharactersInHeaderValue,

    #[error("The relationship '{relationship_name}' is from a nested object and cannot be used in a predicate")]
    NestedObjectRelationshipInPredicate { relationship_name: RelationshipName },
}

#[derive(Debug, thiserror::Error)]
pub enum InternalEngineError {
    #[error("serialization error: {0}")]
    SerializationError(#[from] json::Error),

    #[error("internal error during execution of argument presets: {description}")]
    ArgumentPresetExecution { description: String },

    #[error("internal error: {description}")]
    InternalGeneric { description: String },
}
