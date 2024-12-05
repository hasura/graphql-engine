mod catalog;
mod handler;
mod middleware;
mod parse;
mod process_response;
mod schema;
mod types;

// explicit exports
pub use catalog::Catalog;
pub use handler::handler_internal;
use metadata_resolve::{QualifiedBaseType, QualifiedTypeReference};
pub use middleware::rest_request_tracing_middleware;
use open_dds::relationships::RelationshipType;
pub use parse::ParseError;
pub use schema::{empty_schema, openapi_schema};
pub use types::{InternalError, ModelInfo, QueryResult, RequestError};

/// Helper function to convert a type reference to a relationship type.
pub(crate) fn type_reference_to_relationship_type(
    type_reference: &QualifiedTypeReference,
) -> RelationshipType {
    match type_reference.underlying_type {
        QualifiedBaseType::Named(_) => RelationshipType::Object,
        QualifiedBaseType::List(_) => RelationshipType::Array,
    }
}
