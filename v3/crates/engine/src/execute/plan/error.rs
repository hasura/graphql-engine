use open_dds::{relationships::RelationshipName, types::FieldName};
use thiserror::Error;
use tracing_util::TraceableError;

#[derive(Error, Debug)]
pub enum Error {
    #[error("internal: {0}")]
    Internal(#[from] InternalError),
}

impl TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            Self::Internal(_internal) => tracing_util::ErrorVisibility::Internal,
        }
    }
}

#[derive(Error, Debug)]
pub enum InternalError {
    #[error("Mapping for source column {source_column} already exists in the relationship {relationship_name}")]
    MappingExistsInRelationship {
        source_column: FieldName,
        relationship_name: RelationshipName,
    },

    #[error("remote relationships should have been handled separately")]
    RemoteRelationshipsAreNotSupported,

    #[error("generic error: {description}")]
    InternalGeneric { description: String },
}
