use metadata_resolve::Qualified;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use tracing_util::TraceableError;

use crate::ndc;

#[derive(Debug, thiserror::Error)]
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

#[derive(Debug, thiserror::Error)]
pub enum InternalError {
    #[error("Mapping for source column {source_column} already exists in the relationship {relationship_name}")]
    MappingExistsInRelationship {
        source_column: FieldName,
        relationship_name: RelationshipName,
    },

    #[error("Missing argument mapping to command {command_name} data connector source for argument {argument_name} used in relationship {relationship_name} on type {source_type}")]
    MissingArgumentMappingInCommandRelationship {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },

    #[error("remote relationships should have been handled separately")]
    RemoteRelationshipsAreNotSupported,

    #[error("generic error: {description}")]
    InternalGeneric { description: String },

    #[error("failed to serialise an Expression to JSON: {0}")]
    ExpressionSerializationError(serde_json::Error),

    #[error("error when downgrading ndc request: {0}")]
    NdcRequestDowngradeError(ndc::migration::NdcDowngradeError),
}
