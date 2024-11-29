use metadata_resolve::Qualified;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use tracing_util::TraceableError;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("internal: {0}")]
    Internal(#[from] InternalError),

    #[error("remote joins are not supported in subscriptions")]
    RemoteJoinsAreNotSupportedSubscriptions,

    #[error("remote predicates are not supported in mutations")]
    RemotePredicatesAreNotSupportedInMutations,
}

impl TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            Self::Internal(_internal) => tracing_util::ErrorVisibility::Internal,
            Self::RemoteJoinsAreNotSupportedSubscriptions => tracing_util::ErrorVisibility::User,
            Self::RemotePredicatesAreNotSupportedInMutations => {
                tracing_util::ErrorVisibility::Internal
            }
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
}
