use tracing_util::TraceableError;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("internal: {0}")]
    Internal(#[from] InternalError),

    #[error("remote joins are not supported in subscriptions")]
    RemoteJoinsAreNotSupportedSubscriptions,

    #[error("remote predicates are not supported in mutations")]
    RemotePredicatesAreNotSupportedInMutations,

    #[error("planning returned mutation instead of query")]
    PlanExpectedQueryGotMutation,
}

impl TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            Self::Internal(_internal) => tracing_util::ErrorVisibility::Internal,
            Self::RemoteJoinsAreNotSupportedSubscriptions => tracing_util::ErrorVisibility::User,
            Self::RemotePredicatesAreNotSupportedInMutations
            | Self::PlanExpectedQueryGotMutation => tracing_util::ErrorVisibility::Internal,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InternalError {
    #[error("remote relationships should have been handled separately")]
    RemoteRelationshipsAreNotSupported,

    #[error("generic error: {description}")]
    InternalGeneric { description: String },
}
