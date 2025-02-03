use tracing_util::TraceableError;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Internal(#[from] InternalError),

    #[error("remote joins are not supported in subscriptions")]
    RemoteJoinsAreNotSupportedSubscriptions,

    #[error("remote predicates are not supported in mutations")]
    RemotePredicatesAreNotSupportedInMutations,

    #[error("planning returned mutation instead of expected query")]
    PlanExpectedQueryGotMutation,

    #[error("planning returned query instead of expected mutation")]
    PlanExpectedMutationGotQuery,

    #[error("{0}")]
    OpenDdPlanError(plan::PlanError),
}

impl From<plan::PlanError> for Error {
    fn from(plan_error: plan::PlanError) -> Error {
        Error::OpenDdPlanError(plan_error)
    }
}

impl TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            Self::Internal(_internal) => tracing_util::ErrorVisibility::Internal,
            Self::OpenDdPlanError(error) => error.visibility(),
            Self::RemoteJoinsAreNotSupportedSubscriptions => tracing_util::ErrorVisibility::User,
            Self::RemotePredicatesAreNotSupportedInMutations
            | Self::PlanExpectedMutationGotQuery
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
