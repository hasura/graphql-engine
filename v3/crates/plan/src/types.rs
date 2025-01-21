use metadata_resolve::Qualified;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, derive_more::Display)]
pub enum PlanError {
    Internal(String),   // equivalent to DataFusionError::Internal
    Permission(String), // equivalent to DataFusionError::Plan
    Relationship(RelationshipError),
    OrderBy(OrderByError),
    External(Box<dyn std::error::Error + Send + Sync>), //equivalent to DataFusionError::External
}

impl TraceableError for PlanError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::Internal(_internal) => ErrorVisibility::Internal,
            Self::Permission(_) | Self::External(_) => ErrorVisibility::User,
            Self::Relationship(relationship_error) => relationship_error.visibility(),
            Self::OrderBy(order_by_error) => order_by_error.visibility(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RelationshipError {
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
    #[error("{0}")]
    Other(String),
}

impl TraceableError for RelationshipError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OrderByError {
    #[error("Aggregate relationship {0} is not supported in order_by")]
    RelationshipAggregateNotSupported(RelationshipName),
    #[error("{0}")]
    RemoteRelationshipNotSupported(String),
    #[error("Nested order by is not supported: {0}")]
    NestedOrderByNotSupported(String),
    #[error("An internal error occurred in order_by: {0}")]
    Internal(String),
}

impl OrderByError {
    pub fn into_plan_error(self) -> PlanError {
        PlanError::OrderBy(self)
    }
}

impl TraceableError for OrderByError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::RelationshipAggregateNotSupported(_)
            | Self::NestedOrderByNotSupported(_)
            | Self::RemoteRelationshipNotSupported(_) => ErrorVisibility::User,
            Self::Internal(_) => ErrorVisibility::Internal,
        }
    }
}
