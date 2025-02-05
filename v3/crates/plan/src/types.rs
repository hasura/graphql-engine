use crate::error::InternalError;
use hasura_authn_core::Role;
use metadata_resolve::Qualified;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
pub enum PlanError {
    #[error("{0}")]
    Internal(String), // equivalent to DataFusionError::Internal
    #[error("{0}")]
    Permission(#[from] PermissionError),
    #[error("{0}")]
    Relationship(#[from] RelationshipError),
    #[error("{0}")]
    OrderBy(#[from] OrderByError),
    #[error("{0}")]
    InternalError(InternalError),
    #[error("{0}")]
    External(Box<dyn std::error::Error + Send + Sync>), //equivalent to DataFusionError::External
}

impl TraceableError for PlanError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::InternalError(InternalError::Developer(_)) | Self::External(_) => {
                ErrorVisibility::User
            }
            Self::Permission(permission_error) => permission_error.visibility(),
            Self::Relationship(relationship_error) => relationship_error.visibility(),
            Self::OrderBy(order_by_error) => order_by_error.visibility(),
            Self::InternalError(InternalError::Engine(_)) | Self::Internal(_) => {
                ErrorVisibility::Internal
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum PermissionError {
    #[error("command {command_name:} could not be found")]
    CommandNotFound {
        command_name: Qualified<CommandName>,
    },
    #[error("role {role:} does not have permission to select from command {command_name:}")]
    CommandNotAccessible {
        command_name: Qualified<CommandName>,
        role: Role,
    },
    #[error("model {model_name:} could not be found")]
    ModelNotFound { model_name: Qualified<ModelName> },
    #[error("role {role:} does not have permission to select from model {model_name:}")]
    ModelNotAccessible {
        model_name: Qualified<ModelName>,
        role: Role,
    },
    #[error("object type {object_type_name:} could not be found")]
    ObjectTypeNotFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    #[error("role {role:} does not have permission to select from type {object_type_name:}")]
    ObjectTypeNotAccessible {
        object_type_name: Qualified<CustomTypeName>,
        role: Role,
    },
    #[error("role {role:} does not have permission to select from field {field_name:} in type {object_type_name:}")]
    ObjectFieldNotFound {
        object_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        role: Role,
    },
    #[error("{0}")]
    Other(String),
}

impl TraceableError for PermissionError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::ObjectTypeNotFound { .. }
            | Self::ObjectTypeNotAccessible { .. }
            | Self::ObjectFieldNotFound { .. }
            | Self::CommandNotFound { .. }
            | Self::CommandNotAccessible { .. }
            | Self::ModelNotFound { .. }
            | Self::ModelNotAccessible { .. } => ErrorVisibility::Internal,
            Self::Other(_) => ErrorVisibility::User,
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
    #[error("Missing NDC column name in relationship {relationship_name} in the mapping between source field {source_field} and target field {target_field}")]
    MissingTargetColumn {
        relationship_name: RelationshipName,
        source_field: FieldName,
        target_field: FieldName,
    },
    #[error("Procedure relationships are not supported: {relationship_name}")]
    ProcedureRelationshipsNotSupported { relationship_name: RelationshipName },
    #[error("{0}")]
    RelationshipFieldMappingError(#[from] metadata_resolve::RelationshipFieldMappingError),
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
