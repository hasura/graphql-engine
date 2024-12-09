use open_dds::{relationships::RelationshipName, types::FieldName};

#[derive(Debug, derive_more::Display)]
pub enum PlanError {
    Internal(String),   // equivalent to DataFusionError::Internal
    Permission(String), // equivalent to DataFusionError::Plan
    Relationship(RelationshipError),
    External(Box<dyn std::error::Error + Send + Sync>), //equivalent to DataFusionError::External
}

#[derive(Debug, thiserror::Error)]
pub enum RelationshipError {
    #[error("Mapping for source column {source_column} already exists in the relationship {relationship_name}")]
    MappingExistsInRelationship {
        source_column: FieldName,
        relationship_name: RelationshipName,
    },
    #[error("{0}")]
    Other(String),
}
