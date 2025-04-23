use crate::types::error::ContextualError;
use crate::types::subgraph::Qualified;
use open_dds::relationships::RelationshipName;
use open_dds::types::CustomTypeName;

#[derive(Debug, thiserror::Error)]
pub enum RelationshipError {
    #[error("Relationship {relationship_name} could not be found for type {object_type_name}")]
    RelationshipNotFound {
        object_type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error("Multiple relationships named {relationship_name} defined for type {object_type_name}")]
    DuplicateRelationshipForType {
        object_type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error(
        "Source type {object_type_name} referenced in the definition of relationship {relationship_name} is not defined "
    )]
    RelationshipDefinedOnUnknownType {
        relationship_name: RelationshipName,
        object_type_name: Qualified<CustomTypeName>,
    },
}

impl ContextualError for RelationshipError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        None
    }
}
