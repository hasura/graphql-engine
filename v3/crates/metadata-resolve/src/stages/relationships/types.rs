use super::RelationshipError;
use crate::types::subgraph::Qualified;
use open_dds::identifier::SubgraphName;
use open_dds::relationships::{RelationshipName, RelationshipTarget, RelationshipV1};
use open_dds::types::CustomTypeName;
use std::collections::BTreeMap;

// multiple relationships can share the same `RelationshipName` because they refer to different objects
// so we key them by name AND source object type
pub struct Relationships<'s>(
    pub BTreeMap<RelationshipName, BTreeMap<Qualified<CustomTypeName>, Relationship<'s>>>,
);

impl<'s> Relationships<'s> {
    pub fn get(
        &self,
        object_type_name: &Qualified<CustomTypeName>,
        relationship_name: &RelationshipName,
    ) -> Result<&Relationship<'s>, RelationshipError> {
        self.0
            .get(relationship_name)
            .and_then(|relationships| relationships.get(object_type_name))
            .ok_or_else(|| RelationshipError::RelationshipNotFound {
                relationship_name: relationship_name.clone(),
                object_type_name: object_type_name.clone(),
            })
    }
}

// a wrapper for the underlying metadata so we can write nice accessors
// we don't want any logic here, that should be saved for the proper `object_relationships`
// resolving stage
pub struct Relationship<'s>(pub &'s RelationshipV1);

impl<'s> Relationship<'s> {
    pub fn get_target_subgraph(&self) -> Option<SubgraphName> {
        match &self.0.target {
            RelationshipTarget::Model(model_target) => model_target.subgraph(),
            RelationshipTarget::Command(command_target) => command_target.subgraph(),
        }
    }
}
