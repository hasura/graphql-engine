use super::RelationshipError;
use crate::types::subgraph::Qualified;
use open_dds::relationships::{RelationshipName, RelationshipV1};
use open_dds::types::CustomTypeName;
use std::collections::BTreeMap;

// multiple relationships can share the same `RelationshipName` because they refer to different objects
// so we key them by source object type AND name
pub struct Relationships<'s>(
    pub BTreeMap<Qualified<CustomTypeName>, BTreeMap<RelationshipName, Relationship<'s>>>,
);

impl<'s> Relationships<'s> {
    pub fn get(
        &self,
        object_type_name: &Qualified<CustomTypeName>,
        relationship_name: &RelationshipName,
    ) -> Result<&Relationship<'s>, RelationshipError> {
        self.0
            .get(object_type_name)
            .and_then(|relationships| relationships.get(relationship_name))
            .ok_or_else(|| RelationshipError::RelationshipNotFound {
                relationship_name: relationship_name.clone(),
                object_type_name: object_type_name.clone(),
            })
    }

    pub fn get_relationships_for_type(
        &self,
        object_type_name: &Qualified<CustomTypeName>,
    ) -> &BTreeMap<RelationshipName, Relationship<'s>> {
        self.0.get(object_type_name).unwrap_or(&EMPTY_MAP)
    }
}

static EMPTY_MAP: BTreeMap<RelationshipName, Relationship<'static>> = BTreeMap::new();

#[derive(Debug)]
pub enum Relationship<'s> {
    /// Relationship that targets something in a known subgraph
    Relationship(&'s RelationshipV1),
    /// Relationship that targets something in an unknown subgraph.
    /// This is only used if we're allowing unknown subgraphs, and it is
    /// expected that this relationship is ignored and dropped
    RelationshipToUnknownSubgraph,
}
