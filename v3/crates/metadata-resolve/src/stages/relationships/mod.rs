mod error;
mod types;
use crate::types::subgraph::Qualified;
pub use error::RelationshipError;
use open_dds::{relationships::RelationshipName, types::CustomTypeName};
use std::collections::BTreeMap;
pub use types::{Relationship, Relationships};

// This stage only collects relationships and collects them by subgraph,
//  the `object_relationships` stage resolves them meaningfully in the context of their objects
pub fn resolve<'s>(
    metadata_accessor: &'s open_dds::accessor::MetadataAccessor,
) -> Result<Relationships<'s>, RelationshipError> {
    let mut relationships: BTreeMap<
        RelationshipName,
        BTreeMap<Qualified<CustomTypeName>, Relationship<'s>>,
    > = BTreeMap::new();

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: relationship,
    } in &metadata_accessor.relationships
    {
        let qualified_type_name =
            Qualified::new(subgraph.clone(), relationship.source_type.clone());

        // build up map of maps
        if let Some(existing_relationship) = relationships.get_mut(&relationship.name) {
            let result = existing_relationship
                .insert(qualified_type_name.clone(), Relationship(relationship));

            // explode if we find duplicates for a type
            if result.is_some() {
                return Err(RelationshipError::DuplicateRelationshipForType {
                    relationship_name: relationship.name.clone(),
                    object_type_name: qualified_type_name,
                });
            }
        } else {
            let mut inner_map = BTreeMap::new();
            inner_map.insert(qualified_type_name, Relationship(relationship));
            relationships.insert(relationship.name.clone(), inner_map);
        };
    }

    Ok(Relationships(relationships))
}
