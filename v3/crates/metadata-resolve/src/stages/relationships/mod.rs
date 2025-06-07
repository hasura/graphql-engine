mod error;
mod types;
use super::type_permissions;
use crate::{helpers, types::subgraph::Qualified};
pub use error::RelationshipError;
use open_dds::{
    identifier::SubgraphName,
    relationships::{RelationshipName, RelationshipV1},
    types::CustomTypeName,
};
use std::collections::{BTreeMap, HashSet};
pub use types::{Relationship, Relationships};

// This stage only collects relationships and collects them by subgraph,
//  the `object_relationships` stage resolves them meaningfully in the context of their objects
pub fn resolve<'s>(
    metadata_accessor: &'s open_dds::accessor::MetadataAccessor,
    object_types_with_permissions: &type_permissions::ObjectTypesWithPermissions,
) -> Result<Relationships<'s>, Vec<RelationshipError>> {
    let mut relationships: BTreeMap<
        Qualified<CustomTypeName>,
        BTreeMap<RelationshipName, Relationship<'s>>,
    > = BTreeMap::new();

    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: relationship,
    } in &metadata_accessor.relationships
    {
        results.push(resolve_relationship(
            object_types_with_permissions,
            subgraph,
            relationship,
            metadata_accessor,
            &mut relationships,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| Relationships(relationships))
}

fn resolve_relationship<'s>(
    object_types_with_permissions: &type_permissions::ObjectTypesWithPermissions,
    subgraph: &SubgraphName,
    relationship: &'s RelationshipV1,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    relationships: &mut BTreeMap<
        Qualified<CustomTypeName>,
        BTreeMap<RelationshipName, Relationship<'s>>,
    >,
) -> Result<(), RelationshipError> {
    let qualified_type_name = Qualified::new(subgraph.clone(), relationship.source_type.clone());

    if !object_types_with_permissions.contains_key(&qualified_type_name) {
        return Err(RelationshipError::RelationshipDefinedOnUnknownType {
            relationship_name: relationship.name.clone(),
            object_type_name: qualified_type_name,
        });
    }

    // build up map of maps
    if let Some(existing_relationship) = relationships.get_mut(&qualified_type_name) {
        let result = existing_relationship.insert(
            relationship.name.clone(),
            mk_relationship(
                &metadata_accessor.flags,
                &metadata_accessor.subgraphs,
                relationship,
            ),
        );

        // explode if we find duplicates for a name
        if result.is_some() {
            return Err(RelationshipError::DuplicateRelationshipForType {
                relationship_name: relationship.name.clone(),
                object_type_name: qualified_type_name,
            });
        }
    } else {
        let mut inner_map = BTreeMap::new();
        inner_map.insert(
            relationship.name.clone(),
            mk_relationship(
                &metadata_accessor.flags,
                &metadata_accessor.subgraphs,
                relationship,
            ),
        );
        relationships.insert(qualified_type_name, inner_map);
    }

    Ok(())
}

fn mk_relationship<'s>(
    flags: &open_dds::flags::OpenDdFlags,
    known_subgraphs: &HashSet<SubgraphName>,
    relationship: &'s RelationshipV1,
) -> Relationship<'s> {
    // If we have allowed the usage of unknown subgraphs...
    if flags.contains(open_dds::flags::Flag::AllowPartialSupergraph) {
        let subgraph = helpers::relationship::get_target_subgraph(relationship);

        // ...and the subgraph is unknown
        if subgraph.is_some_and(|subgraph| !known_subgraphs.contains(&subgraph)) {
            // Record the relationship but as one to an unknown subgraph
            Relationship::RelationshipToUnknownSubgraph
        } else {
            Relationship::Relationship(relationship)
        }
    }
    // If we don't allow the usage of unknown subgraphs, we assume all used subgraphs
    // exist and therefore there are no "unknown subgraphs". When we actually resolve
    // the relationship if the target is to a missing subgraph, we will get an error then
    else {
        Relationship::Relationship(relationship)
    }
}
