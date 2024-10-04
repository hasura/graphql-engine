use open_dds::relationships::RelationshipName;
use open_dds::types::CustomTypeName;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

/// The unique relationship name that is passed to NDC
// Relationship names needs to be unique across the IR. This is so that, the
// NDC can use these names to figure out what joins to use.
// A single "source type" can have only one relationship with a given name,
// hence the relationship name in the IR is a tuple between the source type
// and the relationship name.
// Relationship name = (source_type, relationship_name)
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    JsonSchema,
    PartialOrd,
    Ord,
)]
pub struct NdcRelationshipName(pub SmolStr);

impl NdcRelationshipName {
    pub fn new(
        source_type: &metadata_resolve::Qualified<CustomTypeName>,
        relationship_name: &RelationshipName,
    ) -> Self {
        let name = format!(
            "{}___{}__{}",
            source_type.subgraph,
            source_type.name,
            relationship_name.as_str()
        );
        NdcRelationshipName(SmolStr::new(name))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
