use metadata_resolve::{self, serialize_qualified_btreemap, Qualified};
use open_dds::{
    relationships::{RelationshipName, RelationshipType},
    types::CustomTypeName,
};
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct LocalModelRelationshipInfo<'s> {
    pub relationship_name: &'s RelationshipName,
    pub relationship_type: &'s RelationshipType,
    pub source_type: &'s Qualified<CustomTypeName>,
    pub source_data_connector: &'s metadata_resolve::DataConnectorLink,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings:
        &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    pub target_source: &'s metadata_resolve::ModelTargetSource,
    pub target_type: &'s Qualified<CustomTypeName>,
    pub mappings: &'s Vec<metadata_resolve::RelationshipModelMapping>,
}
