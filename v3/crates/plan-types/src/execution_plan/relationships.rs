use open_dds::data_connector::{CollectionName, DataConnectorColumnName};
use open_dds::relationships::RelationshipType;
use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Relationship {
    /// A mapping between columns on the source collection to columns on the target collection
    pub column_mapping: BTreeMap<DataConnectorColumnName, DataConnectorColumnName>,
    pub relationship_type: RelationshipType,
    /// The name of a collection
    pub target_collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, RelationshipArgument>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationshipArgument {
    Column { name: DataConnectorColumnName },
}
