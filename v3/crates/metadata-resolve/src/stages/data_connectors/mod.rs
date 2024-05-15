use crate::types::error::Error;
use crate::types::subgraph::Qualified;

mod types;
use std::collections::BTreeMap;
pub use types::{
    ComparisonOperators, DataConnectorContext, DataConnectorCoreInfo, DataConnectorLink,
    DataConnectorSchema, DataConnectors,
};

/// Resolve data connectors.
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<types::DataConnectors, Error> {
    let mut data_connectors = BTreeMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: data_connector,
    } in &metadata_accessor.data_connectors
    {
        let qualified_data_connector_name =
            Qualified::new(subgraph.to_string(), data_connector.name.clone());

        if data_connectors
            .insert(
                qualified_data_connector_name.clone(),
                types::DataConnectorContext::new(data_connector)?,
            )
            .is_some()
        {
            return Err(Error::DuplicateDataConnectorDefinition {
                name: qualified_data_connector_name,
            });
        }
    }
    Ok(types::DataConnectors { data_connectors })
}
