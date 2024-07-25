use crate::types::configuration::Configuration;
use crate::types::subgraph::Qualified;
mod error;
mod types;

pub use error::DataConnectorError;
pub use error::NamedDataConnectorError;
use std::collections::BTreeMap;
pub use types::{
    ArgumentPreset, CommandsResponseConfig, DataConnectorCapabilities, DataConnectorContext,
    DataConnectorLink, DataConnectorSchema, DataConnectors, NdcVersion,
};

/// Resolve data connectors.
pub fn resolve<'a>(
    metadata_accessor: &'a open_dds::accessor::MetadataAccessor,
    configuration: &Configuration,
) -> Result<types::DataConnectors<'a>, NamedDataConnectorError> {
    let mut data_connectors = BTreeMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: data_connector,
    } in &metadata_accessor.data_connectors
    {
        let qualified_data_connector_name =
            Qualified::new(subgraph.to_string(), data_connector.name.clone());

        let data_conntext_context =
            types::DataConnectorContext::new(data_connector, &configuration.unstable_features)
                .map_err(|error| NamedDataConnectorError {
                    data_connector_name: qualified_data_connector_name.clone(),
                    error,
                })?;

        if data_connectors
            .insert(qualified_data_connector_name.clone(), data_conntext_context)
            .is_some()
        {
            return Err(NamedDataConnectorError {
                data_connector_name: qualified_data_connector_name.clone(),
                error: DataConnectorError::DuplicateDataConnectorDefinition,
            });
        }
    }
    Ok(types::DataConnectors(data_connectors))
}
