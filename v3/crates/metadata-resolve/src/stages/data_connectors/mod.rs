use crate::types::configuration::Configuration;
use crate::types::subgraph::Qualified;
mod error;
mod types;

pub use error::{
    DataConnectorError, DataConnectorIssue, NamedDataConnectorError, NamedDataConnectorIssue,
};
use std::collections::BTreeMap;
pub use types::{
    ArgumentPreset, ArgumentPresetValue, CommandsResponseConfig, DataConnectorCapabilities,
    DataConnectorContext, DataConnectorLink, DataConnectorSchema, DataConnectors,
    DataConnectorsOutput, NdcVersion,
};

/// Resolve data connectors.
pub fn resolve<'a>(
    metadata_accessor: &'a open_dds::accessor::MetadataAccessor,
    configuration: &Configuration,
) -> Result<types::DataConnectorsOutput<'a>, NamedDataConnectorError> {
    let mut data_connectors = BTreeMap::new();
    let mut issues = vec![];
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: data_connector,
    } in &metadata_accessor.data_connectors
    {
        let qualified_data_connector_name =
            Qualified::new(subgraph.clone(), data_connector.name.clone());

        let (data_connector_context, connector_issues) =
            types::DataConnectorContext::new(data_connector, &configuration.unstable_features)
                .map_err(|error| NamedDataConnectorError {
                    data_connector_name: qualified_data_connector_name.clone(),
                    error,
                })?;

        issues.extend(
            connector_issues
                .into_iter()
                .map(|issue| NamedDataConnectorIssue {
                    data_connector_name: qualified_data_connector_name.clone(),
                    issue,
                }),
        );

        if data_connectors
            .insert(
                qualified_data_connector_name.clone(),
                data_connector_context,
            )
            .is_some()
        {
            return Err(NamedDataConnectorError {
                data_connector_name: qualified_data_connector_name.clone(),
                error: DataConnectorError::DuplicateDataConnectorDefinition,
            });
        }
    }
    Ok(types::DataConnectorsOutput {
        data_connectors: types::DataConnectors(data_connectors),
        issues,
    })
}
