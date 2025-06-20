use crate::Qualified;
use open_dds::data_connector::DataConnectorName;
use open_dds::plugins::LifecyclePluginName;

#[derive(Debug, thiserror::Error)]
pub enum PluginValidationError {
    #[error("Plugin {plugin_name} references unknown data connector {data_connector_name}")]
    UnknownDataConnector {
        plugin_name: Qualified<LifecyclePluginName>,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "Data connector {data_connector_name} is referenced by multiple {plugin_type} plugins: {plugin_name_a} and {plugin_name_b}"
    )]
    DuplicatePluginForDataConnector {
        plugin_type: String,
        data_connector_name: Qualified<DataConnectorName>,
        plugin_name_a: Qualified<LifecyclePluginName>,
        plugin_name_b: Qualified<LifecyclePluginName>,
    },
    #[error("Plugin {plugin_name} is defined more than once")]
    DuplicatePluginName {
        plugin_name: Qualified<LifecyclePluginName>,
    },
}
