mod error;
pub mod types;
use crate::Qualified;
pub use error::PluginValidationError;
use open_dds::data_connector::DataConnectorName;
use open_dds::identifier::SubgraphName;
use open_dds::plugins::LifecyclePluginHookV1;
use open_dds::plugins::{
    LifecyclePluginName, LifecyclePreParsePluginHook, LifecyclePreResponsePluginHook,
    LifecyclePreRoutePluginHook,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use types::{ResolvedLifecyclePreNdcRequestPluginHook, ResolvedLifecyclePreNdcResponsePluginHook};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct LifecyclePluginConfigs {
    pub pre_parse_plugins: Vec<LifecyclePreParsePluginHook>,
    pub pre_response_plugins: Vec<LifecyclePreResponsePluginHook>,
    pub pre_route_plugins: Vec<LifecyclePreRoutePluginHook>,
    pub pre_ndc_request_plugins: Vec<ResolvedLifecyclePreNdcRequestPluginHook>,
    pub pre_ndc_response_plugins: Vec<ResolvedLifecyclePreNdcResponsePluginHook>,
}

/// Resolves plugin configurations from metadata
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<LifecyclePluginConfigs, Vec<PluginValidationError>> {
    let mut pre_parse_plugins = Vec::new();
    let mut pre_response_plugins = Vec::new();
    let mut pre_route_plugins = Vec::new();
    let mut pre_ndc_request_plugins = Vec::new();
    let mut pre_ndc_response_plugins = Vec::new();

    let mut validation_errors = Vec::new();

    // Track which data connectors are referenced by which plugins
    let mut ndc_request_plugin_connectors: BTreeMap<
        Qualified<DataConnectorName>,
        Qualified<LifecyclePluginName>,
    > = BTreeMap::new();
    let mut ndc_request_plugin_names: HashSet<Qualified<LifecyclePluginName>> = HashSet::new();
    let mut ndc_response_plugin_connectors: BTreeMap<
        Qualified<DataConnectorName>,
        Qualified<LifecyclePluginName>,
    > = BTreeMap::new();
    let mut ndc_response_plugin_names: HashSet<Qualified<LifecyclePluginName>> = HashSet::new();

    // Create a set of all available data connectors
    let available_data_connectors: HashSet<_> = metadata_accessor
        .data_connectors
        .iter()
        .map(|qualified_object| {
            Qualified::new(
                qualified_object.subgraph.clone(),
                qualified_object.object.name.clone(),
            )
        })
        .collect();

    // Process each plugin
    for plugin_obj in &metadata_accessor.plugins {
        let subgraph = &plugin_obj.subgraph;

        match &plugin_obj.object {
            LifecyclePluginHookV1::Parse(plugin) => pre_parse_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::Response(plugin) => pre_response_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::Route(plugin) => pre_route_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::NdcRequest(plugin) => {
                let qualified_plugin_name = Qualified::new(subgraph.clone(), plugin.name.clone());

                // Add the plugin name to the set of seen names
                if !ndc_request_plugin_names.insert(qualified_plugin_name.clone()) {
                    validation_errors.push(PluginValidationError::DuplicatePluginName {
                        plugin_name: qualified_plugin_name.clone(),
                    });
                }

                let connectors = resolve_ndc_plugin_connectors(
                    &plugin.connectors,
                    subgraph,
                    &qualified_plugin_name,
                    "NdcRequest",
                    &available_data_connectors,
                    &mut ndc_request_plugin_connectors,
                    &mut validation_errors,
                );

                // Create the resolved plugin hook
                pre_ndc_request_plugins.push(ResolvedLifecyclePreNdcRequestPluginHook {
                    name: qualified_plugin_name,
                    connectors,
                    url: plugin.url.clone(),
                    config: plugin.config.clone(),
                });
            }
            LifecyclePluginHookV1::NdcResponse(plugin) => {
                let qualified_plugin_name = Qualified::new(subgraph.clone(), plugin.name.clone());

                // Add the plugin name to the set of seen names
                if !ndc_response_plugin_names.insert(qualified_plugin_name.clone()) {
                    validation_errors.push(PluginValidationError::DuplicatePluginName {
                        plugin_name: qualified_plugin_name.clone(),
                    });
                }

                let connectors = resolve_ndc_plugin_connectors(
                    &plugin.connectors,
                    subgraph,
                    &qualified_plugin_name,
                    "NdcResponse",
                    &available_data_connectors,
                    &mut ndc_response_plugin_connectors,
                    &mut validation_errors,
                );

                // Create the resolved plugin hook
                pre_ndc_response_plugins.push(ResolvedLifecyclePreNdcResponsePluginHook {
                    name: qualified_plugin_name,
                    connectors,
                    url: plugin.url.clone(),
                    config: plugin.config.clone(),
                });
            }
        }
    }

    if validation_errors.is_empty() {
        Ok(LifecyclePluginConfigs {
            pre_parse_plugins,
            pre_response_plugins,
            pre_route_plugins,
            pre_ndc_request_plugins,
            pre_ndc_response_plugins,
        })
    } else {
        Err(validation_errors)
    }
}

/// Resolves and validates connectors for a ndc plugin config
/// Will append validation errors to the `validation_errors` vector if:
/// - a connector is referenced multiple times in the same plugin
/// - a connector is referenced by multiple plugins of the same type
/// - a connector is referenced but not defined
fn resolve_ndc_plugin_connectors(
    plugin_connectors: &Vec<DataConnectorName>,
    subgraph: &SubgraphName,
    qualified_plugin_name: &Qualified<LifecyclePluginName>,
    plugin_type: &str,
    available_data_connectors: &HashSet<Qualified<DataConnectorName>>,
    ndc_plugin_connectors: &mut BTreeMap<
        Qualified<DataConnectorName>,
        Qualified<LifecyclePluginName>,
    >,
    validation_errors: &mut Vec<PluginValidationError>,
) -> Vec<Qualified<DataConnectorName>> {
    let mut connectors = Vec::new();

    for connector in plugin_connectors {
        let qualified_connector_name = Qualified::new(subgraph.clone(), connector.clone());

        // Check for references to unknown data connectors
        if available_data_connectors.contains(&qualified_connector_name) {
            // ignore duplicate references to the same connector
            if connectors.contains(&qualified_connector_name) {
                validation_errors.push(PluginValidationError::DuplicateConnectorName {
                    plugin_name: qualified_plugin_name.clone(),
                    connector_name: connector.clone(),
                });
            } else {
                if let Some(plugin_a) =
                    // track we have a plugin of this type for this connector
                    ndc_plugin_connectors.insert(
                        qualified_connector_name.clone(),
                        qualified_plugin_name.clone(),
                    )
                {
                    // error if there is already another plugin of this type for this connector
                    validation_errors.push(
                        PluginValidationError::DuplicatePluginForDataConnector {
                            plugin_type: plugin_type.to_string(),
                            data_connector_name: qualified_connector_name.clone(),
                            plugin_name_a: plugin_a.clone(),
                            plugin_name_b: qualified_plugin_name.clone(),
                        },
                    );
                }

                connectors.push(qualified_connector_name);
            }
        } else {
            validation_errors.push(PluginValidationError::UnknownDataConnector {
                plugin_name: qualified_plugin_name.clone(),
                data_connector_name: qualified_connector_name.clone(),
            });
        }
    }

    connectors
}
