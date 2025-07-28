mod error;
pub mod types;
use crate::Qualified;
pub use error::PluginValidationError;
use open_dds::data_connector::DataConnectorName;
use open_dds::plugins::{
    LifecyclePluginHookV1, LifecyclePluginName, LifecyclePreParsePluginHook,
    LifecyclePreResponsePluginHookMode, LifecyclePreRoutePluginHook,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use std::sync::Arc;
pub use types::{
    ResolvedLifecyclePreNdcRequestPluginHook, ResolvedLifecyclePreNdcResponsePluginHook,
    ResolvedLifecyclePreResponseAsyncPluginHook, ResolvedLifecyclePreResponsePluginHooks,
    ResolvedLifecyclePreResponseSyncPluginHook, ResolvedLifecyclePreResponseSyncPluginHookConfig,
};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct LifecyclePluginConfigs {
    pub pre_parse_plugins: Vec<LifecyclePreParsePluginHook>,
    pub pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks,
    pub pre_route_plugins: Vec<LifecyclePreRoutePluginHook>,
    pub pre_ndc_request_plugins:
        BTreeMap<Qualified<DataConnectorName>, Arc<ResolvedLifecyclePreNdcRequestPluginHook>>,
    pub pre_ndc_response_plugins:
        BTreeMap<Qualified<DataConnectorName>, Arc<ResolvedLifecyclePreNdcResponsePluginHook>>,
}

/// Resolves plugin configurations from metadata
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<LifecyclePluginConfigs, Vec<PluginValidationError>> {
    let mut pre_parse_plugins = Vec::new();
    let mut pre_response_async_plugins = Vec::new();
    let mut pre_response_sync_plugins = Vec::new();
    let mut pre_route_plugins = Vec::new();
    let mut pre_ndc_request_plugins = BTreeMap::new();
    let mut pre_ndc_response_plugins = BTreeMap::new();

    let mut validation_errors = Vec::new();

    // Track which plugin names have been seen
    let mut ndc_request_plugin_names: HashSet<Qualified<LifecyclePluginName>> = HashSet::new();
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
            LifecyclePluginHookV1::Response(plugin) => {
                match &plugin.config.mode {
                    // No mode specified, or asynchronous mode
                    None | Some(LifecyclePreResponsePluginHookMode::Asynchronous(_)) => {
                        pre_response_async_plugins.push(
                            ResolvedLifecyclePreResponseAsyncPluginHook {
                                name: plugin.name.clone(),
                                url: plugin.url.clone(),
                                request: plugin.config.request.clone(),
                            },
                        );
                    }
                    // Synchronous mode
                    Some(LifecyclePreResponsePluginHookMode::Synchronous(config)) => {
                        pre_response_sync_plugins.push(
                            ResolvedLifecyclePreResponseSyncPluginHook {
                                name: plugin.name.clone(),
                                url: plugin.url.clone(),
                                config: ResolvedLifecyclePreResponseSyncPluginHookConfig {
                                    request: plugin.config.request.clone(),
                                    on_plugin_failure: config.on_plugin_failure.clone(),
                                },
                            },
                        );
                    }
                }
            }
            LifecyclePluginHookV1::Route(plugin) => pre_route_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::NdcRequest(plugin) => {
                let qualified_plugin_name = Qualified::new(subgraph.clone(), plugin.name.clone());

                // Add the plugin name to the set of seen names
                if !ndc_request_plugin_names.insert(qualified_plugin_name.clone()) {
                    validation_errors.push(PluginValidationError::DuplicatePluginName {
                        plugin_name: qualified_plugin_name.clone(),
                    });
                }

                // build a set of connectors for this plugin. Duplicates will be silently dropped
                let connectors: HashSet<_> = plugin
                    .connectors
                    .iter()
                    .map(|connector| Qualified::new(subgraph.clone(), connector.clone()))
                    .collect();

                let resolved_plugin = Arc::new(ResolvedLifecyclePreNdcRequestPluginHook {
                    name: qualified_plugin_name.clone(),
                    connectors: connectors.clone(),
                    url: plugin.url.clone(),
                    config: plugin.config.clone(),
                });

                for connector in &connectors {
                    if !available_data_connectors.contains(connector) {
                        validation_errors.push(PluginValidationError::UnknownDataConnector {
                            plugin_name: qualified_plugin_name.clone(),
                            data_connector_name: connector.clone(),
                        });
                    }

                    // report an error if there are multiple plugins of the same type for a single connector
                    if let Some(plugin_b) =
                        pre_ndc_request_plugins.insert(connector.clone(), resolved_plugin.clone())
                    {
                        validation_errors.push(
                            PluginValidationError::DuplicatePluginForDataConnector {
                                plugin_type: "NdcRequest".to_string(),
                                data_connector_name: connector.clone(),
                                plugin_name_a: plugin_b.name.clone(),
                                plugin_name_b: qualified_plugin_name.clone(),
                            },
                        );
                    }
                }
            }
            LifecyclePluginHookV1::NdcResponse(plugin) => {
                let qualified_plugin_name = Qualified::new(subgraph.clone(), plugin.name.clone());

                // Add the plugin name to the set of seen names
                if !ndc_response_plugin_names.insert(qualified_plugin_name.clone()) {
                    validation_errors.push(PluginValidationError::DuplicatePluginName {
                        plugin_name: qualified_plugin_name.clone(),
                    });
                }

                let connectors: HashSet<_> = plugin
                    .connectors
                    .iter()
                    .map(|connector| Qualified::new(subgraph.clone(), connector.clone()))
                    .collect();

                let resolved_plugin = Arc::new(ResolvedLifecyclePreNdcResponsePluginHook {
                    name: qualified_plugin_name.clone(),
                    connectors: connectors.clone(),
                    url: plugin.url.clone(),
                    config: plugin.config.clone(),
                });

                for connector in &connectors {
                    if !available_data_connectors.contains(connector) {
                        validation_errors.push(PluginValidationError::UnknownDataConnector {
                            plugin_name: qualified_plugin_name.clone(),
                            data_connector_name: connector.clone(),
                        });
                    }

                    // report an error if there are multiple plugins of the same type for a single connector
                    if let Some(plugin_b) =
                        pre_ndc_response_plugins.insert(connector.clone(), resolved_plugin.clone())
                    {
                        validation_errors.push(
                            PluginValidationError::DuplicatePluginForDataConnector {
                                plugin_type: "NdcResponse".to_string(),
                                data_connector_name: connector.clone(),
                                plugin_name_a: plugin_b.name.clone(),
                                plugin_name_b: qualified_plugin_name.clone(),
                            },
                        );
                    }
                }
            }
        }
    }

    if validation_errors.is_empty() {
        Ok(LifecyclePluginConfigs {
            pre_parse_plugins,
            pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks {
                sync_hooks: pre_response_sync_plugins,
                async_hooks: pre_response_async_plugins,
            },
            pre_route_plugins,
            pre_ndc_request_plugins,
            pre_ndc_response_plugins,
        })
    } else {
        Err(validation_errors)
    }
}
