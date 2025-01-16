use open_dds::plugins::LifecyclePluginHookV1;
use open_dds::plugins::{
    LifecyclePreParsePluginHook, LifecyclePreResponsePluginHook, LifecyclePreRoutePluginHook,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct LifecyclePluginConfigs {
    pub pre_parse_plugins: Vec<LifecyclePreParsePluginHook>,
    pub pre_response_plugins: Vec<LifecyclePreResponsePluginHook>,
    pub pre_route_plugins: Vec<LifecyclePreRoutePluginHook>,
}

pub fn resolve(metadata_accessor: &open_dds::accessor::MetadataAccessor) -> LifecyclePluginConfigs {
    let mut pre_parse_plugins = Vec::new();
    let mut pre_response_plugins = Vec::new();
    let mut pre_route_plugins = Vec::new();

    for plugin in &metadata_accessor.plugins {
        match &plugin.object {
            LifecyclePluginHookV1::Parse(plugin) => pre_parse_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::Response(plugin) => pre_response_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::Route(plugin) => {
                pre_route_plugins.push(plugin.clone());
            }
        }
    }

    LifecyclePluginConfigs {
        pre_parse_plugins,
        pre_response_plugins,
        pre_route_plugins,
    }
}
