use open_dds::plugins::LifecyclePluginHookV1;
use open_dds::plugins::{
    LifecyclePreParsePluginHook, LifecyclePreResponsePluginHook, LifecyclePreRoutePluginHook,
};
use serde::{Deserialize, Serialize};

use crate::Warning;

#[derive(Debug, thiserror::Error)]
pub enum PluginIssue {
    #[error("PreRoute plugin {0} ignored because pre_route_plugins is an unstable feature and is not enabled. To enable, add `enable-pre-route-plugins` to the `UNSTABLE_FEATURES`.")]
    PreRoutePluginIgnored(String),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct LifecyclePluginConfigs {
    pub pre_parse_plugins: Vec<LifecyclePreParsePluginHook>,
    pub pre_response_plugins: Vec<LifecyclePreResponsePluginHook>,
    pub pre_route_plugins: Vec<LifecyclePreRoutePluginHook>,
}

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    enable_pre_route_plugins: &bool,
) -> (LifecyclePluginConfigs, Vec<Warning>) {
    let mut pre_parse_plugins = Vec::new();
    let mut pre_response_plugins = Vec::new();
    let mut pre_route_plugins = Vec::new();

    let mut warnings = Vec::new();

    for plugin in &metadata_accessor.plugins {
        match &plugin.object {
            LifecyclePluginHookV1::Parse(plugin) => pre_parse_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::Response(plugin) => pre_response_plugins.push(plugin.clone()),
            LifecyclePluginHookV1::Route(plugin) => {
                if *enable_pre_route_plugins {
                    pre_route_plugins.push(plugin.clone());
                } else {
                    warnings.push(Warning::PluginIssue(PluginIssue::PreRoutePluginIgnored(
                        plugin.name.clone(),
                    )));
                }
            }
        }
    }

    (
        LifecyclePluginConfigs {
            pre_parse_plugins,
            pre_response_plugins,
            pre_route_plugins,
        },
        warnings,
    )
}
