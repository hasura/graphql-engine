use open_dds::{
    data_connector::DataConnectorName,
    plugins::{
        LifecyclePluginName, LifecyclePluginUrl, LifecyclePreNdcRequestPluginHookConfig,
        LifecyclePreNdcResponsePluginHookConfig, LifecyclePreResponsePluginHookConfigRequest,
        OnPluginFailure,
    },
};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

use crate::Qualified;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedLifecyclePreNdcRequestPluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: Qualified<LifecyclePluginName>,
    /// A list of data connectors that this plugin hook should be applied to.
    /// There can only be one plugin hook of this type per data connector.
    pub connectors: HashSet<Qualified<DataConnectorName>>,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreNdcRequestPluginHookConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedLifecyclePreNdcResponsePluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: Qualified<LifecyclePluginName>,
    /// A list of data connectors that this plugin hook should be applied to.
    /// There can only be one plugin hook of this type per data connector.
    pub connectors: HashSet<Qualified<DataConnectorName>>,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the lifecycle plugin hook.
    pub config: LifecyclePreNdcResponsePluginHookConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedLifecyclePreResponsePluginHooks {
    /// Synchronous hooks
    pub sync_hooks: Vec<ResolvedLifecyclePreResponseSyncPluginHook>,
    /// Asynchronous hooks
    pub async_hooks: Vec<ResolvedLifecyclePreResponseAsyncPluginHook>,
}

impl Default for ResolvedLifecyclePreResponsePluginHooks {
    fn default() -> Self {
        Self::new()
    }
}

impl ResolvedLifecyclePreResponsePluginHooks {
    pub fn new() -> Self {
        Self {
            sync_hooks: Vec::new(),
            async_hooks: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.sync_hooks.is_empty() && self.async_hooks.is_empty()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedLifecyclePreResponseSyncPluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the synchronous mode of the plugin hook.
    pub config: ResolvedLifecyclePreResponseSyncPluginHookConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedLifecyclePreResponseSyncPluginHookConfig {
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreResponsePluginHookConfigRequest,
    /// Configuration for the on-plugin-failure behavior of the plugin hook (default: fail).
    pub on_plugin_failure: OnPluginFailure,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedLifecyclePreResponseAsyncPluginHook {
    /// The name of the lifecycle plugin hook.
    pub name: LifecyclePluginName,
    /// The URL to access the lifecycle plugin hook.
    pub url: LifecyclePluginUrl,
    /// Configuration for the request to the lifecycle plugin hook.
    pub request: LifecyclePreResponsePluginHookConfigRequest,
}
