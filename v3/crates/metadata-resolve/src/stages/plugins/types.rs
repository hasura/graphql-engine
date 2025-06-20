use open_dds::{
    data_connector::DataConnectorName,
    plugins::{
        LifecyclePluginName, LifecyclePluginUrl, LifecyclePreNdcRequestPluginHookConfig,
        LifecyclePreNdcResponsePluginHookConfig,
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
