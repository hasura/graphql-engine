use open_dds::plugins::LifecyclePluginHookPreParse;
use open_dds::plugins::LifecyclePluginHookV1;

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Vec<LifecyclePluginHookPreParse> {
    let mut pre_parse_plugins = Vec::new();

    for plugin in &metadata_accessor.plugins {
        match &plugin.object {
            LifecyclePluginHookV1::Parse(plugin) => pre_parse_plugins.push(plugin.clone()),
        }
    }

    pre_parse_plugins
}
