use pre_execution_plugin::configuration::PrePluginConfig;
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
/// Definition of the Pre-execution Plugin configuration used by the API server.
enum PreExecutionPluginConfiguration {
    V1(PrePluginConfig),
}

pub fn read_pre_execution_plugins_config(
    path: &Option<std::path::PathBuf>,
) -> Result<Vec<PrePluginConfig>, anyhow::Error> {
    let pre_plugins: Vec<PreExecutionPluginConfiguration> = match path {
        Some(path) => {
            let raw_pre_execution_plugins_config = std::fs::read_to_string(path)?;
            Ok::<_, anyhow::Error>(serde_json::from_str(&raw_pre_execution_plugins_config)?)
        }
        None => Ok(vec![]),
    }?;
    Ok(pre_plugins
        .into_iter()
        .map(|p| match p {
            PreExecutionPluginConfiguration::V1(config) => config,
        })
        .collect())
}
