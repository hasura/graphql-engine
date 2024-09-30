use std::sync::Arc;

#[derive(Debug, thiserror::Error)]
pub enum BuildError {
    #[error("invalid metadata: {0}")]
    InvalidMetadata(#[from] metadata_resolve::WithContext<metadata_resolve::Error>),
    #[error("unable to build schema: {0}")]
    UnableToBuildSchema(#[from] graphql_schema::Error),
}

/// The response from the build_schema function
pub struct BuildSchemaResponse {
    pub gds_schema: lang_graphql::schema::Schema<graphql_schema::GDS>,
    pub plugin_configs: metadata_resolve::LifecyclePluginConfigs,
    pub warnings: Vec<metadata_resolve::Warning>,
}

/// this function is used by Metadata Build Service
pub fn build_schema(
    metadata: open_dds::Metadata,
    metadata_resolve_configuration: &metadata_resolve::configuration::Configuration,
) -> Result<BuildSchemaResponse, BuildError> {
    let (resolved_metadata, warnings) =
        metadata_resolve::resolve(metadata, metadata_resolve_configuration)?;
    let plugin_configs = resolved_metadata.plugin_configs.clone();
    let gds = graphql_schema::GDS {
        metadata: Arc::new(resolved_metadata),
    };
    Ok(BuildSchemaResponse {
        gds_schema: gds.build_schema()?,
        plugin_configs,
        warnings,
    })
}
