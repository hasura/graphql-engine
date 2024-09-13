use std::sync::Arc;

use open_dds::plugins::LifecyclePluginHookPreParse;

#[derive(Debug, thiserror::Error)]
pub enum BuildError {
    #[error("invalid metadata: {0}")]
    InvalidMetadata(#[from] metadata_resolve::Error),
    #[error("unable to build schema: {0}")]
    UnableToBuildSchema(#[from] schema::Error),
}

/// The response from the build_schema function
pub struct BuildSchemaResponse {
    pub gds_schema: lang_graphql::schema::Schema<schema::GDS>,
    pub pre_parse_plugins: Vec<LifecyclePluginHookPreParse>,
    pub warnings: Vec<metadata_resolve::Warning>,
}

/// this function is used by Metadata Build Service
pub fn build_schema(
    metadata: open_dds::Metadata,
    metadata_resolve_configuration: &metadata_resolve::configuration::Configuration,
) -> Result<BuildSchemaResponse, BuildError> {
    let (resolved_metadata, warnings) =
        metadata_resolve::resolve(metadata, metadata_resolve_configuration)?;
    let pre_parse_plugins = resolved_metadata.pre_parse_plugins.clone();
    let gds = schema::GDS {
        metadata: Arc::new(resolved_metadata),
    };
    Ok(BuildSchemaResponse {
        gds_schema: gds.build_schema()?,
        pre_parse_plugins,
        warnings,
    })
}
