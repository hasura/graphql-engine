use std::sync::Arc;

#[derive(Debug, thiserror::Error)]
pub enum BuildError {
    #[error("invalid metadata: {0}")]
    InvalidMetadata(#[from] metadata_resolve::Error),
    #[error("unable to build schema: {0}")]
    UnableToBuildSchema(#[from] schema::Error),
}

/// this function is used by Metadata Build Service
pub fn build_schema(
    metadata: open_dds::Metadata,
    metadata_resolve_configuration: metadata_resolve::configuration::Configuration,
) -> Result<
    (
        lang_graphql::schema::Schema<schema::GDS>,
        Vec<metadata_resolve::Warning>,
    ),
    BuildError,
> {
    let (resolved_metadata, warnings) =
        metadata_resolve::resolve(metadata, metadata_resolve_configuration)?;
    let gds = schema::GDS {
        metadata: Arc::new(resolved_metadata),
    };
    Ok((gds.build_schema()?, warnings))
}
