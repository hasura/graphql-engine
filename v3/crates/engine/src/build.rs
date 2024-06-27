#[derive(Debug, thiserror::Error)]
pub enum BuildError {
    #[error("invalid metadata: {0}")]
    InvalidMetadata(#[from] metadata_resolve::Error),
    #[error("unable to build schema: {0}")]
    UnableToBuildSchema(#[from] schema::Error),
}

pub fn build_schema(
    metadata: open_dds::Metadata,
    metadata_resolve_configuration: metadata_resolve::configuration::Configuration,
) -> Result<lang_graphql::schema::Schema<schema::GDS>, BuildError> {
    let resolved_metadata = metadata_resolve::resolve(metadata, metadata_resolve_configuration)?;
    let gds = schema::GDS {
        metadata: resolved_metadata,
    };
    Ok(gds.build_schema()?)
}
