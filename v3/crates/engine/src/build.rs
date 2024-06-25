use lang_graphql::schema as gql_schema;
use metadata_resolve;
use schema;
use schema::GDS;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("invalid metadata: {0}")]
    InvalidMetadata(#[from] metadata_resolve::Error),
    #[error("unable to build schema: {0}")]
    UnableToBuildSchema(#[from] schema::Error),
}

pub fn build_schema(
    metadata: open_dds::Metadata,
    metadata_resolve_flags: &metadata_resolve::MetadataResolveFlagsInternal,
) -> Result<gql_schema::Schema<GDS>, BuildError> {
    let resolved_metadata = metadata_resolve::resolve(metadata, metadata_resolve_flags)?;
    let gds = schema::GDS {
        metadata: resolved_metadata,
    };
    Ok(gds.build_schema()?)
}
