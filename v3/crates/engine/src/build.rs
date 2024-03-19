use crate::metadata;
use crate::schema;
use crate::schema::GDS;
use lang_graphql::schema as gql_schema;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("invalid metadata: {0}")]
    InvalidMetadata(#[from] metadata::resolved::error::Error),
    #[error("unable to build schema: {0}")]
    UnableToBuildSchema(#[from] schema::Error),
    #[error("unable to encode schema: {0}")]
    EncodingError(#[from] bincode::Error),
}

pub fn build_schema(metadata: open_dds::Metadata) -> Result<gql_schema::Schema<GDS>, BuildError> {
    let gds = schema::GDS::new(metadata)?;
    Ok(gds.build_schema()?)
}
