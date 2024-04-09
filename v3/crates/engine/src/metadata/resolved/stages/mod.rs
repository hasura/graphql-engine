/// This is where we'll be moving explicit metadata resolve stages
pub mod graphql_config;

use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::metadata::{resolve_metadata, Metadata};

/// this is where we take the input metadata and attempt to resolve a working `Metadata` object
/// currently the bulk of this lives in the `resolve_metadata` function, we'll be slowly breaking
/// it up and moving it into steps here
pub fn resolve(metadata: open_dds::Metadata) -> Result<Metadata, Error> {
    let metadata_accessor: open_dds::accessor::MetadataAccessor =
        open_dds::accessor::MetadataAccessor::new(metadata);

    let graphql_config =
        graphql_config::resolve(&metadata_accessor.graphql_config, &metadata_accessor.flags)?;

    resolve_metadata(metadata_accessor, graphql_config)
}
