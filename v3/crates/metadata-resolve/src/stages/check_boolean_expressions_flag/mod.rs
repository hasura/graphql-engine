/// this is where we will resolve graphql configuration
use crate::types::error::{BooleanExpressionError, Error};
use crate::types::internal_flags::MetadataResolveFlagsInternal;

/// For now, a very simple resolve step that checks for the `enable_boolean_expression_types` flag,
/// and if it's false, and we see a `BooleanExpressionType` kind, we explode
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    flags: &MetadataResolveFlagsInternal,
) -> Result<(), Error> {
    if !flags.enable_boolean_expression_types
        && !metadata_accessor.boolean_expression_types.is_empty()
    {
        Err(Error::BooleanExpressionError {
            boolean_expression_error: BooleanExpressionError::NewBooleanExpressionTypesAreDisabled,
        })
    } else {
        Ok(())
    }
}
