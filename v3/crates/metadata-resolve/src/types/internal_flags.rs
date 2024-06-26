#[derive(Debug, Clone, Copy, Default)]
/// internal feature flags used in metadata resolve steps
pub struct MetadataResolveFlagsInternal {
    pub enable_boolean_expression_types: bool,
    pub enable_aggregate_relationships: bool,
}
