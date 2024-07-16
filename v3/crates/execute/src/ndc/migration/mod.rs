pub mod v01;

#[derive(Debug, thiserror::Error)]
pub enum NdcDowngradeError {
    #[error("Aggregate comparisons are not supported in ndc v0.1.x")]
    AggregateComparisonsNotSupportedInV01,
    #[error("Scopes in expression comparison values are not supported in ndc v0.1.x")]
    ComparisonValueScopesNotSupportedInV01,
    #[error("Ordering by an aggregate column count is not supported in ndc v0.1.x")]
    OrderByAggregateColumnCountNotSupportedInV01,
    #[error("Nested field collections are not supported in ndc v0.1.x")]
    NestedFieldCollectionsNotSupportedInV01,
}
