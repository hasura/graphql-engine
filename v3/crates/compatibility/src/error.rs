use super::compatibility_date::CompatibilityDate;

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum CompatibilityError {
    #[error("no compatibility config found")]
    NoCompatibilityConfigFound,
    #[error("duplicate compatibility config found")]
    DuplicateCompatibilityConfig,
    #[error("compatibility date {specified} is too old, oldest supported date is {oldest}")]
    DateTooOld {
        specified: CompatibilityDate,
        oldest: CompatibilityDate,
    },
    #[error("compatibility date {0} is in the future")]
    DateInTheFuture(CompatibilityDate),
}
