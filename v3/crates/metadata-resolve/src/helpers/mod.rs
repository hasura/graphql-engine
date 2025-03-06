//! Functions that are shared between metadata stages

use core::hash::Hash;
use std::collections::HashSet;

pub mod argument;
pub mod boolean_expression;
mod fancy_errors;
pub mod http;
pub mod ndc_validation;
pub mod relationship;
pub mod type_mappings;
pub mod type_validation;
pub mod typecheck;
pub mod types;

pub use fancy_errors::to_fancy_error;

/// Takes something that can be turned into an Iterator and then uses the `select_key` function
/// to extract a key value for each item in the iterable. It then detects if any these keys are
/// duplicated. If so, the first duplicate value is returned as the `Err` value.
pub fn check_for_duplicates<'a, TItem, TIter, TKey, FSelectKey>(
    items: TIter,
    select_key: FSelectKey,
) -> Result<(), &'a TItem>
where
    TIter: IntoIterator<Item = &'a TItem>,
    TKey: Eq + Hash,
    FSelectKey: Fn(&TItem) -> &TKey,
{
    let mut used_items = HashSet::<&TKey>::new();
    for item in items {
        if !used_items.insert(select_key(item)) {
            return Err(item);
        }
    }
    Ok(())
}
