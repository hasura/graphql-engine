use std::collections::HashMap;
use std::hash::Hash;

/// A trait for setting default-ish values to be stripped and re-constituted during serialization
/// and deserialization (respectively) to save serialization time and artifact size, using the
/// attributes here:
///    <https://serde.rs/field-attrs.html#field-attributes>
///
/// We choose not to use Default at all because 1) the defaults we want for serialization might not
/// be the same, 2) we need to be able to review that, indeed, we have not broken our
/// serialization/deserialization roundtrip, and 3) Default offers no way to define an efficient
/// `is_default()`
///
/// Sadly serde doesn't support anything more declarative yet:
///    <https://github.com/serde-rs/serde/issues/2762>
///
/// Must satisfy `is_ser_default(ser_default())`
pub trait HasDefaultForSerde: Sized + PartialEq {
    // This can't simply be a static value unfortunately, since it might be polymorphic, like
    // HashMap.
    fn ser_default() -> Self;

    /// This should be overriden for performance to .is_empty() or the like if such a method exists
    fn is_ser_default(&self) -> bool {
        *self == Self::ser_default()
    }
}

pub fn ser_default<T: HasDefaultForSerde>() -> T {
    HasDefaultForSerde::ser_default()
}

#[inline]
pub fn is_ser_default<T: HasDefaultForSerde>(value: &T) -> bool {
    value.is_ser_default()
}

impl HasDefaultForSerde for bool {
    fn ser_default() -> Self {
        false
    }
    fn is_ser_default(&self) -> bool {
        !self
    }
}

impl<K, V> HasDefaultForSerde for HashMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    fn ser_default() -> Self {
        HashMap::new()
    }
    fn is_ser_default(&self) -> bool {
        self.is_empty()
    }
}

impl<K, V> HasDefaultForSerde for std::collections::BTreeMap<K, V>
where
    K: Eq,
    V: PartialEq,
{
    fn ser_default() -> Self {
        std::collections::BTreeMap::new()
    }
    fn is_ser_default(&self) -> bool {
        self.is_empty()
    }
}

impl<K, V> HasDefaultForSerde for indexmap::IndexMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    fn ser_default() -> Self {
        indexmap::IndexMap::new()
    }
    fn is_ser_default(&self) -> bool {
        self.is_empty()
    }
}

impl<V> HasDefaultForSerde for Option<V>
where
    V: PartialEq + Sized,
{
    fn ser_default() -> Self {
        None
    }
    fn is_ser_default(&self) -> bool {
        self.is_none()
    }
}

impl<V> HasDefaultForSerde for Vec<V>
where
    V: PartialEq + Sized,
{
    fn ser_default() -> Self {
        Vec::with_capacity(0)
    }
    fn is_ser_default(&self) -> bool {
        self.is_empty()
    }
}
