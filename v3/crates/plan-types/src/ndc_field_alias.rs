use serde::Serialize;
use smol_str::SmolStr;
use std::borrow::Borrow;

/// A NDC field alias. Not quite the same as an OpenDD FieldName since there are
/// no character restrictions on the string itself
#[derive(Serialize, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display, PartialOrd, Ord)]
pub struct NdcFieldAlias(SmolStr);

impl NdcFieldAlias {
    pub fn new<T: AsRef<str>>(str: T) -> Self {
        NdcFieldAlias(SmolStr::new(str))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn inner(&self) -> &SmolStr {
        &self.0
    }

    pub fn into_inner(self) -> SmolStr {
        self.into()
    }
}

impl From<&str> for NdcFieldAlias {
    fn from(value: &str) -> Self {
        NdcFieldAlias::new(value)
    }
}

impl From<NdcFieldAlias> for SmolStr {
    fn from(value: NdcFieldAlias) -> Self {
        value.0
    }
}

impl From<NdcFieldAlias> for String {
    fn from(value: NdcFieldAlias) -> Self {
        value.0.as_str().to_owned()
    }
}

impl Borrow<str> for NdcFieldAlias {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<SmolStr> for NdcFieldAlias {
    fn borrow(&self) -> &SmolStr {
        &self.0
    }
}
