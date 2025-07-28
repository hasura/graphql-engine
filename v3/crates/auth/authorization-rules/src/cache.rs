use std::collections::BTreeMap;

use metadata_resolve::ConditionHash;

// to save us doing this stuff over and over again, we
// store the results of evaluation as we go
pub struct ConditionCache {
    results: BTreeMap<ConditionHash, bool>,
}

impl Default for ConditionCache {
    fn default() -> Self {
        Self::new()
    }
}

impl ConditionCache {
    pub fn new() -> Self {
        Self {
            results: BTreeMap::new(),
        }
    }

    pub fn get(&self, hash: &ConditionHash) -> Option<bool> {
        self.results.get(hash).copied()
    }

    pub fn set(&mut self, hash: ConditionHash, result: bool) {
        self.results.insert(hash, result);
    }
}
