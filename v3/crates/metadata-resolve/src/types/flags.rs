use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use strum_macros::EnumIter;

#[derive(
    Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, EnumIter,
)]
#[serde(rename_all = "snake_case")]
pub enum ResolvedRuntimeFlag {
    ValidateNonNullGraphqlVariables,
    SendMissingArgumentsToNdcAsNulls,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeFlags(BTreeSet<ResolvedRuntimeFlag>);

impl RuntimeFlags {
    pub fn new(flags: BTreeSet<ResolvedRuntimeFlag>) -> Self {
        RuntimeFlags(flags)
    }

    pub fn contains(&self, flag: ResolvedRuntimeFlag) -> bool {
        self.0.contains(&flag)
    }

    pub fn insert(&mut self, flag: ResolvedRuntimeFlag) -> bool {
        self.0.insert(flag)
    }

    pub fn from_open_dds_flags(flags: &open_dds::flags::OpenDdFlags) -> Self {
        let mut runtime_flags = RuntimeFlags::default();
        for flag in flags {
            if flag == &open_dds::flags::Flag::ValidateNonNullGraphqlVariables {
                runtime_flags.insert(ResolvedRuntimeFlag::ValidateNonNullGraphqlVariables);
            }
            if flag == &open_dds::flags::Flag::SendMissingArgumentsToNdcAsNulls {
                runtime_flags.insert(ResolvedRuntimeFlag::SendMissingArgumentsToNdcAsNulls);
            }
        }
        runtime_flags
    }
}
