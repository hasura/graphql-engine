pub mod logical;
pub mod physical;

pub(crate) use logical::{CommandOutput, CommandQuery};
pub(crate) use physical::{build_execution_plan, NDCFunctionPushDown, NDCProcedurePushDown};
