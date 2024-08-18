mod logical;
mod physical;

pub(crate) use logical::{CommandOutput, CommandQuery};
pub(crate) use physical::NDCFunctionPushDown;
