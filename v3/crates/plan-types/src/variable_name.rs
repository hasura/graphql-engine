use serde::Serialize;

/// Name of the variable used in the IR
#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Clone, Serialize)]
pub struct VariableName(pub String);
