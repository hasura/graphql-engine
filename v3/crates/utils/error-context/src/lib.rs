use open_dds::identifier::SubgraphName;
use serde::{Deserialize, Serialize};

/// An error context is more comprehensive than a path as we can refer to multiple places in the
/// source. We also specifically use an orderable collection (`Vec`) so that we can "tell a story"
/// with the context by ordering the steps ("I found X here, but this conflicts with Y there").
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Context(pub Vec<Step>);

/// Each step of a context trace contains a path and an explanation for that path.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Step {
    pub message: String,
    pub path: jsonpath::JSONPath,
    pub subgraph: Option<SubgraphName>,
}
