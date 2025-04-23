use open_dds::identifier::SubgraphName;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

/// An error context is more comprehensive than a path as we can refer to multiple places in the
/// source. We also specifically use an orderable collection (`Vec`) so that we can "tell a story"
/// with the context by ordering the steps ("I found X here, but this conflicts with Y there").
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Context(pub VecDeque<Step>);

impl Context {
    pub fn from_step(step: Step) -> Self {
        Self(vec![step].into())
    }

    // add a new step at the end of the list
    pub fn append(mut self, step: Step) -> Self {
        self.0.push_back(step);
        self
    }

    // add a new step at the beginning of the list
    pub fn prepend(mut self, step: Step) -> Self {
        self.0.push_front(step);
        self
    }
}

/// Each step of a context trace contains a path and an explanation for that path.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Step {
    pub message: String,
    pub path: jsonpath::JSONPath,
    pub subgraph: Option<SubgraphName>,
}
