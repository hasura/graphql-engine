use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use open_dds::types::CustomTypeName;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarTypeRepresentation {
    pub graphql_type_name: Option<ast::TypeName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
}

pub struct ScalarTypesOutput {
    pub scalar_types: BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub issues: Vec<ScalarTypesIssue>,
}

#[derive(Debug, thiserror::Error)]
pub enum ScalarTypesIssue {
    #[error("Scalar type {type_name} conflicts with existing inbuilt type")]
    NameConflictsWithBuiltInType { type_name: CustomTypeName },
}

impl ShouldBeAnError for ScalarTypesIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::Flags) -> bool {
        match self {
            ScalarTypesIssue::NameConflictsWithBuiltInType { .. } => {
                flags.disallow_scalar_type_names_conflicting_with_inbuilt_types
            }
        }
    }
}
