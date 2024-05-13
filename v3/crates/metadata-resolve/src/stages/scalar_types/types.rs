use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use open_dds::types::CustomTypeName;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarTypeRepresentation {
    pub graphql_type_name: Option<ast::TypeName>,
    pub description: Option<String>,
}

pub struct ScalarTypesOutput {
    pub scalar_types: BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}
