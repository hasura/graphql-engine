use crate::metadata::resolved::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use open_dds::types::CustomTypeName;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ScalarTypeRepresentation {
    pub graphql_type_name: Option<ast::TypeName>,
    pub description: Option<String>,
}

pub struct ScalarTypesOutput {
    pub scalar_types: HashMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    pub graphql_types: HashSet<ast::TypeName>,
}
