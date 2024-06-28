use crate::types::subgraph::Qualified;
use open_dds::types::{CustomTypeName, GraphQlTypeName, OperatorName, TypeName, TypeReference};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use lang_graphql::ast::common as ast;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarBooleanExpressionsOutput {
    pub boolean_expression_scalar_types:
        BTreeMap<Qualified<CustomTypeName>, ResolvedScalarBooleanExpressionType>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedScalarBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,

    /// The OpenDD type this scalar refers to
    pub representation: TypeName,

    /// The list of comparison operators that can used on this scalar type
    pub comparison_operators: BTreeMap<OperatorName, TypeReference>,

    /// The list of mappings between OpenDD operator names and the names used in the data
    /// connector schema
    pub data_connector_operator_mappings: BTreeMap<
        Qualified<open_dds::data_connector::DataConnectorName>,
        open_dds::boolean_expression::DataConnectorOperatorMapping,
    >,

    // optional name for exposing this in the GraphQL schema
    pub graphql_name: Option<GraphQlTypeName>,

    // do we allow _is_null comparisons for this type?
    pub include_is_null: IncludeIsNull,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum IncludeIsNull {
    Yes,
    No,
}
