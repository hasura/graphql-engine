use crate::NdcFieldAlias;
use crate::NdcRelationshipName;
use indexmap::IndexMap;
use open_dds::{data_connector::DataConnectorColumnName, types::DataConnectorArgumentName};
use std::collections::BTreeMap;

use super::{arguments, query};

/// Field plan
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Field {
    Column {
        /// Column
        column: DataConnectorColumnName,
        /// Nested fields if column is array or object type
        fields: Option<NestedField>,
        /// Input field arguments
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument>,
    },
    Relationship {
        /// The relationship query
        query_node: Box<query::QueryNode>,
        /// The name of the relationship to follow for the subquery
        relationship: NdcRelationshipName,
        /// Values to be provided to any collection arguments
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NestedField {
    Object(NestedObject),
    Array(NestedArray),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NestedObject {
    pub fields: IndexMap<NdcFieldAlias, Field>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NestedArray {
    pub fields: Box<NestedField>,
}
