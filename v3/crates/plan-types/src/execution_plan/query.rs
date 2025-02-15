use super::{aggregates, arguments, field, filter, relationships};
use crate::NdcFieldAlias;
use crate::OrderByElement;
use crate::{
    AggregateFieldSelection, NdcRelationshipName, QueryExecutionTree, RelationshipColumnMapping,
    VariableName,
};
use indexmap::IndexMap;
use metadata_resolve::Qualified;
use open_dds::{
    data_connector::CollectionName, models::ModelName, types::DataConnectorArgumentName,
};
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
// this represents an execution plan. all predicates only refer to local comparisons.
// remote predicates are represented as additional execution nodes
pub struct QueryExecutionPlan {
    pub query_node: QueryNodeNew,
    /// The name of a collection
    pub collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, relationships::Relationship>,
    /// One set of named variables for each rowset to fetch. Each variable set
    /// should be subtituted in turn, and a fresh set of rows returned.
    pub variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
    /// The data connector used to fetch the data
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}

/// A tree of queries that are used to execute remote predicates
#[derive(Debug, Clone, PartialEq)]
pub struct PredicateQueryTree {
    pub ndc_column_mapping: Vec<RelationshipColumnMapping>,
    pub target_model_name: Qualified<ModelName>,
    pub query: QueryExecutionTree,
    pub children: PredicateQueryTrees,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PredicateQueryTrees(pub BTreeMap<RemotePredicateKey, PredicateQueryTree>);

impl PredicateQueryTrees {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
    pub fn insert(
        &mut self,
        unique_number: &mut UniqueNumber,
        value: PredicateQueryTree,
    ) -> RemotePredicateKey {
        let key = RemotePredicateKey(unique_number.fresh());
        self.0.insert(key, value);
        key
    }
}

impl Default for PredicateQueryTrees {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, derive_more::Display, Ord, Hash, Clone, Copy)]
pub struct RemotePredicateKey(pub u64);

// we need to generate unique identifiers for remote predicates
// in a reproducable fashion so we thread this around
pub struct UniqueNumber(u64);

impl UniqueNumber {
    pub fn new() -> Self {
        UniqueNumber(1)
    }

    // get the next number, increment internal value
    pub fn fresh(&mut self) -> u64 {
        let value = self.0;
        self.0 += 1;
        value
    }
}

impl Default for UniqueNumber {
    fn default() -> Self {
        Self::new()
    }
}

/// Query plan for fetching data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryNodeNew {
    /// Optionally limit to N results
    pub limit: Option<u32>,
    /// Optionally offset from the Nth result
    pub offset: Option<u32>,
    /// Optionally sort results
    pub order_by: Option<Vec<OrderByElement<filter::ResolvedFilterExpression>>>,
    /// Optionally filter results
    pub predicate: Option<filter::ResolvedFilterExpression>,
    /// Aggregate fields of the query
    pub aggregates: Option<aggregates::AggregateSelectionSet>,
    /// Fields of the query
    pub fields: Option<FieldsSelection>,

    pub group_by: Option<aggregates::Grouping>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldsSelection {
    pub fields: IndexMap<NdcFieldAlias, field::Field>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateFieldsSelection {
    pub aggregate_fields: IndexMap<NdcFieldAlias, AggregateFieldSelection>,
}
