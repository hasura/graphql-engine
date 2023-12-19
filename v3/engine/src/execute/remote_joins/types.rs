use ndc_client as ndc;
use open_dds;
use open_dds::types::FieldName;
use std::collections::{BTreeMap, HashMap};

use crate::metadata::resolved;
use crate::metadata::resolved::types::FieldMapping;
use crate::utils::json_ext::ValueExt;

/// This tree structure captures all the locations (in the selection set IR) where
/// remote joins are found and also adds relevant info (join columns etc.) for
/// the join
#[derive(Debug, Clone)]
pub struct JoinLocations<T> {
    pub locations: HashMap<String, Location<T>>,
}

impl<T> JoinLocations<T> {
    pub fn new() -> JoinLocations<T> {
        JoinLocations::default()
    }
}

impl<T> Default for JoinLocations<T> {
    fn default() -> Self {
        JoinLocations {
            locations: HashMap::new(),
        }
    }
}

/// Location indicates if the current node/field is a join node; if it is, then
/// there is information about the join (captured as `RemoteJoin`). It may
/// further have more join nodes, represented by `rest`. Also, the current
/// node/field may not have a join node and only have joins in sub-tree. This is
/// represented by `join_node` being `None` and `rest` containing the sub-tree.
/// This is required to represent where remote join nodes are inside a local
/// relationship selection. If both `join_node` and `rest` are empty then it is
/// an invalid/illegal state.
#[derive(Debug, Clone)]
pub struct Location<T> {
    pub join_node: Option<T>,
    pub rest: JoinLocations<T>,
}

/*
impl<T> JoinLocations<T> {
    pub fn map<U, F>(self, mut f: F) -> JoinLocations<U>
    where
        F: FnMut(T) -> U,
    {
        let mut new_join_locations = JoinLocations::new();
        for (key, location) in self.locations {
            let new_node = location.join_node.map(&mut f);
            let new_location = Location {
                join_node: new_node,
                rest: location.rest.map(&mut f),
            };
            new_join_locations.locations.insert(key, new_location);
        }
        new_join_locations
    }
}*/

/// Contains information to be captured for a join node
#[derive(Debug, Clone, PartialEq)]
pub struct RemoteJoin<'s> {
    /// target data connector to execute query on
    pub target_data_connector: &'s resolved::data_connector::DataConnector,
    /// NDC IR to execute on a data connector
    pub target_ndc_ir: ndc::models::QueryRequest,
    /// Mapping of the fields in source (LHS) to fields in target (RHS). TODO:
    /// add more details about the type contents
    pub join_columns: HashMap<SourceFieldName, (SourceFieldAlias, TargetField)>,
}

pub type SourceFieldName = FieldName;
pub type SourceFieldAlias = String;
pub type SourceField = (FieldName, FieldMapping);
pub type TargetField = (FieldName, FieldMapping);
pub type JoinId = i16;

pub type Arguments = HashMap<Argument, ArgumentId>;
pub type Argument = BTreeMap<String, ValueExt>;
pub type ArgumentId = i16;
pub type ReplacementToken = (JoinId, ArgumentId);

// The structure is same as the structure of `Vec<RowSet>`, except
// `RowFieldValue` in the leaf, it has `ReplacementToken`
pub type ReplacementTokenRows = Vec<Option<Vec<ReplacementToken>>>;

/// Monotonically increasing counter with i16 value
pub struct MonotonicCounter {
    id: i16,
}

impl MonotonicCounter {
    pub fn new() -> MonotonicCounter {
        MonotonicCounter { id: 0 }
    }
    /// increment the counter and get the value
    pub fn get_next(&mut self) -> i16 {
        self.id += 1;
        self.id
    }
    /// get current value without incrementing the counter
    pub fn get(&self) -> i16 {
        self.id
    }
}

impl Default for MonotonicCounter {
    fn default() -> Self {
        Self::new()
    }
}
