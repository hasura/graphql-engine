//! Join tree and related types for remote joins.
//!
use indexmap::IndexMap;
use ndc_client as ndc;
use open_dds;
use open_dds::arguments::ArgumentName;
use open_dds::types::FieldName;
use std::collections::{BTreeMap, HashMap};

use crate::execute::query_plan::ProcessResponseAs;
use crate::metadata::resolved;
use crate::metadata::resolved::types::FieldMapping;
use crate::utils::json_ext::ValueExt;

/// This tree structure captures all the locations (in the selection set IR) where
/// remote joins are found.
///
/// It also includes other info, like field mapping etc., for the join
#[derive(Debug, Clone)]
pub struct JoinLocations<T> {
    pub locations: IndexMap<String, Location<T>>,
}

impl<T> JoinLocations<T> {
    pub fn new() -> JoinLocations<T> {
        JoinLocations::default()
    }
}

impl<T> Default for JoinLocations<T> {
    fn default() -> Self {
        JoinLocations {
            locations: IndexMap::new(),
        }
    }
}

/// Location indicates if the current node/field is a join node.
///
/// If it is a join node, then there is information about the join (captured as
/// [RemoteJoin]). It may further have more join nodes, represented by `rest`.
///
/// The current node may not have a join node and only have joins in sub-tree.
/// This is represented by `join_node` being `None` and `rest` containing the
/// sub-tree. This is required to represent where remote join nodes are inside a
/// local relationship selection.
///
/// ### Example
/// ```graphql
/// city {
///   name
///   state { # -> local relationship
///     name
///     census { # -> remote relationship
///       data
///     }
///   }
/// }
/// ```
///
///
/// `field mapping: state_id; join_field_alias: __state_id`
///
/// ```json
/// [("state", Location { join_node: None, rest: ... })],
/// [("census", Location { join_node: Some(..), rest: ... })]
/// ```
/// ---
/// ```json
/// [
///   {
///     "name": "Bangalore",
///     "state": {
///       "name": "KA",
///       "__state_id": 1
///     }
///   },
///   {
///     "name": "Mumbai",
///     "state": {
///       "name": "MH",
///       "__state_id": 2
///     }
///   }
/// ]
/// ```
///
/// Note: `join_node` and `rest` both cannot be empty; it is an invalid/illegal
/// state.
#[derive(Debug, Clone)]
pub struct Location<T> {
    pub join_node: Option<T>,
    pub rest: JoinLocations<T>,
}

/// Contains information to be captured for a join node
#[derive(Debug, Clone, PartialEq)]
pub struct RemoteJoin<'s, 'ir> {
    /// target data connector to execute query on
    pub target_data_connector: &'s resolved::data_connector::DataConnectorLink,
    /// NDC IR to execute on a data connector
    pub target_ndc_ir: ndc::models::QueryRequest,
    /// Mapping of the fields in source to fields in target.
    /// The HashMap has the following info -
    ///   - key: is the field name in the source
    ///   - value->first item: is the alias we create for the
    ///   source field. If the user did not request the join field in the
    ///   selection set, we include the join mapping field and call it a phantom
    ///   field.
    ///   - value->second item: is the target NDC field. This could be a model
    ///   field or an argument name.
    pub join_mapping: HashMap<SourceFieldName, (SourceFieldAlias, TargetField)>,
    /// Represents how to process the join response.
    pub process_response_as: ProcessResponseAs<'ir>,
    /// Represents the type of the remote join
    pub remote_join_type: RemoteJoinType,
}

pub type SourceFieldName = FieldName;
pub type SourceFieldAlias = String;

#[derive(Debug, Clone, PartialEq)]
pub enum TargetField {
    ModelField((FieldName, FieldMapping)),
    CommandField(ArgumentName),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RemoteJoinType {
    ToModel,
    ToCommand,
}

pub type JoinId = i16;

pub type Arguments = HashMap<Argument, ArgumentId>;
pub type Argument = BTreeMap<String, ValueExt>;
pub type ArgumentId = i16;
pub type ReplacementToken = (JoinId, ArgumentId);

// The structure is same as the structure of `Vec<RowSet>`, except
// `RowFieldValue` in the leaf, it has `ReplacementToken`
pub type ReplacementTokenRows = Vec<Option<Vec<ReplacementToken>>>;

/// Monotonically increasing counter with i16 value
pub(crate) struct MonotonicCounter {
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
