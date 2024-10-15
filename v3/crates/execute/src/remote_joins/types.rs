//! Join tree and related types for remote joins.
//!
use indexmap::IndexMap;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use json_ext::ValueExt;
use open_dds::arguments::ArgumentName;
use open_dds::types::FieldName;

use crate::plan::{self, ProcessResponseAs};

/// This tree structure captures all the locations (in the selection set IR) where
/// remote joins are found.
///
/// It also includes other info, like field mapping etc., for the join
#[derive(Debug, Clone)]
pub struct JoinLocations<'s> {
    pub locations: IndexMap<String, Location<'s>>,
}

impl<'s> JoinLocations<'s> {
    pub fn new() -> Self {
        JoinLocations::default()
    }

    pub fn is_empty(&self) -> bool {
        self.locations.is_empty()
    }
}

impl<'s> Default for JoinLocations<'s> {
    fn default() -> Self {
        JoinLocations {
            locations: IndexMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LocationKind {
    NestedData,
    LocalRelationship,
}

#[derive(Debug, Clone)]
pub enum JoinNode<'s> {
    Local(LocationKind),
    Remote(RemoteJoin<'s>),
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
pub struct Location<'s> {
    pub join_node: JoinNode<'s>,
    pub rest: JoinLocations<'s>,
}

/// Contains information to be captured for a join node
#[derive(Debug, Clone, PartialEq)]
pub struct RemoteJoin<'s> {
    /// target data connector to execute query on
    pub target_data_connector: Arc<metadata_resolve::DataConnectorLink>,
    /// NDC node to execute on a data connector
    pub target_ndc_execution: plan::query::UnresolvedQueryExecutionPlan<'s>,
    /// Mapping of the fields in source to fields in target.
    /// The HashMap has the following info -
    ///   - key: is the field name in the source
    ///   - value->first item: is the alias we create for the
    ///     source field. If the user did not request the join field in the
    ///     selection set, we include the join mapping field and call it a phantom
    ///     field.
    ///   - value->second item: is the target NDC field. This could be a model
    ///     field or an argument name.
    pub join_mapping: HashMap<SourceFieldName, (SourceFieldAlias, TargetField)>,
    /// Represents how to process the join response.
    pub process_response_as: ProcessResponseAs,
    /// Represents the type of the remote join
    pub remote_join_type: RemoteJoinType,
}

/// Name of the source field used in the join mapping
pub type SourceFieldName = FieldName;

/// Alias of the source field used in the join mapping. This is basically a NDC
/// field alias (which in the NDC IR is `String`). Change this when modifying
/// the IR to have a newtype Alias.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFieldAlias(pub String);

/// Target field used in the join mapping
#[derive(Debug, Clone, PartialEq)]
pub enum TargetField {
    ModelField((FieldName, metadata_resolve::NdcColumnForComparison)),
    CommandField(ArgumentName),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RemoteJoinType {
    ToModel,
    ToCommand,
}

/// An 'Argument' is a map of variable name to it's value.
/// For example, `{"first_name": "John", "last_name": "Doe"}`
pub type Argument = BTreeMap<plan_types::VariableName, ValueExt>;
