//! new execution plan types, entirely separate from `execute` crate
mod aggregates;
mod arguments;
mod field;
mod filter;
mod mutation;
mod query;
mod relationships;
mod remote_joins;
use std::sync::Arc;

pub use aggregates::{AggregateFieldSelection, AggregateSelectionSet, Dimension, Grouping};
pub use arguments::{Argument, MutationArgument};
pub use field::{Field, NestedArray, NestedField, NestedObject};
pub use filter::ResolvedFilterExpression;
pub use mutation::MutationExecutionPlan;
pub use query::{
    AggregateFieldsSelection, FieldsSelection, PredicateQueryTree, PredicateQueryTrees,
    QueryExecutionPlan, QueryNode, RemotePredicateKey, UniqueNumber,
};
pub use relationships::{Relationship, RelationshipArgument};
pub use remote_joins::{
    JoinLocations, JoinNode, Location, LocationKind, RemoteJoin, RemoteJoinFieldMapping,
    RemoteJoinObjectFieldMapping, RemoteJoinObjectTargetField, RemoteJoinType, RemoteJoinVariable,
    RemoteJoinVariableSet, SourceFieldAlias, TargetField, mk_argument_target_variable_name,
};

#[derive(Debug, PartialEq)]
pub struct NDCQueryExecution {
    pub execution_tree: QueryExecutionTree,
    pub execution_span_attribute: &'static str,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs,
}

#[derive(Debug, PartialEq)]
pub struct NDCMutationExecution {
    pub execution_tree: MutationExecutionTree,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
    pub execution_span_attribute: &'static str,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs,
}

#[derive(Debug, PartialEq)]
pub struct NDCSubscriptionExecution {
    pub query_execution_plan: QueryExecutionPlan,
    pub polling_interval_ms: u64,
    pub execution_span_attribute: &'static str,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs,
}

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionTree {
    pub remote_predicates: PredicateQueryTrees,
    pub query_execution_plan: query::QueryExecutionPlan,
    pub remote_join_executions: remote_joins::JoinLocations,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MutationExecutionTree {
    pub mutation_execution_plan: mutation::MutationExecutionPlan,
    pub remote_join_executions: remote_joins::JoinLocations,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CommandReturnKind {
    Array,
    Object,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProcessResponseAs {
    Object {
        is_nullable: bool,
    },
    Array {
        is_nullable: bool,
    },
    CommandResponse {
        command_name: Arc<metadata_resolve::Qualified<open_dds::commands::CommandName>>,
        is_nullable: bool,
        return_kind: CommandReturnKind,
        // how to process a command response
        response_config: Option<Arc<metadata_resolve::data_connectors::CommandsResponseConfig>>,
    },
    Aggregates,
}

impl ProcessResponseAs {
    pub fn is_nullable(&self) -> bool {
        match self {
            ProcessResponseAs::Object { is_nullable }
            | ProcessResponseAs::Array { is_nullable }
            | ProcessResponseAs::CommandResponse { is_nullable, .. } => *is_nullable,
            ProcessResponseAs::Aggregates { .. } => false,
        }
    }
}
