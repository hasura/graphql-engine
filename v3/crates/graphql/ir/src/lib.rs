use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use serde::Serialize;

mod aggregates;
mod arguments;
mod commands;
mod error;
mod filter;
mod global_id;
mod model_selection;
mod model_tracking;
mod mutation_root;
mod order_by;
mod permissions;
mod plan;
mod query_root;
mod relationship;
mod root_field;
mod selection_set;
mod subscription_root;

pub use aggregates::mk_alias_from_graphql_field_path;
pub use commands::{CommandInfo, CommandSelection, FunctionBasedCommand, ProcedureBasedCommand};
pub use error::{Error, InternalDeveloperError, InternalEngineError};
pub use filter::FilterExpression;
pub use global_id::{global_id_col_format, GLOBAL_ID_VERSION};
pub use model_selection::ModelSelection;
pub use model_tracking::get_all_usage_counts_in_query;
pub use mutation_root::generate_ir as generate_mutation_ir;
pub use order_by::OrderBy;
pub use plan::{
    generate_request_plan, ApolloFederationSelect, Error as PlanError, MutationPlan, NodeQueryPlan,
    QueryPlan, RequestPlan,
};
pub use query_root::generate_ir as generate_query_ir;
pub use relationship::{build_remote_command_relationship, build_remote_relationship};
pub use root_field::{
    ApolloFederationRootFields, MutationRootField, QueryRootField, SubscriptionRootField,
};
pub use selection_set::{
    generate_selection_set_ir, FieldSelection, NestedSelection, ResultSelectionSet,
};
pub use subscription_root::generate_ir as generate_subscription_ir;

/// Feature flag to decide whether to use the new OpenDD-based GraphQL request pipeline
/// Only used in development and tests atm, `Old` is exposed to users
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GraphqlRequestPipeline {
    Old,
    OpenDd,
}

/// The IR is the intermediate representation of the GraphQL operation.
#[derive(Serialize, Debug)]
pub enum IR<'n, 's> {
    Query(IndexMap<ast::Alias, root_field::QueryRootField<'n, 's>>),
    Mutation(IndexMap<ast::Alias, root_field::MutationRootField<'n, 's>>),
    /// Only one root field is allowed in a subscription
    Subscription(ast::Alias, root_field::SubscriptionRootField<'n, 's>),
}
