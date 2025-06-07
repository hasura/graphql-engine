use indexmap::IndexMap;
use lang_graphql::ast::common as ast;

mod aggregates;
mod arguments;
mod commands;
mod error;
mod filter;
mod flags;
mod global_id;
mod model_selection;
mod model_tracking;
mod mutation_root;
mod order_by;
mod plan;
mod query_root;
mod relationship;
mod root_field;
mod selection_set;
mod subscription_root;

pub use aggregates::mk_alias_from_graphql_field_path;
pub use commands::{CommandInfo, FunctionBasedCommand, ProcedureBasedCommand};
pub use error::{Error, InternalDeveloperError, InternalEngineError};
pub use filter::FilterExpression;
pub use global_id::{GLOBAL_ID_VERSION, global_id_col_format};
pub use model_selection::ModelSelection;
pub use model_tracking::get_all_usage_counts_in_query;
pub use mutation_root::generate_ir as generate_mutation_ir;
pub use order_by::OrderBy;
pub use plan::{
    ApolloFederationSelect, Error as GraphqlIrPlanError, MutationPlan, NodeQueryPlan, QueryPlan,
    RequestPlan, generate_request_plan,
};
pub use query_root::generate_ir as generate_query_ir;
pub use root_field::{
    ApolloFederationRootFields, MutationRootField, QueryRootField, SubscriptionRootField,
};
pub use selection_set::{FieldSelection, NestedSelection, ResultSelectionSet};
pub use subscription_root::generate_ir as generate_subscription_ir;

/// The IR is the intermediate representation of the GraphQL operation.
#[derive(Debug)]
pub enum IR<'n, 's> {
    Query(IndexMap<ast::Alias, root_field::QueryRootField<'n, 's>>),
    Mutation(IndexMap<ast::Alias, root_field::MutationRootField<'n, 's>>),
    /// Only one root field is allowed in a subscription
    Subscription(ast::Alias, root_field::SubscriptionRootField<'n, 's>),
}
