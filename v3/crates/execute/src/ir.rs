use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use serde::Serialize;

pub mod aggregates;
pub mod arguments;
pub mod commands;
pub mod error;
pub mod filter;
pub mod model_selection;
pub mod mutation_root;
pub mod order_by;
pub mod permissions;
pub mod query_root;
pub mod relationship;
pub mod root_field;
pub mod selection_set;

/// The IR is the intermediate representation of the GraphQL operation.
#[derive(Serialize, Debug)]
pub enum IR<'n, 's> {
    Query(IndexMap<ast::Alias, root_field::QueryRootField<'n, 's>>),
    Mutation(IndexMap<ast::Alias, root_field::MutationRootField<'n, 's>>),
}
