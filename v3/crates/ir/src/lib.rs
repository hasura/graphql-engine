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
mod query_root;
mod relationship;
mod remote_joins;
mod root_field;
mod selection_set;

pub use error::{Error, InternalDeveloperError, InternalEngineError};
pub use remote_joins::VariableName;

pub use aggregates::{
    mk_alias_from_graphql_field_path, AggregateFieldSelection, AggregateSelectionSet,
};
pub use arguments::{process_connector_link_presets, Argument};
pub use commands::{CommandInfo, FunctionBasedCommand, ProcedureBasedCommand};
pub use filter::expression::{
    ComparisonTarget, ComparisonValue, Expression, LocalFieldComparison, RelationshipColumnMapping,
    SourceNdcColumn,
};
pub use filter::FilterExpression;
pub use global_id::{global_id_col_format, GLOBAL_ID_VERSION};
pub use model_selection::ModelSelection;
pub use model_tracking::{get_all_usage_counts_in_query, UsagesCounts};
pub use mutation_root::generate_ir as generate_mutation_ir;
pub use order_by::{OrderByElement, OrderByTarget, ResolvedOrderBy};
pub use permissions::process_model_predicate;
pub use query_root::generate_ir as generate_query_ir;
pub use relationship::{
    build_remote_command_relationship, build_remote_relationship, get_field_mapping_of_field_name,
    LocalCommandRelationshipInfo, LocalModelRelationshipInfo,
};
pub use root_field::{ApolloFederationRootFields, MutationRootField, QueryRootField};
pub use selection_set::{
    generate_selection_set_ir, FieldSelection, NdcFieldAlias, NdcRelationshipName, NestedSelection,
    ResultSelectionSet,
};

/// The IR is the intermediate representation of the GraphQL operation.
#[derive(Serialize, Debug)]
pub enum IR<'n, 's> {
    Query(IndexMap<ast::Alias, root_field::QueryRootField<'n, 's>>),
    Mutation(IndexMap<ast::Alias, root_field::MutationRootField<'n, 's>>),
}
