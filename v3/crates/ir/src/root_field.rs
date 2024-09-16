/// IR of a root field
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use open_dds::permissions::Role;
use serde::Serialize;

use super::{
    commands,
    query_root::{apollo_federation, node_field, select_aggregate, select_many, select_one},
};
use schema::GDS;

/// IR of a query root field
#[derive(Serialize, Debug)]
pub enum QueryRootField<'n, 's> {
    // __typename field on query root
    TypeName {
        type_name: ast::TypeName,
    },
    // __schema field
    SchemaField {
        role: Role,
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
    },
    // __type field
    TypeField {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
        type_name: ast::TypeName,
        role: Role,
    },
    // Operation that selects a single row from a model
    ModelSelectOne {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        ir: select_one::ModelSelectOne<'n, 's>,
    },
    // Operation that selects many rows from a model
    ModelSelectMany {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        ir: select_many::ModelSelectMany<'n, 's>,
    },
    // Operation that selects an aggregate of rows from a model
    ModelSelectAggregate {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        ir: select_aggregate::ModelSelectAggregate<'n, 's>,
    },
    // Operation that selects a single row from the model corresponding
    // to the Global Id input.
    NodeSelect(Option<node_field::NodeSelect<'n, 's>>),
    FunctionBasedCommand {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        ir: commands::FunctionBasedCommand<'s>,
    },
    // Apollo Federation related root fields
    ApolloFederation(ApolloFederationRootFields<'n, 's>),
}

#[derive(Serialize, Debug)]
pub enum ApolloFederationRootFields<'n, 's> {
    // Operation that selects entities according to the Apollo Federation spec
    EntitiesSelect(Vec<apollo_federation::EntitySelect<'n, 's>>),
    // Operation for the _service field (returns the schema in SDL format)
    ServiceField {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
        role: Role,
    },
}

/// IR of a mutation root field
#[derive(Serialize, Debug)]
pub enum MutationRootField<'n, 's> {
    // __typename field on mutation root
    TypeName {
        type_name: ast::TypeName,
    },
    ProcedureBasedCommand {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        ir: commands::ProcedureBasedCommand<'s>,
    },
}
