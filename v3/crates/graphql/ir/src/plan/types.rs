use gql::normalized_ast;
use graphql_schema::GDS;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use plan_types::{
    JoinLocations, NDCMutationExecution, NDCQueryExecution, NDCSubscriptionExecution,
    PredicateQueryTrees,
};
use std::sync::Arc;

// in the new world, this is where we'll create execution plans in GraphQL
// it's here because
// a) it marks old and new more clearly
// b) it removes graphql concepts from `execute`

pub type QueryPlan<'n, 's, 'ir> = IndexMap<ast::Alias, NodeQueryPlan<'n, 's, 'ir>>;

/// Unlike a query, the root nodes of a mutation aren't necessarily independent. Specifically, the
/// GraphQL specification says that each root mutation must be executed sequentially. Moreover, if
/// we want to, say, insert a parent _and_ children in one query, we want the ability to make
/// transactional requests. In a mutation plan, we group nodes by connector, allowing us to issue
/// transactional commands to connectors whose capabilities allow for transactional mutations.
/// Otherwise, we can just send them one-by-one (though still sequentially).
pub struct MutationPlan<'n, 's> {
    pub nodes: IndexMap<
        Arc<metadata_resolve::DataConnectorLink>,
        IndexMap<ast::Alias, MutationSelect<'n, 's>>,
    >,
    pub type_names: IndexMap<ast::Alias, ast::TypeName>,
}

pub struct MutationSelect<'n, 's> {
    pub mutation_execution: NDCMutationExecution,
    pub selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
}

// At least for now, requests are _either_ queries or mutations, and a mix of the two can be
// treated as an invalid request. We may want to change this in the future.
pub enum RequestPlan<'n, 's, 'ir> {
    QueryPlan(QueryPlan<'n, 's, 'ir>),
    MutationPlan(MutationPlan<'n, 's>),
    SubscriptionPlan(ast::Alias, SubscriptionSelect<'s, 'ir>),
}

pub struct SubscriptionSelect<'ir, 's> {
    pub subscription_execution: NDCSubscriptionExecution,
    pub selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
}

/// Query plan of individual root field or node
#[derive(Debug)]
pub enum NodeQueryPlan<'n, 's, 'ir> {
    /// __typename field on query root
    TypeName { type_name: ast::TypeName },
    /// __schema field
    SchemaField {
        role: Role,
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
    },
    /// __type field
    TypeField {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
        type_name: ast::TypeName,
        role: Role,
    },
    /// NDC query to be executed
    NDCQueryExecution {
        query_execution: NDCQueryExecution,
        selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
    },
    /// NDC query for Relay 'node' to be executed
    RelayNodeSelect(
        Option<(
            NDCQueryExecution,
            &'ir normalized_ast::SelectionSet<'s, GDS>,
        )>,
    ),
    /// Apollo Federation query to be executed
    ApolloFederationSelect(ApolloFederationSelect<'n, 's, 'ir>),
}

#[derive(Debug)]
pub enum ApolloFederationSelect<'n, 's, 'ir> {
    /// NDC queries for Apollo Federation '_entities' to be executed
    EntitiesSelect(
        Vec<(
            NDCQueryExecution,
            &'ir normalized_ast::SelectionSet<'s, GDS>,
        )>,
    ),
    ServiceField {
        sdl: String,
        selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
    },
}

pub struct Plan<T> {
    pub inner: T,
    pub join_locations: JoinLocations,
    pub remote_predicates: PredicateQueryTrees,
}
