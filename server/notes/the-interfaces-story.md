This note is in [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L334).
It is referenced at:
  - line 203 of [Hasura.GraphQL.Parser.Internal.Parser](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Parser.hs#L203)
  - line 259 of [Hasura.GraphQL.Parser.Internal.Parser](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Parser.hs#L259)
  - line 450 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L450)
  - line 473 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L473)
  - line 485 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L485)

# The interfaces story

GraphQL interfaces are not conceptually complicated, but they pose some
non-obvious challenges for our implementation. First, familiarize yourself with
GraphQL interfaces themselves:

  * https://graphql.org/learn/schema/#interfaces
  * http://spec.graphql.org/June2018/#sec-Interfaces
  * http://spec.graphql.org/June2018/#sec-Objects

The most logical repesentation of object and interface types is to have objects
reference the interfaces they implement, but not the other way around. After
all, that’s how it works in the GraphQL language: when you declare an interface,
you just specify its fields, and you specify which interfaces each object type
implements as part of their declarations.

However, this representation is actually not very useful for us. We /also/ need
the interfaces to reference the objects that implement them---forming a circular
structure---for two reasons:

  1. Most directly, we need this information for introspection queries.
     Introspection queries for object types return the set of interfaces they
     implement <http://spec.graphql.org/June2018/#sec-Object>, and introspection
     queries for interfaces return the set of object types that implement them
     <http://spec.graphql.org/June2018/#sec-Interface>.

  2. Less obviously, it’s more natural to specify the relationships “backwards”
     like this when building the schema using the parser combinator language.

     From the parser’s point of view, each implementation of an interface
     corresponds to a distinct parsing possibility. For example, when we
     generate a Relay schema, the type of the `node` root field is an interface,
     and each table is a type that implements it:

         type query_root {
           node(id: ID!): Node
           ...
         }

         interface Node {
           id: ID!
         }

         type author implements Node {
           id: ID!
           name: String!
           ...
         }

         type article implements Node {
           id: ID!
           title: String!
           body: String!
           ...
         }

     A query will use fragments on the Node type to access table-specific fields:

         query get_article_info($article_id: ID!) {
           node(id: $article_id) {
             ... on article {
               title
               body
             }
           }
         }

     The query parser needs to know which types implement the interface (and
     how to parse their selection sets) so that it can parse the fragments.

This presents some complications, since we need to build this information in a
circular fashion. Currently, we do this in a very naïve way:

  * We require selectionSetObject to specify the interfaces it implements /and/
    require selectionSetInterface to specify the objects that implement it.

  * We take advantage of our existing memoization mechanism to do the knot-tying
    for us (see Note [Tying the knot] in Hasura.GraphQL.Parser.Class).

You may notice that this makes it possible for the definitions to be
inconsistent: we could construct an interface parser that parses some object
type, but forget to specify that the object type implements the interface. This
inconsistency is currently completely unchecked, which is quite unfortunate. It
also means we don’t support remote schema-defined object types that implement
interfaces we generate, since we don’t know anything about those types when we
construct the interface.

Since we don’t make very much use of interface types at the time of this
writing, this isn’t much of a problem in practice. But if that changes, it would
be worth implementing a more sophisticated solution that can gather up all the
different sources of information and make sure they’re consistent.
