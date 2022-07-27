This note is in [Hasura.GraphQL.Schema.Introspect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Introspect.hs#L146).
It is referenced at:
  - line 194 of [Hasura.GraphQL.Schema.Introspect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Introspect.hs#L194)
  - line 253 of [Hasura.GraphQL.Schema.Introspect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Introspect.hs#L253)

# What introspection exposes

NB: By "introspection query", we mean a query making use of the __type or
__schema top-level fields.

It would be very convenient if we could simply build up our desired GraphQL
schema, without regard for introspection. Ideally, we would then extract the
data required for introspection from this complete GraphQL schema. There are,
however, some complications:

1. Of course, we _do_ need to include the introspection fields themselves into
   the query_root, so that we can deal with introspection queries
   appropriately. So we can't avoid thinking about introspection entirely while
   constructing the GraphQL schema.

2. The GraphQL specification says that although we must always expose __type and
   __schema fields as part of the query_root, they must not be visible fields of
   the query_root object type. See
   http://spec.graphql.org/June2018/#sec-Schema-Introspection

At this point, one might naively attempt to generate two GraphQL schemas:

- One without the __type and __schema fields, from which we generate the data
  required for responding to introspection queries.
- One with the __type and __schema fields, which is used to actually respond to
  queries.

However, this is also not GraphQL-compliant!

The problem here is that while __type and __schema are not visible fields of the
query root object, their *types*, __Type and __Schema respectively, *must be*
exposed through __type introspection, even though those types never appear as
(transitive) members of the query/mutation/subscription root fields.

So in order to gather the data required for introspection, we follow the
following recipe:

A. Collect type information from the introspection-free GraphQL schema

B. Collect type information from the introspection-only GraphQL schema

C. Stitch together the results of (A) and (B). In particular, the query_root
from (A) is used, and all types from (A) and (B) are used, except for the
query_root obtained in (B).
