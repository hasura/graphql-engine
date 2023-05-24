This note is in [Hasura.RQL.DDL.Schema.Cache](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache.hs#L318).
It is referenced at:
  - line 471 of [Hasura.RQL.DDL.Schema.Cache](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache.hs#L471)
  - line 215 of [Hasura.RQL.DDL.Schema.Cache.Common](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache/Common.hs#L215)
  - line 325 of [Hasura.RQL.DDL.Schema.Cache.Dependencies](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache/Dependencies.hs#L325)

# Avoiding GraphQL schema rebuilds when changing irrelevant Metadata

There are many Metadata operations that don't influence the GraphQL schema.  So
we should be caching its construction.

The `Hasura.Incremental` framework allows us to cache such constructions:
whenever we have an arrow `Rule m a b`, where `a` is the input to the arrow and
`b` the output, we can use the `Inc.cache` combinator to obtain a new arrow
which is only re-executed when the input `a` changes in a material way.  To test
this, `a` needs an `Eq` instance.

We can't simply apply `Inc.cache` to the GraphQL schema cache building phase
(`buildGQLContext`), because the inputs (components of `BuildOutputs` such as
`SourceCache`) don't have an `Eq` instance.

So the purpose of `buildOutputsAndSchema` is that we cach already at an earlier
point, encompassing more computation.  The Metadata and invalidation keys (which
have `Eq` instances) are used as a caching key, and `Inc.cache` can be applied
to the whole sequence of steps.

But because of the all-or-nothing nature of caching, it's important that
`buildOutputsAndSchema` is re-run as little as possible.  So the exercise
becomes to minimize the amount of stuff stored in `BuildOutputs`, so that as
many Metadata operations as possible can be handled outside of this codepath
that produces a GraphQL schema.

