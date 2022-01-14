This note is in [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L75).
It is referenced at:
  - line 162 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L162)
  - line 168 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L168)
  - line 183 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L183)
  - line 191 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L191)
  - line 199 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L199)
  - line 210 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L210)
  - line 225 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L225)

# Desugaring derived operations

One subtlety to the above is that we want to define operations in terms of other operations as much
as possible to avoid the need to write an enormous number of rewrite rules, but if we define them
that way directly, then we’ll end up using needlessly inefficient implementations when the
operations aren’t specialized. Therefore, we provide efficient implementations of operations like
`second`, but aggressively rewrite them in terms of simpler primitives like `first` when GHC is able
to specialize them.
