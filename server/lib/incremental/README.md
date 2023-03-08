# `incremental`

A library for caching intermediate results in `Arrow` computations. Used by
`graphql-engine` to optimise schema cache updates by avoiding redundant
recomputation.

## Schema cache?

The [`SchemaCache`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-Types-SchemaCache.html#t:SchemaCache) is
is a very complex object that, among other things, keeps track of the GraphQL
schema and its parser for each user role in a `graphql-engine` instance. Its
computation is based on both user-supplied metadata _and_ the schema of each
data source configured to work with Hasura. When one of these dependencies is
updated (perhaps the user makes a metadata change), we'd like to recompile the
schema cache as quickly (and efficiently with regards to memory) as possible.

For our specific uses of `incremental`, see the implementation of
[`buildSchemaCacheRule`](https://hasura.github.io/graphql-engine/server/haddock/main/src/Hasura.RQL.DDL.Schema.Cache.html#buildSchemaCacheRule)
and its `Inc.cache` calls, as well as the note titled, [`Avoiding GraphQL schema
rebuilds when changing irrelevant
Metadata`](https://hasura.github.io/graphql-engine/server/notes/avoiding-graphql-schema-rebuilds-when-changing-irrelevant-metadata.html).

## `Arrow` 101 and `ArrowCache`

[The `Arrow` abstraction](http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf)
describes a computation from some input to some output. A very simple example
of an `Arrow` is regular Haskell functions `a -> b`: a computation that
transforms some `a` into some `b`. Another good example is functions of the
shape `a -> m b` for some `Monad m`: a computation that transforms some `a`
into some `b` with potential side-effects. The `Arrow` abstraction forms the
basis of a whole hierarchy of classes, such as
[`ArrowChoice`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Arrow.html#t:ArrowChoice)
and
[`ArrowLoop`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Arrow.html#t:ArrowLoop),
each of which build on this basic idea. We can think of these as analogous to
`Monad` being the basis of classes like
[`MonadPlus`](https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Base.html#t:MonadPlus)
and
[`MonadIO`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-IO-Class.html#t:MonadIO):
each class describes further specific capabilities of the generic `Monad`
interface.

This library introduces the
[`ArrowCache`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental-Internal-Cache.html#t:ArrowCache) class. Sometimes, an `Arrow`
represents a computation that is computationally expensive, but is also
consistent in its output for any given input. For example, a compiler takes a
collection of source files and produces a binary. This computation can be very
expensive if we have a lot of source files and/or a complicated compiler, but
the result should be deterministic. In this case, `ArrowCache` allows us to
decorate an `Arrow` with a caching mechanism, avoiding recompilation of
unchanged files.

When we `cache` an arrow, we keep track of the last input/output pair. If the
next input matches the last input, we can skip the computation and return the
last output, avoiding the need for the expensive computation. If the next input
doesn't match the last input, we replace the stored pair with the new
input/output pair. We can cache any arrow or composition of arrows in a
pipeline, allowing us to express different granularities of caching[^1].

If thinking of programs as a composition of `Arrow`s is unfamiliar, we can
instead think of our program as a directed, acyclic graph of intermediate
results. For example, our compiler might first compile the individual modules,
and then bundle them together. Any one of these intermediate results can be
cached to avoid expensive recomputation when its dependencies remain unchanged.

## How does it work?

### `Rule` and `cache`

[`Rule`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental-Internal-Rule.html#t:Rule)
is the interesting implementation of `ArrowCache`. Its instance in
[`Hasura.Incremental.Internal.Cache`](https://hasura.github.io/graphql-engine/server/haddock/main/src/Hasura.Incremental.Internal.Cache.html)
explains the system above, specifically in the `cached` definition of its
`where` clause: if the input has not changed, then we return the last result.
Otherwise, we recompute a result for the new input. Note that this is not
memoisation: we don't store an input/output map. We only ever store the last
input/output pair.

### Keyed Dependencies

The `ArrowCache` also features two other functions:
[`newDependency`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental-Internal-Cache.html#v:newDependency)
and
[`dependOn`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental-Internal-Cache.html#v:dependOn).
Consider an arrow that takes `Metadata` as input, but only ever accesses a
small set of specific fields within it. In this case, we'd ideally only like to
consider those particular fields when we determine whether we should recompute!

The `Dependency` type gives us a way to keep track of the parts of a structure
on which we'd like to depend, and `Rule` then uses this information to cache
specifically based on our
[`Accesses`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental-Internal-Dependency.html#t:Accesses),
rather than just naïvely checking for equality between the last and next input.
The functions for working with `Dependency` live in
[`Hasura.Incremental.Internal.Dependency`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental-Internal-Dependency.html).

### Invalidation keys

Sometimes, we want to cache arrows that are _not_ deterministic. In these
cases, we might want to re-compute some value despite the inputs being the
same. For example, data source introspection is an inherently non-deterministic
action: what if the database schema changes? In these cases, the provided
[`InvalidationKey`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Incremental.html#t:InvalidationKey)
helper type can be used. When we `invalidate` an `InvalidationKey`, the
resulting value does not equal the previous value, which should force a
recomputation of the arrow when it is given as part of the input.

## Gotchas

### Monadic parameter-passing in `Rule`

The reason for using the `Arrow` abstraction is to talk about the computational
relationship between the input and the output. In our specific case, what we
want to talk about is whether or not we've seen this pairing before. However,
this relies on the `Arrow` input being the only input to the computation.

When we `build` or `rebuild` a `Rule m a b` computation, we do so inside some
`Applicative m`. If this `m` happens to be, say, `(->) r`, then we have access
to an `r` parameter that isn't explicitly given as the `a` parameter, and it
will not be considered part of the input that determines whether or not we need
to recompute. This can lead to problems: if the result of the computation
meaningfully changes depending on this value, then repeated calls may yield
incorrect results. In general, we should be careful to make sure that our
chosen `m` does not affect our decision to cache.

On the other hand, this is an occasionally helpful loophole. For example, in
`Hasura.RQL.Types.SchemaCache.Build`, we use a `MonadReader BuildReason m`
constraint to determine whether we need to update, say, the event trigger
catalogue. If the input (i.e. the schema) hasn't changed, then the
`BuildReason` doesn't matter: the fact that it is accessed via the `m` means
that caching works the way we'd like it to work.

Another example is when `m` happens to be `IO`: in these cases, we have no
guarantees about values being introduced during the computation. As mentioned
in the `InvalidationKey` section, non-determinism may be desired, and in these
cases the onus is on the user to invalidate the cache when recomputations are
required.

[^1]: It might be tempting to cache every single arrow. However, this quite
quickly leads to a large amount of memory being consumed, and often for no good
reason. Sometimes, a result is so simple to recompute that we're better off
recomputing rather than allocating more memory.
