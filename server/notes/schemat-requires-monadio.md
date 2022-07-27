This note is in [Hasura.GraphQL.Parser.Monad](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Monad.hs#L99).
It is referenced at:
  - line 43 of [Hasura.GraphQL.Parser.Monad](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Monad.hs#L43)

# SchemaT requires MonadIO

The MonadSchema instance for SchemaT requires MonadIO, which is unsatisfying.
The only reason the constraint is needed is to implement knot-tying via IORefs
(see Note [Tying the knot] in Hasura.GraphQL.Parser.Class), which really only
requires the power of ST. Using ST would be much nicer, since we could discharge
the burden locally, but unfortunately we also want to use MonadUnique, which
is handled by IO in the end.

This means that we need IO at the base of our monad, so to use STRefs, we’d need
a hypothetical STT transformer (i.e. a monad transformer version of ST). But
such a thing isn’t safe in general, since reentrant monads like ListT or ContT
would incorrectly share state between the different threads of execution.

In theory, this can be resolved by using something like Vault (from the vault
package) to create “splittable” sets of variable references. That would allow
you to create a transformer with an STRef-like interface that works over any
arbitrary monad. However, while the interface would be safe, the implementation
of such an abstraction requires unsafe primitives, and to the best of my
knowledge no such transformer exists in any existing libraries.

So we decide it isn’t worth the trouble and just use MonadIO. If `eff` ever pans
out, it should be able to support this more naturally, so we can fix it then.
