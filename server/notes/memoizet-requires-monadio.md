This note is in [Control.Monad.Memoize](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Monad/Memoize.hs#L185).
It is referenced at:
  - line 135 of [Control.Monad.Memoize](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Monad/Memoize.hs#L135)

# MemoizeT requires MonadIO

The MonadMemoize instance for MemoizeT requires MonadIO, which is unsatisfying.
The only reason the constraint is needed is to implement knot-tying via IORefs
(see Note [Tying the knot] above), which really only requires the power of
ST. Alternatively, it might be possible to use the ST monad instead, but that
has not been done for historical reasons.

