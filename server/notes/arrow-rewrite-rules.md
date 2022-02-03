This note is in [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L282).
It is referenced at:
  - line 4 of [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L4)
  - line 135 of [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L135)
  - line 159 of [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L159)
  - line 63 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L63)

# Arrow rewrite rules

GHC’s desugaring of `proc` notation is not very clever, so it can generate some pretty inefficient
code. Almost everything is translated into uses of `arr`, `first`, and `(|||)`, and arrows end up
threading around massive tuples that constantly need to be packed and unpacked. To get good
performance, GHC relies on rewrite rules that expose optimizations to the simplifier, allowing the
packing and unpacking to be significantly reduced.

The most crucial rewrite rules are the ones for “`arr` fusion”, which rewrite expressions like
`arr f . arr g` into `arr (f . g)`. It might not be obvious at first why this is so important, but
remember that the arguments to `arr` are plain functions, not arrows. These functions might be
something like:

    f (a, (b, c)) = ((a, b), c)
    g ((a, _), c) = (a, c)

The composition of these functions `f . g` can be optimized to

    f . g = \(a, (_, c)) -> (a, c)

skipping the intermediate tuple completely, but GHC can only do that if the two functions are
composed directly. If GHC only sees `arr f . arr g`, then it can’t assume anything about `arr`
(which might be overloaded), so it gets stuck.

The rewrite rules defined in Control.Category, Control.Arrow, and this module take advantage of
certain typeclass laws to enable many more optimizations to fire. However, there is a caveat to all
this: when GHC knows the concrete type of a particular arrow, it aggressively specializes uses of
`arr` and other operations to the concrete type. This process bypasses the rewrite rules completely.

GHC tries to warn us about this with the `-Winline-rule-shadowing` warning, but in this case, we
want the rules anyway, since they might fire on polymorphic code. However, the takeaway is that the
generic rules are not sufficient to get fast code: it’s important to /also/ define type-specific
rules in the event that GHC specializes concrete code. The good news is that those type-specific
rules can take advantage of type-specific optimizations, getting even better performance than would
be possible using the generic rules. The bad news is it’s a bit more work.
