This note is in [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L219).
It is referenced at:
  - line 297 of [Hasura.Incremental.Internal.Rule](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Incremental/Internal/Rule.hs#L297)
  - line 41 of [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L41)
  - line 68 of [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L68)
  - line 107 of [Control.Arrow.Extended](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Extended.hs#L107)
  - line 32 of [Control.Arrow.Trans](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Trans.hs#L32)
  - line 46 of [Control.Arrow.Trans](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Control/Arrow/Trans.hs#L46)

# Weird control operator types

Arrow notation (i.e. `proc`) has support for so-called “custom control operators,” which allow
things like

    proc (x, y) -> do
      z <- foo -< x
      (f -< z) `catchA` \e -> g -< (y, e)

to magically work. What’s so magical about that? Well, note that `catchA` is an ordinary function,
but it’s being given /commands/ as arguments, not expressions. Also note that the arguments to
`catchA` reference the variables `y` and `z`, which are bound earlier in the `proc` expression as
arrow-local variables.

To make this work, GHC has to thread `y` and `z` through `catchA` in the generated code, which will
end up being something like this:

        arr (\(x, y) -> (x, (x, y)))
    >>> first foo
    >>> arr (\(z, (x, y)) -> (z, y))
    >>> catchA (first f)
               (arr (\((_, y), e) -> (y, e)) >>> g)

Quite complicated, which is why we’re glad we don’t have to write it all out ourselves!
Unfortunately, since GHC 7.8, GHC has required some pretty stupid-looking types for control
operators to allow them to be used in `proc` notation. The natural type for `catchA` is

    catchA :: arr a b -> arr (a, e) b -> arr a b

but GHC requires the far uglier

    catchA :: arr (a, s) b -> arr (a, (e, s)) b -> arr (a, s) b

in order to make the type inference work out. I (Alexis) have submitted a GHC proposal to fix this
<https://github.com/ghc-proposals/ghc-proposals/pull/303>, so hopefully we’ll be able to use the
nicer type in the future (GHC 8.12 at the earliest). For now, though, we’ll have to use the ugly
version.

As of GHC 8.10, the way to read arrow control operator types is to look for arguments with a shape
like this:

    arr (e, (a1, (a2, ... (an, s)))) b

The “actual” arguments to the arrow are the `a1` through `an` types, and the `e` and `s` types are
sort of “bookends.” So if you see a type like

    arr (e, (Integer, (Char, (Bool, s)))) String

then you should read it as an arrow that takes three “arguments” of type `Integer`, `Char`, and
`Bool` and returns a `String`.

Stopping there is basically good enough, but if you want to know what’s really going on, the idea is
that each command in a `proc` block has an “environment” and an “argument stack,” represented by the
types `e` and `s`, respectively. The environment is used to thread arrow-local variables that are
currently in scope, and the argument stack (as the name implies) is used to pass the command
arguments. Control operators can push and pop things from this argument stack, and in the base case,
the empty argument stack is represented by `()`. For a full explanation, see the section of the GHC
User’s Guide on arrow notation:

    https://downloads.haskell.org/ghc/8.8.1/docs/html/users_guide/glasgow_exts.html#arrow-notation

Yes, this all kind of sucks. Sorry.

