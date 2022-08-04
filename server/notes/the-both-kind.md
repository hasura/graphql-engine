This note is in [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L92).
It is referenced at:
  - line 67 of [Hasura.GraphQL.Parser.Internal.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Types.hs#L67)
  - line 87 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L87)
  - line 185 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L185)

# The 'Both kind

As described in the Haddock comments for Kind and <:, we use Kind to index
various types, such as Type and Parser. We use this to enforce various
correctness constraints mandated by the GraphQL spec; for example, we don’t
allow input object fields to have output types and we don’t allow output object
fields to have input types.

But scalars and enums can be used as input types *or* output types. A natural
encoding of that in Haskell would be to make constructors for those types
polymorphic, like this:

    data Kind = Input | Output

    data TypeInfo k where
      TIScalar      :: TypeInfo k           -- \ Polymorphic!
      TIEnum        :: ... -> TypeInfo k    -- /
      TIInputObject :: ... -> TypeInfo origin 'Input
      TIObject      :: ... -> TypeInfo origin 'Output

Naturally, this would give the `scalar` parser constructor a similarly
polymorphic type:

    scalar
      :: MonadParse m
      => Name
      -> Maybe Description
      -> ScalarRepresentation a
      -> Parser k m a             -- Polymorphic!

But if we actually try that, we run into problems. The trouble is that we want
to use the Kind to influence several different things:

  * As mentioned above, we use it to ensure that the types we generate are
    well-kinded according to the GraphQL spec rules.

  * We use it to determine what a Parser consumes as input. Parsers for input
    types parse GraphQL input values, but Parsers for output types parse
    selection sets. (See Note [The meaning of Parser 'Output] in
    Hasura.GraphQL.Parser.Internal.Parser for an explanation of why.)

  * We use it to know when to expect a sub-selection set for a field of an
    output object (see Note [The delicate balance of GraphQL kinds]).

These many uses of Kind cause some trouble for a polymorphic representation. For
example, consider our `scalar` parser constructor above---if we were to
instantiate it at kind 'Output, we’d receive a `Parser 'Output`, which we would
then expect to be able to apply to a selection set. But that doesn’t make any
sense, since scalar fields don’t have selection sets!

Another issue with this representation has to do with effectful parser
constructors (such as constructors that can throw errors). These have types like

    mkFooParser :: (MonadMemoize m, MonadParse n) => Blah -> m (Parser k n Foo)

where the parser construction is itself monadic. This causes some annoyance,
since even if mkFooParser returns a Parser of a polymorphic kind, code like this
will not typecheck:

    (fooParser :: forall k. Parser k n Foo) <- mkFooParser blah

The issue is that we have to instantiate k to a particular type to be able to
call mkFooParser. If we want to use the result at both kinds, we’d have to call
mkFooParser twice:

    (fooInputParser :: Parser 'Input n Foo) <- mkFooParser blah
    (fooOutputParser :: Parser 'Output n Foo) <- mkFooParser blah

Other situations encounter similar difficulties, and they are not easy to
resolve without impredicative polymorphism (which GHC does not support).

To avoid this problem, we don’t use polymorphic kinds, but instead introduce a
form of kind subsumption. Types that can be used as both input and output types
are explicitly given the kind 'Both. This allows us to get the best of both
worlds:

  * We use the <: typeclass to accept 'Both in most places where we expect
    either input or output types.

  * We can treat 'Both specially to avoid requiring `scalar` to supply a
    selection set parser (see Note [The delicate balance of GraphQL kinds] for
    further explanation).

  * Because we avoid the polymorphism, we don’t run into the aforementioned
    issue with monadic parser constructors.

All of this is subtle and somewhat complicated, but unfortunately there isn’t
much of a way around that: GraphQL is subtle and complicated. Our use of an
explicit 'Both kind isn’t the only way to encode these things, but it’s the
particular set of compromises we’ve chosen to accept.

