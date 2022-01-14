This note is in [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L201).
It is referenced at:
  - line 257 of [Hasura.GraphQL.Parser.Internal.Parser](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Parser.hs#L257)
  - line 314 of [Hasura.GraphQL.Parser.Internal.Parser](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Parser.hs#L314)
  - line 152 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L152)
  - line 190 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L190)

# The delicate balance of GraphQL kinds

As discussed in Note [The 'Both kind], we use GraphQL kinds to distinguish
several different things. One of them is which output types take sub-selection
sets. For example, scalars don’t accept sub-selection sets, so if we have a
schema like

    type Query {
      users: [User!]!
    }

    type User {
      id: Int!
    }

then the following query is illegal:

    query {
      users {
        id {
          blah
        }
      }
    }

The id field has a scalar type, so it should not take a sub-selection set. This
is actually something we care about distinguishing at the type level, because it
affects the type of the `selection` parser combinator. Suppose we have a
`Parser 'Output m UserQuery` for the User type. When we parse a field with that
type, we expect to receive a UserQuery as a result, unsurprisingly. But what if
we parse an output field using the `int` parser, which has this type:

    int :: MonadParse m => Parser 'Both m Int32

If we follow the same logic as for the User parser above, we’d expect to receive
an Int32 as a result... but that doesn’t make any sense, since the Int32
corresponds to the result *we* are suppose to produce as a result of executing
the query, not something user-specified.

One way to solve this would be to associate every Parser with two result types:
one when given an input object, and one when given a selection set. Then our
parsers could be given these types, instead:

    user :: MonadParse m => Parser 'Output m Void UserQuery
    int :: MonadParse m => Parser 'Both m Int32 ()

But if you work through this, you’ll find that *all* parsers will either have
Void or () for at least one of their input result types or their output result
types, depending on their kind:

  * All 'Input parsers must have Void for their output result type, since they
    aren’t allowed to be used in output contexts at all.

  * All 'Output parsers must have Void for their input result type, since they
    aren’t allowed to be used in input contexts at all.

  * That just leaves 'Both. The only types of kind 'Both are scalars and enums,
    neither of which accept a sub-selection set. Their output result type would
    therefore be (), since they are allowed to appear in output contexts, but
    they don’t return any results.

The end result of this is that we clutter all our types with Voids and ()s, with
little actual benefit.

If you really think about it, the fact that the no types of kind 'Both accept a
sub-selection set is really something of a coincidence. In theory, one could
imagine a future version of the GraphQL spec adding a type that can be used as
both an input type or an output type, but accepts a sub-selection set. If that
ever happens, we’ll have to tweak our encoding, but for now, we can take
advantage of this happy coincidence and make the kinds serve double duty:

  * We can make `ParserInput 'Both` identical to `ParserInput 'Input`, since
    all parsers of kind 'Both only parse input values.

  * We can require types of kind 'Both in `selection`, which does not expect a
    sub-selection set, and types of kind 'Output in `subselection`, which does.

Relying on this coincidence might seem a little gross, and perhaps it is
somewhat. But it’s enormously convenient: not doing this would make some types
significantly more complicated, since we would have to thread around more
information at the type level and we couldn’t make as many simplifying
assumptions. So until GraphQL adds a type that violates these assumptions, we
are happy to take advantage of this coincidence.
