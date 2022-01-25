This note is in [Hasura.GraphQL.Parser.Class](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Class.hs#L25).
It is referenced at:
  - line 54 of [Hasura.GraphQL.Parser.Internal.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Types.hs#L54)
  - line 93 of [Hasura.GraphQL.Parser.Class](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Class.hs#L93)
  - line 105 of [Hasura.GraphQL.Parser.Monad](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Monad.hs#L105)
  - line 424 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L424)
  - line 772 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L772)
  - line 791 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L791)

# Tying the knot

GraphQL type definitions can be mutually recursive, and indeed, they quite often
are! For example, two tables that reference one another will be represented by
types such as the following:

    type author {
      id: Int!
      name: String!
      articles: [article!]!
    }

    type article {
      id: Int!
      title: String!
      content: String!
      author: author!
    }

This doesn’t cause any trouble if the schema is represented by a mapping from
type names to type definitions, but the Parser abstraction is all about avoiding
that kind of indirection to improve type safety — parsers refer to their
sub-parsers directly. This presents two problems during schema generation:

  1. Schema generation needs to terminate in finite time, so we need to ensure
     we don’t try to eagerly construct an infinitely-large schema due to the
     mutually-recursive structure.

  2. To serve introspection queries, we do eventually need to construct a
     mapping from names to types (a TypeMap), so we need to be able to
     recursively walk the entire schema in finite time.

Solving point number 1 could be done with either laziness or sharing, but
neither of those are enough to solve point number 2, which requires /observable/
sharing. We need to construct a Parser graph that contains enough information to
detect cycles during traversal.

It may seem appealing to just use type names to detect cycles, which would allow
us to get away with using laziness rather than true sharing. Unfortunately, that
leads to two further problems:

  * It’s possible to end up with two different types with the same name, which
    is an error and should be reported as such. Using names to break cycles
    prevents us from doing that, since we have no way to check that two types
    with the same name are actually the same.

  * Some Parser constructors can fail — the `column` parser checks that the type
    name is a valid GraphQL name, for example. This extra validation means lazy
    schema construction isn’t viable, since we need to eagerly build the schema
    to ensure all the validation checks hold.

So we’re forced to use sharing. But how do we do it? Somehow, we have to /tie
the knot/ — we have to build a cyclic data structure — and some of the cycles
may be quite large. Doing all this knot-tying by hand would be incredibly
tricky, and it would require a lot of inversion of control to thread the shared
parsers around.

To avoid contorting the program, we instead implement a form of memoization. The
MonadSchema class provides a mechanism to memoize a parser constructor function,
which allows us to get sharing mostly for free. The memoization strategy also
annotates cached parsers with a Unique that can be used to break cycles while
traversing the graph, so we get observable sharing as well.
