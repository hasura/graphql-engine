This note is in [Hasura.GraphQL.Parser.Internal.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Types.hs#L79).
It is referenced at:
  - line 54 of [Hasura.GraphQL.Parser.Internal.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Types.hs#L54)
  - line 70 of [Hasura.GraphQL.Parser.Internal.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Internal/Types.hs#L70)
  - line 129 of [Hasura.GraphQL.Parser.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Parser/Schema.hs#L129)

# The meaning of Parser 'Output

The ParserInput type family determines what a Parser accepts as input during
query parsing, which varies based on its Kind. A `Parser 'Input`,
unsurprisingly, parses GraphQL input values, much in the same way aeson
`Parser`s parse JSON values.

Therefore, one might naturally conclude that `Parser 'Output` ought to parse
GraphQL output values. But it doesn’t---a Parser is used to parse GraphQL
*queries*, and output values don’t show up in queries anywhere! Rather, the
output values are the results of executing the query, not something the user
sends us, so we don’t have to parse those at all.

What output types really correspond to in GraphQL queries is selection sets. For
example, if we have the GraphQL types

    type User {
      posts(filters: PostFilters): [Post]
    }

    input PostFilters {
      newer_than: Date
    }

    type Post {
      id: Int
      title: String
      body: String
    }

then we might receive a query that looks like this:

    query list_user_posts($user_id: Int, $date: Date) {
      user_by_id(id: $user_id) {
        posts(filters: {newer_than: $date}) {
          id
          title
        }
      }
    }

We have Parsers to represent each of these types: a `Parser 'Input` for
PostFilters, and two `Parser 'Output`s for User and Post. When we parse the
query, we pass the `{newer_than: $date}` input value to the PostFilters parser,
as expected. But what do we pass to the User parser? The answer is this
selection set:

    {
      posts(filters: {newer_than: $date}) {
        id
        title
      }
    }

Likewise, the Post parser eventually receives the inner selection set:

    {
      id
      title
    }

These Parsers handle interpreting the fields of the selection sets. This is why
`ParserInput 'Output` is SelectionSet---the GraphQL *type* associated with the
Parser is an output type, but the part of the *query* that corresponds to that
output type isn’t an output value but a selection set.
