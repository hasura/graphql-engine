{-# LANGUAGE StrictData          #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Types where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax      hiding (Definition)

import           Hasura.GraphQL.Parser.Schema


-- -----------------------------------------------------------------------------
-- type definitions

-- | A 'Parser' that corresponds to a type in the GraphQL schema. A 'Parser' is
-- really two things at once:
--
--   1. As its name implies, a 'Parser' can be used to parse GraphQL queries
--      (via 'runParser').
--
--   2. Less obviously, a 'Parser' represents a slice of the GraphQL schema,
--      since every 'Parser' corresponds to a particular GraphQL type, and
--      information about that type can be recovered (via 'parserType').
--
-- A natural way to view this is that 'Parser's support a sort of dynamic
-- reflection: in addition to running a 'Parser' on an input query, you can ask
-- it to tell you about what type of input it expects. Importantly, you can do
-- this even if you don’t have a query to parse; this is necessary to implement
-- GraphQL introspection, which provides precisely this sort of reflection on
-- types.
--
-- Another way of viewing a 'Parser' is a little more quantum: just as light
-- “sometimes behaves like a particle and sometimes behaves like a wave,” a
-- 'Parser' “sometimes behaves like a query parser and sometimes behaves like a
-- type.” In this way, you can think of a function that produces a 'Parser' as
-- simultaneously both a function that constructs a GraphQL schema and a
-- function that parses a GraphQL query. 'Parser' constructors therefore
-- interleave two concerns: information about a type definition (like the type’s
-- name and description) and information about how to parse a query on that type.
--
-- Notably, these two concerns happen at totally different phases in the
-- program: GraphQL schema construction happens when @graphql-engine@ first
-- starts up, before it receives any GraphQL queries at all. But query parsing
-- obviously can’t happen until there is actually a query to parse. For that
-- reason, it’s useful to take care to distinguish which effects are happening
-- at which phase during 'Parser' construction, since otherwise you may get
-- mixed up!
--
-- For some more information about how to interpret the meaning of a 'Parser',
-- see Note [The meaning of Parser 'Output].
data Parser k m a = Parser
  { pType   :: ~(Type k)
  -- ^ Lazy for knot-tying reasons; see Note [Tying the knot] in
  -- Hasura.GraphQL.Parser.Class.
  , pParser :: ParserInput k -> m a
  } deriving (Functor)

instance HasName (Parser k m a) where
  getName = getName . pType

instance HasDefinition (Parser k m a) (TypeInfo k) where
  definitionLens f parser = definitionLens f (pType parser) <&> \pType -> parser { pType }

type family ParserInput k where
  -- see Note [The 'Both kind] in Hasura.GraphQL.Parser.Schema
  ParserInput 'Both = InputValue Variable
  ParserInput 'Input = InputValue Variable
  -- see Note [The meaning of Parser 'Output]
  ParserInput 'Output = SelectionSet NoFragments Variable

parserType :: Parser k m a -> Type k
parserType = pType

runParser :: Parser k m a -> ParserInput k -> m a
runParser = pParser

{- Note [The meaning of Parser 'Output]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
output type isn’t an output value but a selection set. -}
