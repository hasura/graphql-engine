-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class.Parse where

import           Hasura.Prelude

import           Data.Parser.JSONPath

import           Hasura.RQL.Types.Error

-- | A class that provides functionality for parsing GraphQL queries, i.e.
-- running a fully-constructed 'Parser'.
class Monad m => MonadParse m where
  withPath :: (JSONPath -> JSONPath) -> m a -> m a
  -- | Not the full power of 'MonadError' because parse errors cannot be
  -- caught.
  parseErrorWith :: Code -> Text -> m a
  -- | See 'QueryReusability'.
  markNotReusable :: m ()

parseError :: MonadParse m => Text -> m a
parseError = parseErrorWith ValidationFailed

-- | Tracks whether or not a query is /reusable/. Reusable queries are nice,
-- since we can cache their resolved ASTs and avoid re-resolving them if we
-- receive an identical query. However, we can’t always safely reuse queries if
-- they have variables, since some variable values can affect the generated SQL.
-- For example, consider the following query:
--
-- > query users_where($condition: users_bool_exp!) {
-- >   users(where: $condition) {
-- >     id
-- >   }
-- > }
--
-- Different values for @$condition@ will produce completely different queries,
-- so we can’t reuse its plan (unless the variable values were also all
-- identical, of course, but we don’t bother caching those).
data QueryReusability = Reusable | NotReusable

instance Semigroup QueryReusability where
  NotReusable <> _           = NotReusable
  _           <> NotReusable = NotReusable
  Reusable    <> Reusable    = Reusable

instance Monoid QueryReusability where
  mempty = Reusable
