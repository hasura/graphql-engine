-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class.Parse where

import Data.Parser.JSONPath
import Hasura.Base.Error
import Hasura.Prelude

-- | A class that provides functionality for parsing GraphQL queries, i.e.
-- running a fully-constructed 'Parser'.
class Monad m => MonadParse m where
  withPath :: (JSONPath -> JSONPath) -> m a -> m a

  -- | Not the full power of 'MonadError' because parse errors cannot be
  -- caught.
  parseErrorWith :: Code -> Text -> m a

parseError :: MonadParse m => Text -> m a
parseError = parseErrorWith ValidationFailed
