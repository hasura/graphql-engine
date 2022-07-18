-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class.Parse
  ( MonadParse (..),
    parseError,
  )
where

import Data.Aeson.Types (JSONPathElement)
import Hasura.Base.Error
import Hasura.Base.ErrorMessage
import Prelude

-- | A class that provides functionality for parsing GraphQL queries, i.e.
-- running a fully-constructed 'Parser'.
class Monad m => MonadParse m where
  withKey :: JSONPathElement -> m a -> m a

  -- | Not the full power of 'MonadError' because parse errors cannot be
  -- caught.
  parseErrorWith :: Code -> ErrorMessage -> m a

parseError :: MonadParse m => ErrorMessage -> m a
parseError = parseErrorWith ValidationFailed
