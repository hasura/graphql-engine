-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class.Parse
  ( MonadParse (..),
    parseError,
    withPath,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Hasura.Base.ErrorMessage
import Hasura.GraphQL.Parser.ErrorCode
import Prelude

-- | A class that provides functionality for parsing GraphQL queries, i.e.
-- running a fully-constructed 'Parser'.
class Monad m => MonadParse m where
  withKey :: Aeson.JSONPathElement -> m a -> m a

  -- | Not the full power of 'MonadError' because parse errors cannot be
  -- caught.
  parseErrorWith :: ParseErrorCode -> ErrorMessage -> m a

withPath :: MonadParse m => Aeson.JSONPath -> m a -> m a
withPath path action = foldr withKey action path

parseError :: MonadParse m => ErrorMessage -> m a
parseError = parseErrorWith ValidationFailed
