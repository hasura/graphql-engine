-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class
  ( MonadParse (..),
    parseError,
    withPath,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.Typeable
import Hasura.Base.ErrorMessage
import Hasura.GraphQL.Parser.ErrorCode
import Prelude

-- | A class that provides functionality for parsing GraphQL queries, i.e.
-- running a fully-constructed 'Parser'.
class (Monad m, Typeable m) => MonadParse m where
  withKey :: J.JSONPathElement -> m a -> m a

  -- | Not the full power of 'MonadError' because parse errors cannot be
  -- caught.
  parseErrorWith :: ParseErrorCode -> ErrorMessage -> m a

withPath :: (MonadParse m) => J.JSONPath -> m a -> m a
withPath path action = foldr withKey action path

parseError :: (MonadParse m) => ErrorMessage -> m a
parseError = parseErrorWith ValidationFailed
