-- | Monad transformers for GraphQL schema construction and query parsing.
module Hasura.GraphQL.Parser.Monad
  ( Parse,
    runParse,
    ParseError (..),
  )
where

import Control.Monad.Except
import Data.Aeson (JSONPath)
import Data.Functor.Identity
import Hasura.Base.ErrorMessage
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.ErrorCode
import Prelude

-- Disable custom prelude warnings in preparation for extracting this module into a separate package.
{-# ANN module ("HLint: ignore Use onLeft" :: String) #-}

-- -------------------------------------------------------------------------------------------------
-- query parsing

newtype Parse a = Parse
  { unParse :: Except ParseError a
  }
  deriving (Functor, Applicative, Monad)

runParse ::
  MonadError ParseError m =>
  Parse a ->
  m a
runParse parse =
  either throwError pure . runExcept $ unParse parse

instance MonadParse Parse where
  withKey key = Parse . withExceptT (\pe -> pe {pePath = key : pePath pe}) . unParse
  parseErrorWith code message = Parse $ do
    throwError $ ParseError {peCode = code, pePath = [], peMessage = message}

data ParseError = ParseError
  { pePath :: JSONPath,
    peMessage :: ErrorMessage,
    peCode :: ParseErrorCode
  }
