{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.GraphQL.Parser.TestInstances () where

import Hasura.Base.ErrorMessage (fromErrorMessage)
import Hasura.GraphQL.Parser.Monad (ParseError (..))
import Hasura.Prelude

-- Orphan instances so that we can write assertions over 'Either ParseError a'.
deriving stock instance Eq ParseError

-- This cannot be automatically derived because 'ErrorMessage' doesn't have a 'Show' instance.
instance Show ParseError where
  show ParseError {pePath, peMessage, peCode} =
    "ParseError { pePath = " <> show pePath <> ", peMessage = " <> show (fromErrorMessage peMessage) <> ", peCode = " <> show peCode <> "}"
