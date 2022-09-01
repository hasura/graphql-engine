{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.GraphQL.Parser.TestInstances () where

import Data.Text qualified as Text
import Hasura.Base.ErrorMessage (ErrorMessage, fromErrorMessage)
import Hasura.GraphQL.Parser.Monad (ParseError (..))

-- Orphan instances so that we can write assertions over 'Either ErrorMessage a'.
instance Show ErrorMessage where
  show = Text.unpack . fromErrorMessage

-- Orphan instances so that we can write assertions over 'Either ParseError a'.
deriving stock instance Eq ParseError

deriving stock instance Show ParseError
