module Hasura.GraphQL.Parser.ErrorCode (ParseErrorCode (..)) where

import Prelude

data ParseErrorCode
  = ValidationFailed
  | ParseFailed
  | ConflictingDefinitionsError
  | NotSupported
  deriving stock (Eq, Show)
