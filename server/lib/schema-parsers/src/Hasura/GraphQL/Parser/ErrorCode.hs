module Hasura.GraphQL.Parser.ErrorCode (ParseErrorCode (..)) where

data ParseErrorCode
  = ValidationFailed
  | ParseFailed
  | ConflictingDefinitionsError
  | NotSupported
  deriving stock (Eq, Show)
