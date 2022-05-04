-- | Postgres types ComputedFieldDefinition
--
-- Computed field metadata definition for a Postgres backend
module Hasura.Backends.Postgres.Types.ComputedFieldDefinition
  ( ComputedFieldDefinition (..),
  )
where

import Data.Aeson.Extended
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Function

data ComputedFieldDefinition = ComputedFieldDefinition
  { _cfdFunction :: !QualifiedFunction,
    _cfdTableArgument :: !(Maybe FunctionArgName),
    _cfdSessionArgument :: !(Maybe FunctionArgName)
  }
  deriving (Show, Eq, Generic)

instance NFData ComputedFieldDefinition

instance Hashable ComputedFieldDefinition

instance Cacheable ComputedFieldDefinition

instance ToJSON ComputedFieldDefinition where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON ComputedFieldDefinition where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}
