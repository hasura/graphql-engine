{-# LANGUAGE TemplateHaskell #-}

-- | Postgres types related to computed fields
module Hasura.Backends.Postgres.Types.ComputedField
  ( ComputedFieldDefinition (..),
    FunctionTableArgument (..),
    FunctionSessionArgument (..),
    ComputedFieldImplicitArguments (..),
    fromComputedFieldImplicitArguments,
    ComputedFieldReturn (..),
    _CFRScalar,
    _CFRSetofTable,
  )
where

import Control.Lens.TH (makePrisms)
import Data.Aeson.Casing
import Data.Aeson.Extended
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Types.Function
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

-- | The function table argument is either the very first argument or the named
-- argument with an index. The index is 0 if the named argument is the first.
data FunctionTableArgument
  = FTAFirst
  | FTANamed
      !FunctionArgName
      -- ^ argument name
      !Int
      -- ^ argument index
  deriving (Show, Eq, Generic)

instance Cacheable FunctionTableArgument

instance NFData FunctionTableArgument

instance Hashable FunctionTableArgument

instance ToJSON FunctionTableArgument where
  toJSON FTAFirst = String "first_argument"
  toJSON (FTANamed argName _) = object ["name" .= argName]

-- | The session argument, which passes Hasura session variables to a
-- SQL function as a JSON object.
data FunctionSessionArgument
  = FunctionSessionArgument
      !FunctionArgName
      -- ^ The argument name
      !Int
      -- ^ The ordinal position in the function input parameters
  deriving (Show, Eq, Generic)

instance Cacheable FunctionSessionArgument

instance NFData FunctionSessionArgument

instance Hashable FunctionSessionArgument

instance ToJSON FunctionSessionArgument where
  toJSON (FunctionSessionArgument argName _) = toJSON argName

data ComputedFieldImplicitArguments = ComputedFieldImplicitArguments
  { _cffaTableArgument :: FunctionTableArgument,
    _cffaSessionArgument :: Maybe FunctionSessionArgument
  }
  deriving stock (Show, Eq, Generic)

instance NFData ComputedFieldImplicitArguments

instance Hashable ComputedFieldImplicitArguments

instance Cacheable ComputedFieldImplicitArguments

instance ToJSON ComputedFieldImplicitArguments where
  toJSON = genericToJSON hasuraJSON

fromComputedFieldImplicitArguments ::
  v ->
  ComputedFieldImplicitArguments ->
  [ArgumentExp v]
fromComputedFieldImplicitArguments _ (ComputedFieldImplicitArguments _ Nothing) = [AETableRow] -- No session argument
fromComputedFieldImplicitArguments sess (ComputedFieldImplicitArguments FTAFirst _) = [AETableRow, AESession sess]
fromComputedFieldImplicitArguments sess (ComputedFieldImplicitArguments (FTANamed _ 0) _) = [AETableRow, AESession sess] -- Index is 0 implies table argument is first
fromComputedFieldImplicitArguments sess _ = [AESession sess, AETableRow]

data ComputedFieldReturn
  = CFRScalar PGScalarType
  | CFRSetofTable QualifiedTable
  deriving (Show, Eq, Generic)

instance Cacheable ComputedFieldReturn

instance NFData ComputedFieldReturn

instance Hashable ComputedFieldReturn

instance ToJSON ComputedFieldReturn where
  toJSON =
    genericToJSON $
      defaultOptions
        { constructorTagModifier = snakeCase . drop 3,
          sumEncoding = TaggedObject "type" "info"
        }

$(makePrisms ''ComputedFieldReturn)
