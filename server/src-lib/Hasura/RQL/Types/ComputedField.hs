{- |
Description: Schema cache types related to computed field
-}

module Hasura.RQL.Types.ComputedField where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.SQL.Types

import           Control.Lens               hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Sequence              as Seq
import qualified Database.PG.Query          as Q

newtype ComputedFieldName =
  ComputedFieldName { unComputedFieldName :: NonEmptyText}
  deriving (Show, Eq, NFData, Lift, FromJSON, ToJSON, Q.ToPrepArg, DQuote, Hashable, Q.FromCol, Generic, Arbitrary, Cacheable)

computedFieldNameToText :: ComputedFieldName -> Text
computedFieldNameToText = unNonEmptyText . unComputedFieldName

fromComputedField :: ComputedFieldName -> FieldName
fromComputedField = FieldName . computedFieldNameToText

-- | The function table argument is either the very first argument or the named
-- argument with an index. The index is 0 if the named argument is the first.
data FunctionTableArgument
  = FTAFirst
  | FTANamed
    !FunctionArgName -- ^ argument name
    !Int -- ^ argument index
  deriving (Show, Eq, Generic)
instance Cacheable FunctionTableArgument

instance ToJSON FunctionTableArgument where
  toJSON FTAFirst             = String "first_argument"
  toJSON (FTANamed argName _) = object ["name" .= argName]

-- | The session argument, which passes Hasura session variables to a
-- SQL function as a JSON object.
data FunctionSessionArgument
  = FunctionSessionArgument !FunctionArgName !Int
  deriving (Show, Eq, Generic)
instance Cacheable FunctionSessionArgument

instance ToJSON FunctionSessionArgument where
  -- TODO is this the right JSON to output?
  toJSON (FunctionSessionArgument argName _) = object ["name" .= argName]

data ComputedFieldReturn
  = CFRScalar !PGScalarType
  | CFRSetofTable !QualifiedTable
  deriving (Show, Eq, Generic)
instance Cacheable ComputedFieldReturn
$(deriveToJSON defaultOptions { constructorTagModifier = snakeCase . drop 3
                              , sumEncoding = TaggedObject "type" "info"
                              }
   ''ComputedFieldReturn
 )
$(makePrisms ''ComputedFieldReturn)

data ComputedFieldFunction
  = ComputedFieldFunction
  { _cffName            :: !QualifiedFunction
  , _cffInputArgs       :: !(Seq.Seq FunctionArg)
  , _cffTableArgument   :: !FunctionTableArgument
  , _cffSessionArgument :: !(Maybe FunctionSessionArgument)
  , _cffDescription     :: !(Maybe PGDescription)
  } deriving (Show, Eq, Generic)
instance Cacheable ComputedFieldFunction
$(deriveToJSON (aesonDrop 4 snakeCase) ''ComputedFieldFunction)

data ComputedFieldInfo
  = ComputedFieldInfo
  { _cfiName       :: !ComputedFieldName
  , _cfiFunction   :: !ComputedFieldFunction
  , _cfiReturnType :: !ComputedFieldReturn
  , _cfiComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance Cacheable ComputedFieldInfo
$(deriveToJSON (aesonDrop 4 snakeCase) ''ComputedFieldInfo)
$(makeLenses ''ComputedFieldInfo)

onlyScalarComputedFields :: [ComputedFieldInfo] -> [ComputedFieldInfo]
onlyScalarComputedFields = filter (has (cfiReturnType._CFRScalar))
