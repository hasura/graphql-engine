{- |
Description: Schema cache types related to computed field
-}

module Hasura.RQL.Types.ComputedField where

import           Hasura.Prelude

import qualified Data.Sequence                      as Seq
import qualified Database.PG.Query                  as Q

import           Control.Lens                       hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.Backends.Postgres.SQL.Types hiding (FunctionName, TableName)
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.SQL.Backend


newtype ComputedFieldName =
  ComputedFieldName { unComputedFieldName :: NonEmptyText}
  deriving (Show, Eq, Ord, NFData, FromJSON, ToJSON, ToJSONKey
           , Q.ToPrepArg, ToTxt, Hashable, Q.FromCol, Generic, Arbitrary, Cacheable)

computedFieldNameToText :: ComputedFieldName -> Text
computedFieldNameToText = unNonEmptyText . unComputedFieldName

fromComputedField :: ComputedFieldName -> FieldName
fromComputedField = FieldName . computedFieldNameToText

data ComputedFieldDefinition b
  = ComputedFieldDefinition
  { _cfdFunction        :: !(FunctionName b)
  , _cfdTableArgument   :: !(Maybe FunctionArgName)
  , _cfdSessionArgument :: !(Maybe FunctionArgName)
  } deriving (Generic)
deriving instance (Backend b) => Show (ComputedFieldDefinition b)
deriving instance (Backend b) => Eq (ComputedFieldDefinition b)
instance (Backend b) => NFData (ComputedFieldDefinition b)
instance (Backend b) => Cacheable (ComputedFieldDefinition b)
instance (Backend b) => ToJSON (ComputedFieldDefinition b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields = True}
instance (Backend b) => FromJSON (ComputedFieldDefinition b) where
  parseJSON = genericParseJSON hasuraJSON{omitNothingFields = True}

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
  = FunctionSessionArgument
    !FunctionArgName -- ^ The argument name
    !Int -- ^ The ordinal position in the function input parameters
  deriving (Show, Eq, Generic)
instance Cacheable FunctionSessionArgument

instance ToJSON FunctionSessionArgument where
  toJSON (FunctionSessionArgument argName _) = toJSON argName

data ComputedFieldReturn (b :: BackendType)
  = CFRScalar !(ScalarType b)
  | CFRSetofTable !(TableName b)
  deriving (Generic)
deriving instance Backend b => Show (ComputedFieldReturn b)
deriving instance Backend b => Eq (ComputedFieldReturn b)
instance Backend b => Cacheable (ComputedFieldReturn b)
instance Backend b => ToJSON (ComputedFieldReturn b) where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = snakeCase . drop 3
                   , sumEncoding = TaggedObject "type" "info"
                   }
$(makePrisms ''ComputedFieldReturn)

data ComputedFieldFunction (b :: BackendType)
  = ComputedFieldFunction
  { _cffName            :: !QualifiedFunction
  , _cffInputArgs       :: !(Seq.Seq (FunctionArg b))
  , _cffTableArgument   :: !FunctionTableArgument
  , _cffSessionArgument :: !(Maybe FunctionSessionArgument)
  , _cffDescription     :: !(Maybe PGDescription)
  } deriving (Show, Eq, Generic)
instance (Backend b) => Cacheable (ComputedFieldFunction b)
instance (Backend b) => ToJSON (ComputedFieldFunction b) where
  toJSON = genericToJSON hasuraJSON

data ComputedFieldInfo (b :: BackendType)
  = ComputedFieldInfo
  { _cfiXComputedFieldInfo :: !(XComputedField b)
  , _cfiName               :: !ComputedFieldName
  , _cfiFunction           :: !(ComputedFieldFunction b)
  , _cfiReturnType         :: !(ComputedFieldReturn b)
  , _cfiComment            :: !(Maybe Text)
  } deriving (Generic)
deriving instance (Backend b) => Eq (ComputedFieldInfo b)
deriving instance (Backend b) => Show (ComputedFieldInfo b)
instance (Backend b) => Cacheable (ComputedFieldInfo b)
instance (Backend b) => ToJSON (ComputedFieldInfo b) where
  -- spelling out the JSON instance in order to skip the Trees That Grow field
  toJSON (ComputedFieldInfo _ name func tp comment) =
    object ["name" .= name, "function" .= func, "return_type" .= tp, "comment" .= comment]
$(makeLenses ''ComputedFieldInfo)

onlyScalarComputedFields :: [ComputedFieldInfo backend] -> [ComputedFieldInfo backend]
onlyScalarComputedFields = filter (has (cfiReturnType._CFRScalar))
