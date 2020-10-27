{- |
Description: Schema cache types related to computed field
-}

module Hasura.RQL.Types.ComputedField where


import           Hasura.Prelude

import qualified Data.Sequence              as Seq
import qualified Database.PG.Query          as Q

import           Control.Lens               hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import           Hasura.Incremental         (Cacheable)
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Function
import           Hasura.SQL.Backend
import           Hasura.SQL.Types


newtype ComputedFieldName =
  ComputedFieldName { unComputedFieldName :: NonEmptyText}
  deriving (Show, Eq, NFData, Lift, FromJSON, ToJSON, Q.ToPrepArg, ToTxt, Hashable, Q.FromCol, Generic, Arbitrary, Cacheable)

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
  = FunctionSessionArgument
    !FunctionArgName -- ^ The argument name
    !Int -- ^ The ordinal position in the function input parameters
  deriving (Show, Eq, Generic)
instance Cacheable FunctionSessionArgument

instance ToJSON FunctionSessionArgument where
  toJSON (FunctionSessionArgument argName _) = toJSON argName

data ComputedFieldReturn (b :: Backend)
  = CFRScalar !(ScalarType b)
  | CFRSetofTable !QualifiedTable
  deriving (Generic)
deriving instance Show (ComputedFieldReturn 'Postgres)
deriving instance Eq (ComputedFieldReturn 'Postgres)
instance Cacheable (ComputedFieldReturn 'Postgres)
instance ToJSON (ComputedFieldReturn 'Postgres) where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = snakeCase . drop 3
                   , sumEncoding = TaggedObject "type" "info"
                   }
  toEncoding = genericToEncoding $
    defaultOptions { constructorTagModifier = snakeCase . drop 3
                   , sumEncoding = TaggedObject "type" "info"
                   }
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

data ComputedFieldInfo (b :: Backend)
  = ComputedFieldInfo
  { _cfiName       :: !ComputedFieldName
  , _cfiFunction   :: !ComputedFieldFunction
  , _cfiReturnType :: !(ComputedFieldReturn b)
  , _cfiComment    :: !(Maybe Text)
  } deriving (Generic)
deriving instance Eq (ComputedFieldInfo 'Postgres)
instance Cacheable (ComputedFieldInfo 'Postgres)
instance ToJSON (ComputedFieldInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase
  toEncoding = genericToEncoding $ aesonDrop 4 snakeCase
$(makeLenses ''ComputedFieldInfo)

onlyScalarComputedFields :: [ComputedFieldInfo backend] -> [ComputedFieldInfo backend]
onlyScalarComputedFields = filter (has (cfiReturnType._CFRScalar))
