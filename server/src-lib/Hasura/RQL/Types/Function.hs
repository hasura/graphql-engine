module Hasura.RQL.Types.Function where

import           Hasura.Prelude

import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Char                          (toLower)
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types.Common


-- | https://www.postgresql.org/docs/current/xfunc-volatility.html
data FunctionVolatility
  = FTVOLATILE
  | FTIMMUTABLE
  | FTSTABLE
  deriving (Eq, Generic)
instance NFData FunctionVolatility
instance Cacheable FunctionVolatility
$(deriveJSON defaultOptions{constructorTagModifier = drop 2} ''FunctionVolatility)

funcTypToTxt :: FunctionVolatility -> Text
funcTypToTxt FTVOLATILE  = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE    = "STABLE"

instance Show FunctionVolatility where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName =
  FunctionArgName { getFuncArgNameTxt :: Text}
  deriving (Show, Eq, NFData, ToJSON, FromJSON, ToTxt, IsString, Generic, Arbitrary, Cacheable)

newtype HasDefault = HasDefault { unHasDefault :: Bool }
  deriving (Show, Eq, ToJSON, Cacheable)

data FunctionArg
  = FunctionArg
  { faName       :: !(Maybe FunctionArgName)
  , faType       :: !QualifiedPGType
  , faHasDefault :: !HasDefault
  } deriving (Show, Eq, Generic)
instance Cacheable FunctionArg
$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionArg)

data InputArgument a
  = IAUserProvided !a
  | IASessionVariables !FunctionArgName
  deriving (Show, Eq, Functor)
$(deriveToJSON defaultOptions
               { constructorTagModifier = snakeCase . drop 2
               , sumEncoding = TaggedObject "type" "argument"
               }
 ''InputArgument
 )
$(makePrisms ''InputArgument)

type FunctionInputArgument = InputArgument FunctionArg


-- | Indicates whether the user requested the corresponding function to be
-- tracked as a mutation or a query/subscription, in @track_function@.
data FunctionExposedAs = FEAQuery | FEAMutation
  deriving (Show, Eq, Generic)

instance NFData FunctionExposedAs
instance Cacheable FunctionExposedAs
$(deriveJSON
    defaultOptions{ sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3 }
    ''FunctionExposedAs)


-- | Tracked SQL function metadata. See 'mkFunctionInfo'.
data FunctionInfo
  = FunctionInfo
  { fiName          :: !QualifiedFunction
  , fiSystemDefined :: !SystemDefined
  , fiVolatility    :: !FunctionVolatility
  , fiExposedAs     :: !FunctionExposedAs
  -- ^ In which part of the schema should this function be exposed?
  --
  -- See 'mkFunctionInfo' and '_fcExposedAs'.
  , fiInputArgs     :: !(Seq.Seq FunctionInputArgument)
  , fiReturnType    :: !QualifiedTable
  -- ^ NOTE: when a table is created, a new composite type of the same name is
  -- automatically created; so strictly speaking this field means "the function
  -- returns the composite type corresponding to this table".
  , fiDescription   :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''FunctionInfo)

getInputArgs :: FunctionInfo -> Seq.Seq FunctionArg
getInputArgs =
  Seq.fromList . mapMaybe (^? _IAUserProvided) . toList . fiInputArgs

type FunctionCache = HashMap QualifiedFunction FunctionInfo -- info of all functions

-- Metadata requests related types

-- | Tracked function configuration, and payload of the 'track_function' API call.
data FunctionConfig
  = FunctionConfig
  { _fcSessionArgument :: !(Maybe FunctionArgName)
  , _fcExposedAs       :: !(Maybe FunctionExposedAs)
  -- ^ In which top-level field should we expose this function?
  --
  -- The user might omit this, in which case we'll infer the location from the
  -- SQL functions volatility. See 'mkFunctionInfo' or the @track_function@ API
  -- docs for details of validation, etc.
  } deriving (Show, Eq, Generic)
instance NFData FunctionConfig
instance Cacheable FunctionConfig
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields = True} ''FunctionConfig)

-- | The default function config; v1 of the API implies this.
emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing Nothing


-- | JSON API payload for v2 of 'track_function':
--
-- https://hasura.io/docs/1.0/graphql/core/api-reference/schema-metadata-api/custom-functions.html#track-function-v2
data TrackFunctionV2
  = TrackFunctionV2
  { _tfv2Source        :: !SourceName
  , _tfv2Function      :: !QualifiedFunction
  , _tfv2Configuration :: !FunctionConfig
  } deriving (Show, Eq, Generic)
$(deriveToJSON (aesonDrop 5 snakeCase) ''TrackFunctionV2)

instance FromJSON TrackFunctionV2 where
  parseJSON = withObject "Object" $ \o ->
    TrackFunctionV2
    <$> o .:? "source" .!= defaultSource
    <*> o .: "function"
    <*> o .:? "configuration" .!= emptyFunctionConfig

-- | Raw SQL function metadata from postgres
data RawFunctionInfo
  = RawFunctionInfo
  { rfiOid              :: !OID
  , rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionVolatility
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !PGScalarType
  , rfiReturnTypeType   :: !PGTypeKind
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![QualifiedPGType]
  , rfiInputArgNames    :: ![FunctionArgName]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PGDescription)
  } deriving (Show, Eq, Generic)
instance NFData RawFunctionInfo
instance Cacheable RawFunctionInfo
$(deriveJSON (aesonDrop 3 snakeCase) ''RawFunctionInfo)

type PostgresFunctionsMetadata = HashMap QualifiedFunction [RawFunctionInfo]
