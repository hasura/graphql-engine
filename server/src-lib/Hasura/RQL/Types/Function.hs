module Hasura.RQL.Types.Function where

import           Hasura.Prelude

import qualified Data.HashSet                       as Set
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Char                          (toLower)
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.Types as PG

import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend
import           Hasura.Session

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

data FunctionArg (b :: BackendType)
  = FunctionArg
  { faName       :: !(Maybe FunctionArgName)
  , faType       :: !(FunctionArgType b)
  , faHasDefault :: !HasDefault
  } deriving (Generic)
deriving instance Backend b => Show (FunctionArg b)
deriving instance Backend b => Eq   (FunctionArg b)
instance Backend b => Cacheable (FunctionArg b)
instance (Backend b) => ToJSON (FunctionArg b) where
  toJSON = genericToJSON hasuraJSON

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

type FunctionInputArgument b = InputArgument (FunctionArg b)


-- | Indicates whether the user requested the corresponding function to be
-- tracked as a mutation or a query/subscription, in @track_function@.
data FunctionExposedAs = FEAQuery | FEAMutation
  deriving (Show, Eq, Generic)

instance NFData FunctionExposedAs
instance Cacheable FunctionExposedAs
$(deriveJSON
    defaultOptions{ sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3 }
    ''FunctionExposedAs)


-- | Tracked SQL function metadata. See 'buildFunctionInfo'.
data FunctionInfo (b :: BackendType)
  = FunctionInfo
  { _fiName          :: !(FunctionName b)
  , _fiSystemDefined :: !SystemDefined
  , _fiVolatility    :: !FunctionVolatility
  , _fiExposedAs     :: !FunctionExposedAs
  -- ^ In which part of the schema should this function be exposed?
  --
  -- See 'mkFunctionInfo' and '_fcExposedAs'.
  , _fiInputArgs     :: !(Seq.Seq (FunctionInputArgument b))
  , _fiReturnType    :: !(TableName b)
  -- ^ NOTE: when a table is created, a new composite type of the same name is
  -- automatically created; so strictly speaking this field means "the function
  -- returns the composite type corresponding to this table".
  , _fiDescription   :: !(Maybe PG.PGDescription) -- FIXME: make generic
  , _fiPermissions   :: !(Set.HashSet RoleName)
  , _fiJsonAggSelect :: !JsonAggSelect
  -- ^ Roles to which the function is accessible
  } deriving (Generic)
deriving instance Backend b => Show (FunctionInfo b)
deriving instance Backend b => Eq   (FunctionInfo b)
instance (Backend b) => ToJSON (FunctionInfo b) where
  toJSON = genericToJSON hasuraJSON
$(makeLenses ''FunctionInfo)

getInputArgs :: FunctionInfo b -> Seq.Seq (FunctionArg b)
getInputArgs =
  Seq.fromList . mapMaybe (^? _IAUserProvided) . toList . _fiInputArgs

type FunctionCache b = HashMap (FunctionName b) (FunctionInfo b) -- info of all functions

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
$(deriveJSON hasuraJSON{omitNothingFields = True} ''FunctionConfig)

-- | The default function config; v1 of the API implies this.
emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing Nothing


-- | JSON API payload for v2 of 'track_function':
--
-- https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#track-function-v2
data TrackFunctionV2
  = TrackFunctionV2
  { _tfv2Source        :: !SourceName
  , _tfv2Function      :: !PG.QualifiedFunction
  , _tfv2Configuration :: !FunctionConfig
  } deriving (Show, Eq, Generic)
$(deriveToJSON hasuraJSON ''TrackFunctionV2)

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
  , rfiReturnTypeSchema :: !PG.SchemaName
  , rfiReturnTypeName   :: !PG.PGScalarType
  , rfiReturnTypeType   :: !PG.PGTypeKind
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![PG.QualifiedPGType]
  , rfiInputArgNames    :: ![FunctionArgName]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PG.PGDescription)
  } deriving (Show, Eq, Generic)
instance NFData RawFunctionInfo
instance Cacheable RawFunctionInfo
$(deriveJSON hasuraJSON ''RawFunctionInfo)

type DBFunctionsMetadata b = HashMap (FunctionName b) [RawFunctionInfo] -- TODO: Generalize RawFunctionInfo

data FunctionPermissionsCtx
  = FunctionPermissionsInferred
  | FunctionPermissionsManual
  deriving (Show, Eq)

instance FromJSON FunctionPermissionsCtx where
  parseJSON = withBool "FunctionPermissionsCtx" $
    pure . bool FunctionPermissionsManual FunctionPermissionsInferred

instance ToJSON FunctionPermissionsCtx where
  toJSON = \case
    FunctionPermissionsInferred -> Bool True
    FunctionPermissionsManual   -> Bool False

newtype FunctionPermissionMetadata
  = FunctionPermissionMetadata
  { _fpmRole       :: RoleName
  } deriving (Show, Eq, Generic)
instance Cacheable FunctionPermissionMetadata
$(makeLenses ''FunctionPermissionMetadata)
$(deriveJSON hasuraJSON ''FunctionPermissionMetadata)
