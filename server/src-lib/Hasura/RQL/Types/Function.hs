module Hasura.RQL.Types.Function where

import Control.Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Hasura.Session

-- | https://www.postgresql.org/docs/current/xfunc-volatility.html
data FunctionVolatility
  = FTVOLATILE
  | FTIMMUTABLE
  | FTSTABLE
  deriving (Eq, Generic)

instance NFData FunctionVolatility

instance Cacheable FunctionVolatility

$(deriveJSON defaultOptions {constructorTagModifier = drop 2} ''FunctionVolatility)

funcTypToTxt :: FunctionVolatility -> Text
funcTypToTxt FTVOLATILE = "VOLATILE"
funcTypToTxt FTIMMUTABLE = "IMMUTABLE"
funcTypToTxt FTSTABLE = "STABLE"

instance Show FunctionVolatility where
  show = T.unpack . funcTypToTxt

newtype FunctionArgName = FunctionArgName {getFuncArgNameTxt :: Text}
  deriving (Show, Eq, NFData, ToJSON, FromJSON, ToTxt, IsString, Generic, Cacheable, Hashable)

newtype HasDefault = HasDefault {unHasDefault :: Bool}
  deriving (Show, Eq, ToJSON, Cacheable, NFData, Hashable)

data FunctionArg (b :: BackendType) = FunctionArg
  { faName :: !(Maybe FunctionArgName),
    faType :: !(FunctionArgType b),
    faHasDefault :: !HasDefault
  }
  deriving (Generic)

deriving instance Backend b => Show (FunctionArg b)

deriving instance Backend b => Eq (FunctionArg b)

instance Backend b => Cacheable (FunctionArg b)

instance Backend b => NFData (FunctionArg b)

instance Backend b => Hashable (FunctionArg b)

instance (Backend b) => ToJSON (FunctionArg b) where
  toJSON = genericToJSON hasuraJSON

data InputArgument a
  = IAUserProvided !a
  | IASessionVariables !FunctionArgName
  deriving (Show, Eq, Functor)

$( deriveToJSON
     defaultOptions
       { constructorTagModifier = snakeCase . drop 2,
         sumEncoding = TaggedObject "type" "argument"
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

$( deriveJSON
     defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 3}
     ''FunctionExposedAs
 )

newtype FunctionPermissionInfo = FunctionPermissionInfo
  { _fpmRole :: RoleName
  }
  deriving (Show, Eq, Generic)

instance Cacheable FunctionPermissionInfo

$(makeLenses ''FunctionPermissionInfo)
$(deriveJSON hasuraJSON ''FunctionPermissionInfo)

type FunctionPermissionsMap = HashMap RoleName FunctionPermissionInfo

-- | Tracked SQL function metadata. See 'buildFunctionInfo'.
data FunctionInfo (b :: BackendType) = FunctionInfo
  { _fiName :: !(FunctionName b),
    _fiSystemDefined :: !SystemDefined,
    _fiVolatility :: !FunctionVolatility,
    -- | In which part of the schema should this function be exposed?
    --
    -- See 'mkFunctionInfo' and '_fcExposedAs'.
    _fiExposedAs :: !FunctionExposedAs,
    _fiInputArgs :: !(Seq.Seq (FunctionInputArgument b)),
    -- | NOTE: when a table is created, a new composite type of the same name is
    -- automatically created; so strictly speaking this field means "the function
    -- returns the composite type corresponding to this table".
    _fiReturnType :: !(TableName b),
    -- | this field represents the description of the function as present on the database
    _fiDescription :: !(Maybe Text),
    _fiPermissions :: !FunctionPermissionsMap,
    -- | Roles to which the function is accessible
    _fiJsonAggSelect :: !JsonAggSelect,
    _fiComment :: !(Maybe Text)
  }
  deriving (Generic)

deriving instance Backend b => Show (FunctionInfo b)

deriving instance Backend b => Eq (FunctionInfo b)

instance (Backend b) => ToJSON (FunctionInfo b) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''FunctionInfo)

getInputArgs :: FunctionInfo b -> Seq.Seq (FunctionArg b)
getInputArgs =
  Seq.fromList . mapMaybe (^? _IAUserProvided) . toList . _fiInputArgs

type FunctionCache b = HashMap (FunctionName b) (FunctionInfo b) -- info of all functions

-- Metadata requests related types

-- | Tracked function configuration, and payload of the 'track_function' API call.
data FunctionConfig = FunctionConfig
  { _fcSessionArgument :: !(Maybe FunctionArgName),
    -- | In which top-level field should we expose this function?
    --
    -- The user might omit this, in which case we'll infer the location from the
    -- SQL functions volatility. See 'mkFunctionInfo' or the @track_function@ API
    -- docs for details of validation, etc.
    _fcExposedAs :: !(Maybe FunctionExposedAs)
  }
  deriving (Show, Eq, Generic)

instance NFData FunctionConfig

instance Cacheable FunctionConfig

$(deriveJSON hasuraJSON {omitNothingFields = True} ''FunctionConfig)

-- | The default function config; v1 of the API implies this.
emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing Nothing

-- Lists are used to model overloaded functions.
type DBFunctionsMetadata b = HashMap (FunctionName b) [RawFunctionInfo b]

data FunctionPermissionsCtx
  = FunctionPermissionsInferred
  | FunctionPermissionsManual
  deriving (Show, Eq)

instance FromJSON FunctionPermissionsCtx where
  parseJSON =
    withBool "FunctionPermissionsCtx" $
      pure . bool FunctionPermissionsManual FunctionPermissionsInferred

instance ToJSON FunctionPermissionsCtx where
  toJSON = \case
    FunctionPermissionsInferred -> Bool True
    FunctionPermissionsManual -> Bool False
