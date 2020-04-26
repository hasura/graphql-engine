-- | Types and classes related to configuration when the server is initialised
module Hasura.Server.Init.Config where

import qualified Data.Aeson                       as J
import qualified Data.Aeson.TH                    as J
import qualified Data.HashSet                     as Set
import qualified Data.String                      as DataString
import qualified Data.Text                        as T
import qualified Database.PG.Query                as Q

import           Data.Char                        (toLower)
import           Data.Time.Clock.Units            (milliseconds)
import           Network.Wai.Handler.Warp         (HostPreference)

import qualified Hasura.Cache                     as Cache
import qualified Hasura.GraphQL.Execute           as E
import qualified Hasura.GraphQL.Execute.LiveQuery as LQ
import qualified Hasura.Logging                   as L

import           Hasura.Prelude
import           Hasura.Server.Auth
import           Hasura.Server.Cors
import           Hasura.Session

data RawConnParams
  = RawConnParams
  { rcpStripes      :: !(Maybe Int)
  , rcpConns        :: !(Maybe Int)
  , rcpIdleTime     :: !(Maybe Int)
  , rcpAllowPrepare :: !(Maybe Bool)
  } deriving (Show, Eq)

type RawAuthHook = AuthHookG (Maybe T.Text) (Maybe AuthHookType)

data RawServeOptions impl
  = RawServeOptions
  { rsoPort                :: !(Maybe Int)
  , rsoHost                :: !(Maybe HostPreference)
  , rsoConnParams          :: !RawConnParams
  , rsoTxIso               :: !(Maybe Q.TxIsolation)
  , rsoAdminSecret         :: !(Maybe AdminSecret)
  , rsoAuthHook            :: !RawAuthHook
  , rsoJwtSecret           :: !(Maybe JWTConfig)
  , rsoUnAuthRole          :: !(Maybe RoleName)
  , rsoCorsConfig          :: !(Maybe CorsConfig)
  , rsoEnableConsole       :: !Bool
  , rsoConsoleAssetsDir    :: !(Maybe Text)
  , rsoEnableTelemetry     :: !(Maybe Bool)
  , rsoWsReadCookie        :: !Bool
  , rsoStringifyNum        :: !Bool
  , rsoEnabledAPIs         :: !(Maybe [API])
  , rsoMxRefetchInt        :: !(Maybe LQ.RefetchInterval)
  , rsoMxBatchSize         :: !(Maybe LQ.BatchSize)
  , rsoEnableAllowlist     :: !Bool
  , rsoEnabledLogTypes     :: !(Maybe [L.EngineLogType impl])
  , rsoLogLevel            :: !(Maybe L.LogLevel)
  , rsoPlanCacheSize       :: !(Maybe Cache.CacheSize)
  , rsoDevMode             :: !Bool
  , rsoAdminInternalErrors :: !(Maybe Bool)
  }

-- | @'ResponseInternalErrorsConfig' represents the encoding of the internal
-- errors in the response to the client.
-- See the github comment https://github.com/hasura/graphql-engine/issues/4031#issuecomment-609747705 for more details.
data ResponseInternalErrorsConfig
  = InternalErrorsAllRequests
  | InternalErrorsAdminOnly
  | InternalErrorsDisabled
  deriving (Show, Eq)

shouldIncludeInternal :: RoleName -> ResponseInternalErrorsConfig -> Bool
shouldIncludeInternal role = \case
  InternalErrorsAllRequests -> True
  InternalErrorsAdminOnly   -> isAdmin role
  InternalErrorsDisabled    -> False

data ServeOptions impl
  = ServeOptions
  { soPort                         :: !Int
  , soHost                         :: !HostPreference
  , soConnParams                   :: !Q.ConnParams
  , soTxIso                        :: !Q.TxIsolation
  , soAdminSecret                  :: !(Maybe AdminSecret)
  , soAuthHook                     :: !(Maybe AuthHook)
  , soJwtSecret                    :: !(Maybe JWTConfig)
  , soUnAuthRole                   :: !(Maybe RoleName)
  , soCorsConfig                   :: !CorsConfig
  , soEnableConsole                :: !Bool
  , soConsoleAssetsDir             :: !(Maybe Text)
  , soEnableTelemetry              :: !Bool
  , soStringifyNum                 :: !Bool
  , soEnabledAPIs                  :: !(Set.HashSet API)
  , soLiveQueryOpts                :: !LQ.LiveQueriesOptions
  , soEnableAllowlist              :: !Bool
  , soEnabledLogTypes              :: !(Set.HashSet (L.EngineLogType impl))
  , soLogLevel                     :: !L.LogLevel
  , soPlanCacheOptions             :: !E.PlanCacheOptions
  , soResponseInternalErrorsConfig :: !ResponseInternalErrorsConfig
  }

data DowngradeOptions
  = DowngradeOptions
  { dgoTargetVersion :: !T.Text
  , dgoDryRun        :: !Bool
  } deriving (Show, Eq)

data RawConnInfo =
  RawConnInfo
  { connHost     :: !(Maybe String)
  , connPort     :: !(Maybe Int)
  , connUser     :: !(Maybe String)
  , connPassword :: !String
  , connUrl      :: !(Maybe String)
  , connDatabase :: !(Maybe String)
  , connOptions  :: !(Maybe String)
  , connRetries  :: !(Maybe Int)
  } deriving (Eq, Read, Show)

data HGECommandG a
  = HCServe !a
  | HCExport
  | HCClean
  | HCExecute
  | HCVersion
  | HCDowngrade !DowngradeOptions
  deriving (Show, Eq)

data API
  = METADATA
  | GRAPHQL
  | PGDUMP
  | DEVELOPER
  | CONFIG
  deriving (Show, Eq, Read, Generic)
$(J.deriveJSON (J.defaultOptions { J.constructorTagModifier = map toLower })
  ''API)

instance Hashable API

type HGECommand impl = HGECommandG (ServeOptions impl)
type RawHGECommand impl = HGECommandG (RawServeOptions impl)

data HGEOptionsG a
  = HGEOptionsG
  { hoConnInfo :: !RawConnInfo
  , hoCommand  :: !(HGECommandG a)
  } deriving (Show, Eq)

type RawHGEOptions impl = HGEOptionsG (RawServeOptions impl)
type HGEOptions impl = HGEOptionsG (ServeOptions impl)

type Env = [(String, String)]

readHookType :: String -> Either String AuthHookType
readHookType tyS =
  case tyS of
    "GET"  -> Right AHTGet
    "POST" -> Right AHTPost
    _      -> Left "Only expecting GET / POST"

parseStrAsBool :: String -> Either String Bool
parseStrAsBool t
  | map toLower t `elem` truthVals = Right True
  | map toLower t `elem` falseVals = Right False
  | otherwise = Left errMsg
  where
    truthVals = ["true", "t", "yes", "y"]
    falseVals = ["false", "f", "no", "n"]

    errMsg = " Not a valid boolean text. " ++ "True values are "
             ++ show truthVals ++ " and  False values are " ++ show falseVals
             ++ ". All values are case insensitive"

readIsoLevel :: String -> Either String Q.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-committed"  -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable"    -> return Q.Serializable
    _                 -> Left "Only expecting read-committed / repeatable-read / serializable"

readAPIs :: String -> Either String [API]
readAPIs = mapM readAPI . T.splitOn "," . T.pack
  where readAPI si = case T.toUpper $ T.strip si of
          "METADATA"  -> Right METADATA
          "GRAPHQL"   -> Right GRAPHQL
          "PGDUMP"    -> Right PGDUMP
          "DEVELOPER" -> Right DEVELOPER
          "CONFIG"    -> Right CONFIG
          _            -> Left "Only expecting list of comma separated API types metadata,graphql,pgdump,developer,config"

readLogLevel :: String -> Either String L.LogLevel
readLogLevel s = case T.toLower $ T.strip $ T.pack s of
  "debug" -> Right L.LevelDebug
  "info"  -> Right L.LevelInfo
  "warn"  -> Right L.LevelWarn
  "error" -> Right L.LevelError
  _       -> Left "Valid log levels: debug, info, warn or error"

readJson :: (J.FromJSON a) => String -> Either String a
readJson = J.eitherDecodeStrict . txtToBs . T.pack

class FromEnv a where
  fromEnv :: String -> Either String a

instance FromEnv String where
  fromEnv = Right

instance FromEnv HostPreference where
  fromEnv = Right . DataString.fromString

instance FromEnv Text where
  fromEnv = Right . T.pack

instance FromEnv AuthHookType where
  fromEnv = readHookType

instance FromEnv Int where
  fromEnv = maybe (Left "Expecting Int value") Right . readMaybe

instance FromEnv AdminSecret where
  fromEnv = Right . AdminSecret . T.pack

instance FromEnv RoleName where
  fromEnv string = case mkRoleName (T.pack string) of
    Nothing       -> Left "empty string not allowed"
    Just roleName -> Right roleName

instance FromEnv Bool where
  fromEnv = parseStrAsBool

instance FromEnv Q.TxIsolation where
  fromEnv = readIsoLevel

instance FromEnv CorsConfig where
  fromEnv = readCorsDomains

instance FromEnv [API] where
  fromEnv = readAPIs

instance FromEnv LQ.BatchSize where
  fromEnv = fmap LQ.BatchSize . readEither

instance FromEnv LQ.RefetchInterval where
  fromEnv = fmap (LQ.RefetchInterval . milliseconds . fromInteger) . readEither

instance FromEnv JWTConfig where
  fromEnv = readJson

instance L.EnabledLogTypes impl => FromEnv [L.EngineLogType impl] where
  fromEnv = L.parseEnabledLogTypes

instance FromEnv L.LogLevel where
  fromEnv = readLogLevel

instance FromEnv Cache.CacheSize where
  fromEnv = Cache.mkCacheSize

type WithEnv a = ReaderT Env (ExceptT String Identity) a

runWithEnv :: Env -> WithEnv a -> Either String a
runWithEnv env m = runIdentity $ runExceptT $ runReaderT m env
