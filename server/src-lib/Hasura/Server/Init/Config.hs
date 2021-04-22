-- | Types and classes related to configuration when the server is initialised
module Hasura.Server.Init.Config where

import qualified Data.Aeson                               as J
import qualified Data.Aeson.Casing                        as J
import qualified Data.Aeson.TH                            as J
import qualified Data.HashSet                             as Set
import qualified Data.String                              as DataString
import qualified Data.Text                                as T
import qualified Database.PG.Query                        as Q
import qualified Network.WebSockets                       as WS


import           Data.Aeson
import           Data.Char                                (toLower)
import           Data.Time
import           Data.URL.Template
import           Network.Wai.Handler.Warp                 (HostPreference)

import qualified Hasura.Cache.Bounded                     as Cache
import qualified Hasura.GraphQL.Execute.LiveQuery.Options as LQ
import qualified Hasura.GraphQL.Execute.Plan              as E
import qualified Hasura.Logging                           as L
import qualified System.Metrics                           as EKG
import qualified System.Metrics.Distribution              as EKG.Distribution
import qualified System.Metrics.Gauge                     as EKG.Gauge

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth
import           Hasura.Server.Cors
import           Hasura.Server.Types
import           Hasura.Session

data RawConnParams
  = RawConnParams
  { rcpStripes      :: !(Maybe Int)
  , rcpConns        :: !(Maybe Int)
  , rcpIdleTime     :: !(Maybe Int)
  , rcpConnLifetime :: !(Maybe NominalDiffTime)
  -- ^ Time from connection creation after which to destroy a connection and
  -- choose a different/new one.
  , rcpAllowPrepare :: !(Maybe Bool)
  , rcpPoolTimeout  :: !(Maybe NominalDiffTime)
  -- ^ See @HASURA_GRAPHQL_PG_POOL_TIMEOUT@
  } deriving (Show, Eq)

type RawAuthHook = AuthHookG (Maybe Text) (Maybe AuthHookType)

-- | Sleep time interval for recurring activities such as (@'asyncActionsProcessor')
--   Presently @'msToOptionalInterval' interprets `0` as Skip.
data OptionalInterval
  = Skip -- ^ No polling
  | Interval !Milliseconds -- ^ Interval time
  deriving (Show, Eq)

msToOptionalInterval :: Milliseconds -> OptionalInterval
msToOptionalInterval = \case
  0 -> Skip
  s -> Interval s

instance FromJSON OptionalInterval where
  parseJSON v = msToOptionalInterval <$> parseJSON v

instance ToJSON OptionalInterval where
  toJSON = \case
    Skip       -> toJSON @Milliseconds 0
    Interval s -> toJSON s

data RawServeOptions impl
  = RawServeOptions
  { rsoPort                          :: !(Maybe Int)
  , rsoHost                          :: !(Maybe HostPreference)
  , rsoConnParams                    :: !RawConnParams
  , rsoTxIso                         :: !(Maybe Q.TxIsolation)
  , rsoAdminSecret                   :: !(Maybe AdminSecretHash)
  , rsoAuthHook                      :: !RawAuthHook
  , rsoJwtSecret                     :: !(Maybe JWTConfig)
  , rsoUnAuthRole                    :: !(Maybe RoleName)
  , rsoCorsConfig                    :: !(Maybe CorsConfig)
  , rsoEnableConsole                 :: !Bool
  , rsoConsoleAssetsDir              :: !(Maybe Text)
  , rsoEnableTelemetry               :: !(Maybe Bool)
  , rsoWsReadCookie                  :: !Bool
  , rsoStringifyNum                  :: !Bool
  , rsoDangerousBooleanCollapse      :: !(Maybe Bool)
  , rsoEnabledAPIs                   :: !(Maybe [API])
  , rsoMxRefetchInt                  :: !(Maybe LQ.RefetchInterval)
  , rsoMxBatchSize                   :: !(Maybe LQ.BatchSize)
  , rsoEnableAllowlist               :: !Bool
  , rsoEnabledLogTypes               :: !(Maybe [L.EngineLogType impl])
  , rsoLogLevel                      :: !(Maybe L.LogLevel)
  , rsoPlanCacheSize                 :: !(Maybe Cache.CacheSize)
  , rsoDevMode                       :: !Bool
  , rsoAdminInternalErrors           :: !(Maybe Bool)
  , rsoEventsHttpPoolSize            :: !(Maybe Int)
  , rsoEventsFetchInterval           :: !(Maybe Milliseconds)
  , rsoAsyncActionsFetchInterval     :: !(Maybe Milliseconds)
  , rsoLogHeadersFromEnv             :: !Bool
  , rsoEnableRemoteSchemaPermissions :: !Bool
  , rsoWebSocketCompression          :: !Bool
  , rsoWebSocketKeepAlive            :: !(Maybe Int)
  , rsoInferFunctionPermissions      :: !(Maybe Bool)
  , rsoEnableMaintenanceMode         :: !Bool
  , rsoSchemaPollInterval            :: !(Maybe Milliseconds)
  , rsoExperimentalFeatures          :: !(Maybe [ExperimentalFeature])
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
  InternalErrorsAdminOnly   -> role == adminRoleName
  InternalErrorsDisabled    -> False

newtype KeepAliveDelay
  = KeepAliveDelay
      { unKeepAliveDelay :: Seconds
      } deriving (Eq, Show)

data ServeOptions impl
  = ServeOptions
  { soPort                          :: !Int
  , soHost                          :: !HostPreference
  , soConnParams                    :: !Q.ConnParams
  , soTxIso                         :: !Q.TxIsolation
  , soAdminSecret                   :: !(Maybe AdminSecretHash)
  , soAuthHook                      :: !(Maybe AuthHook)
  , soJwtSecret                     :: !(Maybe JWTConfig)
  , soUnAuthRole                    :: !(Maybe RoleName)
  , soCorsConfig                    :: !CorsConfig
  , soEnableConsole                 :: !Bool
  , soConsoleAssetsDir              :: !(Maybe Text)
  , soEnableTelemetry               :: !Bool
  , soStringifyNum                  :: !Bool
  , soDangerousBooleanCollapse      :: !Bool
  , soEnabledAPIs                   :: !(Set.HashSet API)
  , soLiveQueryOpts                 :: !LQ.LiveQueriesOptions
  , soEnableAllowlist               :: !Bool
  , soEnabledLogTypes               :: !(Set.HashSet (L.EngineLogType impl))
  , soLogLevel                      :: !L.LogLevel
  , soPlanCacheOptions              :: !E.PlanCacheOptions
  , soResponseInternalErrorsConfig  :: !ResponseInternalErrorsConfig
  , soEventsHttpPoolSize            :: !(Maybe Int)
  , soEventsFetchInterval           :: !(Maybe Milliseconds)
  , soAsyncActionsFetchInterval     :: !OptionalInterval
  , soLogHeadersFromEnv             :: !Bool
  , soEnableRemoteSchemaPermissions :: !RemoteSchemaPermsCtx
  , soConnectionOptions             :: !WS.ConnectionOptions
  , soWebsocketKeepAlive            :: !KeepAliveDelay
  , soInferFunctionPermissions      :: !FunctionPermissionsCtx
  , soEnableMaintenanceMode         :: !MaintenanceMode
  , soSchemaPollInterval            :: !OptionalInterval
  , soExperimentalFeatures          :: !(Set.HashSet ExperimentalFeature)
  }

data DowngradeOptions
  = DowngradeOptions
  { dgoTargetVersion :: !Text
  , dgoDryRun        :: !Bool
  } deriving (Show, Eq)

data PostgresConnInfo a
  = PostgresConnInfo
  { _pciDatabaseConn :: !a
  , _pciRetries      :: !(Maybe Int)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data PostgresRawConnDetails =
  PostgresRawConnDetails
  { connHost     :: !String
  , connPort     :: !Int
  , connUser     :: !String
  , connPassword :: !String
  , connDatabase :: !String
  , connOptions  :: !(Maybe String)
  } deriving (Eq, Read, Show)

data PostgresRawConnInfo
  = PGConnDatabaseUrl !URLTemplate
  | PGConnDetails !PostgresRawConnDetails
  deriving (Show, Eq)

rawConnDetailsToUrl :: PostgresRawConnDetails -> URLTemplate
rawConnDetailsToUrl =
  mkPlainURLTemplate . rawConnDetailsToUrlText

rawConnDetailsToUrlText :: PostgresRawConnDetails -> Text
rawConnDetailsToUrlText PostgresRawConnDetails{..} =
  T.pack $
    "postgresql://" <> connUser <>
    ":" <> connPassword <>
    "@" <> connHost <>
    ":" <> show connPort <>
    "/" <> connDatabase <>
    maybe "" ("?options=" <>) connOptions

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

$(J.deriveJSON (J.aesonPrefix J.camelCase){J.omitNothingFields=True} ''PostgresRawConnDetails)

type HGECommand impl = HGECommandG (ServeOptions impl)
type RawHGECommand impl = HGECommandG (RawServeOptions impl)

data HGEOptionsG a b
  = HGEOptionsG
  { hoDatabaseUrl   :: !(PostgresConnInfo a)
  , hoMetadataDbUrl :: !(Maybe String)
  , hoCommand       :: !(HGECommandG b)
  } deriving (Show, Eq)

type RawHGEOptions impl = HGEOptionsG (Maybe PostgresRawConnInfo) (RawServeOptions impl)
type HGEOptions impl = HGEOptionsG (Maybe UrlConf) (ServeOptions impl)

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

readExperimentalFeatures :: String -> Either String [ExperimentalFeature]
readExperimentalFeatures = mapM readAPI . T.splitOn "," . T.pack
  where readAPI si = case T.toLower $ T.strip si of
          "inherited_roles" -> Right EFInheritedRoles
          _                 -> Left "Only expecting list of comma separated experimental features"

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

-- Deserialize from seconds, in the usual way
instance FromEnv NominalDiffTime where
  fromEnv s = maybe (Left "could not parse as a Double") (Right . realToFrac) $
                (readMaybe s :: Maybe Double)

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

instance FromEnv AdminSecretHash where
  fromEnv = Right . hashAdminSecret . T.pack

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

instance FromEnv [ExperimentalFeature] where
  fromEnv = readExperimentalFeatures

instance FromEnv LQ.BatchSize where
  fromEnv s = do
    val <- readEither s
    maybe (Left "batch size should be a non negative integer") Right $ LQ.mkBatchSize val

instance FromEnv LQ.RefetchInterval where
  fromEnv x = do
    val <- fmap (milliseconds . fromInteger) . readEither $ x
    maybe (Left "refetch interval should be a non negative integer") Right $ LQ.mkRefetchInterval val

instance FromEnv Milliseconds where
  fromEnv = fmap fromInteger . readEither

instance FromEnv JWTConfig where
  fromEnv = readJson

instance L.EnabledLogTypes impl => FromEnv [L.EngineLogType impl] where
  fromEnv = L.parseEnabledLogTypes

instance FromEnv L.LogLevel where
  fromEnv = readLogLevel

instance FromEnv Cache.CacheSize where
  fromEnv = Cache.parseCacheSize

instance FromEnv URLTemplate where
  fromEnv = parseURLTemplate . T.pack

type WithEnv a = ReaderT Env (ExceptT String Identity) a

runWithEnv :: Env -> WithEnv a -> Either String a
runWithEnv env m = runIdentity $ runExceptT $ runReaderT m env

-- | Collection of various server metrics
data ServerMetrics
  = ServerMetrics
  { smWarpThreads         :: !EKG.Gauge.Gauge
  -- ^ Current Number of warp threads
  , smNumEventsFetched    :: !EKG.Distribution.Distribution
  -- ^ Total Number of events fetched from last 'Event Trigger Fetch'
  , smNumEventHTTPWorkers :: !EKG.Gauge.Gauge
  -- ^ Current number of Event trigger's HTTP workers in process
  , smEventLockTime       :: !EKG.Distribution.Distribution
  -- ^ Time between the 'Event Trigger Fetch' from DB and the processing of the event
  }

createServerMetrics :: EKG.Store -> IO ServerMetrics
createServerMetrics store = do
  smWarpThreads <- EKG.createGauge "warp_threads" store
  smNumEventsFetched <- EKG.createDistribution "num_events_fetched" store
  smNumEventHTTPWorkers <- EKG.createGauge "num_event_trigger_http_workers" store
  smEventLockTime <- EKG.createDistribution "event_lock_time" store
  pure ServerMetrics { .. }
