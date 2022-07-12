{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types and classes related to configuration when the server is initialised
module Hasura.Server.Init.Config
  ( API (..),
    DowngradeOptions (..),
    FromEnv (..),
    HGECommand (..),
    HGEOptions (..),
    HGEOptionsRaw (..),
    KeepAliveDelay (..),
    OptionalInterval (..),
    PostgresConnInfo (..),
    PostgresRawConnDetails (..),
    PostgresRawConnInfo (..),
    RawAuthHook,
    RawConnParams (..),
    RawServeOptions (..),
    ResponseInternalErrorsConfig (..),
    ServeOptions (..),
    WSConnectionInitTimeout (..),
    WithEnv,
    defaultKeepAliveDelay,
    defaultWSConnectionInitTimeout,
    msToOptionalInterval,
    parseStrAsBool,
    rawConnDetailsToUrl,
    rawConnDetailsToUrlText,
    readAPIs,
    readDefaultNamingCase,
    readExperimentalFeatures,
    readHookType,
    readJson,
    readLogLevel,
    readNonNegativeInt,
    runWithEnv,
    shouldIncludeInternal,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.TH qualified as J
import Data.Char (toLower)
import Data.HashSet qualified as Set
import Data.String qualified as DataString
import Data.Text qualified as T
import Data.Time
import Data.URL.Template
import Database.PG.Query qualified as Q
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.RemoteSchema
import Hasura.Server.Auth
import Hasura.Server.Cors
import Hasura.Server.Logging
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Session
import Network.Wai.Handler.Warp (HostPreference)
import Network.WebSockets qualified as WS

--------------------------------------------------------------------------------

data RawConnParams = RawConnParams
  { rcpStripes :: !(Maybe Int),
    rcpConns :: !(Maybe Int),
    rcpIdleTime :: !(Maybe Int),
    -- | Time from connection creation after which to destroy a connection and
    -- choose a different/new one.
    rcpConnLifetime :: !(Maybe NominalDiffTime),
    rcpAllowPrepare :: !(Maybe Bool),
    -- | See @HASURA_GRAPHQL_PG_POOL_TIMEOUT@
    rcpPoolTimeout :: !(Maybe NominalDiffTime)
  }
  deriving (Show, Eq)

type RawAuthHook = AuthHookG (Maybe Text) (Maybe AuthHookType)

-- | Sleep time interval for recurring activities such as (@'asyncActionsProcessor')
--   Presently @'msToOptionalInterval' interprets `0` as Skip.
data OptionalInterval
  = -- | No polling
    Skip
  | -- | Interval time
    Interval !Milliseconds
  deriving (Show, Eq)

msToOptionalInterval :: Milliseconds -> OptionalInterval
msToOptionalInterval = \case
  0 -> Skip
  s -> Interval s

instance FromJSON OptionalInterval where
  parseJSON v = msToOptionalInterval <$> parseJSON v

instance ToJSON OptionalInterval where
  toJSON = \case
    Skip -> toJSON @Milliseconds 0
    Interval s -> toJSON s

data API
  = METADATA
  | GRAPHQL
  | PGDUMP
  | DEVELOPER
  | CONFIG
  deriving (Show, Eq, Read, Generic)

$( J.deriveJSON
     (J.defaultOptions {J.constructorTagModifier = map toLower})
     ''API
 )

instance Hashable API

-- | The Serve Command options accumulated from the arg parser and env.
data RawServeOptions impl = RawServeOptions
  { rsoPort :: Maybe Int,
    rsoHost :: Maybe HostPreference,
    rsoConnParams :: RawConnParams,
    rsoTxIso :: Maybe Q.TxIsolation,
    rsoAdminSecret :: Maybe AdminSecretHash,
    rsoAuthHook :: RawAuthHook,
    rsoJwtSecret :: Maybe JWTConfig,
    rsoUnAuthRole :: Maybe RoleName,
    rsoCorsConfig :: Maybe CorsConfig,
    rsoEnableConsole :: Bool,
    rsoConsoleAssetsDir :: Maybe Text,
    rsoEnableTelemetry :: Maybe Bool,
    rsoWsReadCookie :: Bool,
    rsoStringifyNum :: Bool,
    rsoDangerousBooleanCollapse :: Maybe Bool,
    rsoEnabledAPIs :: Maybe [API],
    rsoMxRefetchInt :: Maybe ES.RefetchInterval,
    rsoMxBatchSize :: Maybe ES.BatchSize,
    -- we have different config options for livequery and streaming subscriptions
    rsoStreamingMxRefetchInt :: Maybe ES.RefetchInterval,
    rsoStreamingMxBatchSize :: Maybe ES.BatchSize,
    rsoEnableAllowlist :: Bool,
    rsoEnabledLogTypes :: Maybe [L.EngineLogType impl],
    rsoLogLevel :: Maybe L.LogLevel,
    rsoDevMode :: Bool,
    rsoAdminInternalErrors :: Maybe Bool,
    rsoEventsHttpPoolSize :: Maybe Int,
    rsoEventsFetchInterval :: Maybe Milliseconds,
    rsoAsyncActionsFetchInterval :: Maybe Milliseconds,
    rsoEnableRemoteSchemaPermissions :: Bool,
    rsoWebSocketCompression :: Bool,
    rsoWebSocketKeepAlive :: Maybe Int,
    rsoInferFunctionPermissions :: Maybe Bool,
    rsoEnableMaintenanceMode :: Bool,
    rsoSchemaPollInterval :: Maybe Milliseconds,
    rsoExperimentalFeatures :: Maybe [ExperimentalFeature],
    rsoEventsFetchBatchSize :: Maybe NonNegativeInt,
    rsoGracefulShutdownTimeout :: Maybe Seconds,
    rsoWebSocketConnectionInitTimeout :: Maybe Int,
    rsoEnableMetadataQueryLoggingEnv :: Bool,
    -- see Note [Experimental features]

    -- | stores global default naming convention
    rsoDefaultNamingConvention :: Maybe NamingCase
  }

--------------------------------------------------------------------------------

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
  InternalErrorsAdminOnly -> role == adminRoleName
  InternalErrorsDisabled -> False

newtype KeepAliveDelay = KeepAliveDelay {unKeepAliveDelay :: Seconds}
  deriving (Eq, Show)

$(J.deriveJSON hasuraJSON ''KeepAliveDelay)

defaultKeepAliveDelay :: KeepAliveDelay
defaultKeepAliveDelay = KeepAliveDelay $ fromIntegral (5 :: Int)

newtype WSConnectionInitTimeout = WSConnectionInitTimeout {unWSConnectionInitTimeout :: Seconds}
  deriving (Eq, Show)

$(J.deriveJSON hasuraJSON ''WSConnectionInitTimeout)

defaultWSConnectionInitTimeout :: WSConnectionInitTimeout
defaultWSConnectionInitTimeout = WSConnectionInitTimeout $ fromIntegral (3 :: Int)

--------------------------------------------------------------------------------

-- | The final Serve Command options accummulated from the Arg Parser
-- and the Environment, fully processed and ready to apply when
-- running the server.
data ServeOptions impl = ServeOptions
  { soPort :: Int,
    soHost :: HostPreference,
    soConnParams :: Q.ConnParams,
    soTxIso :: Q.TxIsolation,
    soAdminSecret :: Set.HashSet AdminSecretHash,
    soAuthHook :: Maybe AuthHook,
    soJwtSecret :: [JWTConfig],
    soUnAuthRole :: Maybe RoleName,
    soCorsConfig :: CorsConfig,
    soEnableConsole :: Bool,
    soConsoleAssetsDir :: Maybe Text,
    soEnableTelemetry :: Bool,
    soStringifyNum :: StringifyNumbers,
    soDangerousBooleanCollapse :: Bool,
    soEnabledAPIs :: Set.HashSet API,
    soLiveQueryOpts :: ES.LiveQueriesOptions,
    soStreamingQueryOpts :: ES.StreamQueriesOptions,
    soEnableAllowlist :: Bool,
    soEnabledLogTypes :: Set.HashSet (L.EngineLogType impl),
    soLogLevel :: L.LogLevel,
    soResponseInternalErrorsConfig :: ResponseInternalErrorsConfig,
    soEventsHttpPoolSize :: Maybe Int,
    soEventsFetchInterval :: Maybe Milliseconds,
    soAsyncActionsFetchInterval :: OptionalInterval,
    soEnableRemoteSchemaPermissions :: RemoteSchemaPermsCtx,
    soConnectionOptions :: WS.ConnectionOptions,
    soWebsocketKeepAlive :: KeepAliveDelay,
    soInferFunctionPermissions :: FunctionPermissionsCtx,
    soEnableMaintenanceMode :: MaintenanceMode (),
    soSchemaPollInterval :: OptionalInterval,
    soExperimentalFeatures :: Set.HashSet ExperimentalFeature,
    soEventsFetchBatchSize :: NonNegativeInt,
    soDevMode :: Bool,
    soGracefulShutdownTimeout :: Seconds,
    soWebsocketConnectionInitTimeout :: WSConnectionInitTimeout,
    soEventingMode :: EventingMode,
    soReadOnlyMode :: ReadOnlyMode,
    soEnableMetadataQueryLogging :: MetadataQueryLoggingMode,
    soDefaultNamingConvention :: Maybe NamingCase
  }

-- | The Downgrade Command options. These are only sourced from the
-- Arg Parser and are used directly in 'Hasura.Server.Migrate'.
data DowngradeOptions = DowngradeOptions
  { dgoTargetVersion :: !Text,
    dgoDryRun :: !Bool
  }
  deriving (Show, Eq)

-- | Postgres connection info tupled with a retry count.
--
-- The @a@ here is one of the following:
-- 1. 'Maybe PostgresRawConnInfo'
-- 2. 'Maybe UrlConf'
-- 3. 'Maybe Text'
-- 4. 'Maybe DatabaseUrl' where 'DatabaseUrl' is an alias for 'Text'
--
-- If it contains a 'Maybe PostgresRawConnInfo' then you have not yet
-- processed your arg parser results.
data PostgresConnInfo a = PostgresConnInfo
  { _pciDatabaseConn :: !a,
    _pciRetries :: !(Maybe Int)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Structured Postgres connection information as provided by the arg
-- parser or env vars.
data PostgresRawConnDetails = PostgresRawConnDetails
  { connHost :: !String,
    connPort :: !Int,
    connUser :: !String,
    connPassword :: !String,
    connDatabase :: !String,
    connOptions :: !(Maybe String)
  }
  deriving (Eq, Read, Show)

$(J.deriveJSON (J.aesonPrefix J.camelCase) {J.omitNothingFields = True} ''PostgresRawConnDetails)

-- | Postgres Connection info can come in the form of a templated URI
-- string or structured data.
data PostgresRawConnInfo
  = PGConnDatabaseUrl !URLTemplate
  | PGConnDetails !PostgresRawConnDetails
  deriving (Show, Eq)

rawConnDetailsToUrl :: PostgresRawConnDetails -> URLTemplate
rawConnDetailsToUrl =
  mkPlainURLTemplate . rawConnDetailsToUrlText

rawConnDetailsToUrlText :: PostgresRawConnDetails -> Text
rawConnDetailsToUrlText PostgresRawConnDetails {..} =
  T.pack $
    "postgresql://" <> connUser
      <> ":"
      <> connPassword
      <> "@"
      <> connHost
      <> ":"
      <> show connPort
      <> "/"
      <> connDatabase
      <> maybe "" ("?options=" <>) connOptions

-- | The HGE Arg parser Command choices.
--
-- This is polymorphic so that we can pack either 'RawServeOptions' or
-- 'RawProServeOptions' in it.
data HGECommand a
  = HCServe !a
  | HCExport
  | HCClean
  | HCVersion
  | HCDowngrade !DowngradeOptions
  deriving (Show, Eq)

-- | HGE Options from the arg parser and the env.
data HGEOptionsRaw impl = HGEOptionsRaw
  { horDatabaseUrl :: !(PostgresConnInfo (Maybe PostgresRawConnInfo)),
    horMetadataDbUrl :: !(Maybe String),
    horCommand :: !(HGECommand impl)
  }

-- | The final processed HGE options.
data HGEOptions impl = HGEOptions
  { hoDatabaseUrl :: !(PostgresConnInfo (Maybe UrlConf)),
    hoMetadataDbUrl :: !(Maybe String),
    hoCommand :: !(HGECommand impl)
  }

readHookType :: String -> Either String AuthHookType
readHookType tyS =
  case tyS of
    "GET" -> Right AHTGet
    "POST" -> Right AHTPost
    _ -> Left "Only expecting GET / POST"

parseStrAsBool :: String -> Either String Bool
parseStrAsBool t
  | map toLower t `elem` truthVals = Right True
  | map toLower t `elem` falseVals = Right False
  | otherwise = Left errMsg
  where
    truthVals = ["true", "t", "yes", "y"]
    falseVals = ["false", "f", "no", "n"]

    errMsg =
      " Not a valid boolean text. True values are "
        ++ show truthVals
        ++ " and  False values are "
        ++ show falseVals
        ++ ". All values are case insensitive"

readNonNegativeInt :: String -> Either String NonNegativeInt
readNonNegativeInt s =
  onNothing (mkNonNegativeInt =<< readMaybe s) $ Left "Only expecting a non negative integer"

readAPIs :: String -> Either String [API]
readAPIs = mapM readAPI . T.splitOn "," . T.pack
  where
    readAPI si = case T.toUpper $ T.strip si of
      "METADATA" -> Right METADATA
      "GRAPHQL" -> Right GRAPHQL
      "PGDUMP" -> Right PGDUMP
      "DEVELOPER" -> Right DEVELOPER
      "CONFIG" -> Right CONFIG
      _ -> Left "Only expecting list of comma separated API types metadata,graphql,pgdump,developer,config"

readDefaultNamingCase :: String -> Either String NamingCase
readDefaultNamingCase = parseNamingConventionFromText . T.pack

{- Note: [Experimental features]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The graphql-engine accepts a list of experimental features that can be
enabled at the startup. Experimental features are a way to introduce
new, but not stable features to our users in a manner in which they have
the choice to enable or disable a certain feature(s).

The objective of an experimental feature should be that when the feature is disabled,
the graphql-engine should work the same way as it worked before adding the said feature.

The experimental feature's flag is `--experimental-features` and the corresponding
environment variable is `HASURA_GRAPHQL_EXPERIMENTAL_FEATURES` which expects a comma-seperated
value.

When an experimental feature is stable enough i.e. it's stable through multiple non-beta releases
then we make the feature not experimental i.e. it will always be enabled. Note that when we do this
we still have to support parsing of the experimental feature because users of the previous version
will have it enabled and when they upgrade an error should not be thrown at the startup. For example:

The inherited roles was an experimental feature when introduced and it was enabled by
setting `--experimental-features` to `inherited_roles` and then it was decided to make the inherited roles
a stable feature, so it was removed as an experimental feature but the code was modified such that
`--experimental-features inherited_roles` to not throw an error.

-}

readExperimentalFeatures :: String -> Either String [ExperimentalFeature]
readExperimentalFeatures = mapM readAPI . T.splitOn "," . T.pack
  where
    readAPI si = case T.toLower $ T.strip si of
      "inherited_roles" -> Right EFInheritedRoles
      "streaming_subscriptions" -> Right EFStreamingSubscriptions
      "optimize_permission_filters" -> Right EFOptimizePermissionFilters
      "naming_convention" -> Right EFNamingConventions
      _ ->
        Left $
          "Only expecting list of comma separated experimental features, options are:"
            ++ "inherited_roles, streaming_subscriptions, optimize_permission_filters, naming_convention"

readLogLevel :: String -> Either String L.LogLevel
readLogLevel s = case T.toLower $ T.strip $ T.pack s of
  "debug" -> Right L.LevelDebug
  "info" -> Right L.LevelInfo
  "warn" -> Right L.LevelWarn
  "error" -> Right L.LevelError
  _ -> Left "Valid log levels: debug, info, warn or error"

class FromEnv a where
  fromEnv :: String -> Either String a

-- Deserialize from seconds, in the usual way
instance FromEnv NominalDiffTime where
  fromEnv s =
    maybe (Left "could not parse as a Double") (Right . realToFrac) $
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
    Nothing -> Left "empty string not allowed"
    Just roleName -> Right roleName

instance FromEnv Bool where
  fromEnv = parseStrAsBool

instance FromEnv Q.TxIsolation where
  fromEnv = readIsoLevel

instance FromEnv CorsConfig where
  fromEnv = readCorsDomains

instance FromEnv [API] where
  fromEnv = readAPIs

instance FromEnv NamingCase where
  fromEnv = readDefaultNamingCase

instance FromEnv [ExperimentalFeature] where
  fromEnv = readExperimentalFeatures

instance FromEnv ES.BatchSize where
  fromEnv s = do
    val <- readEither s
    maybe (Left "batch size should be a non negative integer") Right $ ES.mkBatchSize val

instance FromEnv ES.RefetchInterval where
  fromEnv x = do
    val <- fmap (milliseconds . fromInteger) . readEither $ x
    maybe (Left "refetch interval should be a non negative integer") Right $ ES.mkRefetchInterval val

instance FromEnv Milliseconds where
  fromEnv = fmap fromInteger . readEither

instance FromEnv Seconds where
  fromEnv = fmap fromInteger . readEither

instance FromEnv JWTConfig where
  fromEnv = readJson

instance FromEnv [JWTConfig] where
  fromEnv = readJson

instance L.EnabledLogTypes impl => FromEnv [L.EngineLogType impl] where
  fromEnv = L.parseEnabledLogTypes

instance FromEnv L.LogLevel where
  fromEnv = readLogLevel

instance FromEnv URLTemplate where
  fromEnv = parseURLTemplate . T.pack

instance FromEnv NonNegativeInt where
  fromEnv = readNonNegativeInt

type WithEnv a = ReaderT [(String, String)] (ExceptT String Identity) a

runWithEnv :: [(String, String)] -> WithEnv a -> Either String a
runWithEnv env m = runIdentity $ runExceptT $ runReaderT m env
