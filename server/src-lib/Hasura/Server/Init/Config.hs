{-# LANGUAGE ViewPatterns #-}

-- | Types and classes related to configuration when the server is initialised
module Hasura.Server.Init.Config
  ( -- * Option
    Option (..),
    optionPP,

    -- * HGEOptionsRaw
    HGEOptionsRaw (..),
    horDatabaseUrl,
    horMetadataDbUrl,
    horCommand,

    -- * HGEOptions
    HGEOptions (..),
    hoCommand,

    -- * PostgresConnInfo
    PostgresConnInfo (..),
    pciDatabaseConn,
    pciRetries,

    -- * PostgresRawConnInfo
    PostgresConnInfoRaw (..),
    _PGConnDatabaseUrl,
    _PGConnDetails,
    mkUrlConnInfo,

    -- * PostgresRawConnDetails
    PostgresConnDetailsRaw (..),

    -- * HGECommand
    HGECommand (..),
    _HCServe,

    -- * ServeOptionsRaw
    ServeOptionsRaw (..),
    ConsoleStatus (..),
    isConsoleEnabled,
    AdminInternalErrorsStatus (..),
    isAdminInternalErrorsEnabled,
    isWebSocketCompressionEnabled,
    AllowListStatus (..),
    isAllowListEnabled,
    DevModeStatus (..),
    isDevModeEnabled,
    TelemetryStatus (..),
    isTelemetryEnabled,
    WsReadCookieStatus (..),
    isWsReadCookieEnabled,
    Port,
    _getPort,
    mkPort,
    unsafePort,
    API (..),
    KeepAliveDelay (..),
    OptionalInterval (..),
    AuthHookRaw (..),
    ConnParamsRaw (..),
    ResponseInternalErrorsConfig (..),
    WSConnectionInitTimeout (..),
    msToOptionalInterval,
    rawConnDetailsToUrl,
    rawConnDetailsToUrlText,
    shouldIncludeInternal,

    -- * ServeOptions
    ServeOptions (..),

    -- * Downgrade Options
    DowngradeOptions (..),
    -- $experimentalFeatures
    -- $readOnlyMode
  )
where

--------------------------------------------------------------------------------

import Control.Lens (Lens', Prism')
import Control.Lens qualified as Lens
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.URL.Template qualified as Template
import Database.PG.Query qualified as Query
import Hasura.Backends.Postgres.Connection.MonadTx qualified as MonadTx
import Hasura.GraphQL.Execute.Subscription.Options qualified as Subscription.Options
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata (MetadataDefaults)
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.RQL.Types.Schema.Options qualified as Schema.Options
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Logging qualified as Server.Logging
import Hasura.Server.Types qualified as Server.Types
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WebSockets
import Refined (NonNegative, Positive, Refined, unrefine)

--------------------------------------------------------------------------------

-- | The collected default value, env var, and help message for an
-- option. If there should be no default value then use 'Option ()'.
data Option def = Option
  { _default :: def,
    _envVar :: String,
    _helpMessage :: String
  }
  deriving (Functor)

-- | Helper function for pretty printing @Option a@.
optionPP :: Option a -> (String, String)
optionPP = _envVar &&& _helpMessage

--------------------------------------------------------------------------------

-- | Raw HGE Options from the arg parser and the env.
data HGEOptionsRaw impl = HGEOptionsRaw
  { _horDatabaseUrl :: PostgresConnInfo (Maybe PostgresConnInfoRaw),
    _horMetadataDbUrl :: Maybe String,
    _horCommand :: HGECommand impl
  }

horDatabaseUrl :: Lens' (HGEOptionsRaw impl) (PostgresConnInfo (Maybe PostgresConnInfoRaw))
horDatabaseUrl = Lens.lens _horDatabaseUrl $ \hdu a -> hdu {_horDatabaseUrl = a}

horMetadataDbUrl :: Lens' (HGEOptionsRaw impl) (Maybe String)
horMetadataDbUrl = Lens.lens _horMetadataDbUrl $ \hdu a -> hdu {_horMetadataDbUrl = a}

horCommand :: Lens' (HGEOptionsRaw impl) (HGECommand impl)
horCommand = Lens.lens _horCommand $ \hdu a -> hdu {_horCommand = a}

--------------------------------------------------------------------------------

-- | The final processed HGE options.
data HGEOptions impl = HGEOptions
  { _hoDatabaseUrl :: PostgresConnInfo (Maybe Common.UrlConf),
    _hoMetadataDbUrl :: Maybe String,
    _hoCommand :: HGECommand impl
  }

hoCommand :: Lens' (HGEOptions impl) (HGECommand impl)
hoCommand = Lens.lens _hoCommand $ \hdu a -> hdu {_hoCommand = a}

--------------------------------------------------------------------------------

-- | Postgres connection info tupled with a retry count.
--
-- In practice, the @a@ here is one of the following:
-- 1. 'Maybe PostgresConnInfoRaw'
-- 2. 'Maybe UrlConf'
-- 3. 'Maybe Text'
-- 4. 'Maybe DatabaseUrl' where 'DatabaseUrl' is an alias for 'Text'
--
-- If it contains a 'Maybe PostgresConnInfoRaw' then you have not yet
-- processed your arg parser results.
data PostgresConnInfo a = PostgresConnInfo
  { _pciDatabaseConn :: a,
    _pciRetries :: Maybe Int
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

pciDatabaseConn :: Lens' (PostgresConnInfo a) a
pciDatabaseConn = Lens.lens _pciDatabaseConn $ \pci a -> pci {_pciDatabaseConn = a}

pciRetries :: Lens' (PostgresConnInfo a) (Maybe Int)
pciRetries = Lens.lens _pciRetries $ \pci mi -> pci {_pciRetries = mi}

--------------------------------------------------------------------------------

-- | Postgres Connection info in the form of a templated URI string or
-- structured data.
data PostgresConnInfoRaw
  = PGConnDatabaseUrl Template.Template
  | PGConnDetails PostgresConnDetailsRaw
  deriving (Show, Eq)

mkUrlConnInfo :: String -> PostgresConnInfoRaw
mkUrlConnInfo = PGConnDatabaseUrl . Template.mkPlainTemplate . Text.pack

_PGConnDatabaseUrl :: Prism' PostgresConnInfoRaw Template.Template
_PGConnDatabaseUrl = Lens.prism' PGConnDatabaseUrl $ \case
  PGConnDatabaseUrl template -> Just template
  PGConnDetails _ -> Nothing

_PGConnDetails :: Prism' PostgresConnInfoRaw PostgresConnDetailsRaw
_PGConnDetails = Lens.prism' PGConnDetails $ \case
  PGConnDatabaseUrl _ -> Nothing
  PGConnDetails prcd -> Just prcd

rawConnDetailsToUrl :: PostgresConnDetailsRaw -> Template.Template
rawConnDetailsToUrl =
  Template.mkPlainTemplate . rawConnDetailsToUrlText

--------------------------------------------------------------------------------

-- | Structured Postgres connection information as provided by the arg
-- parser or env vars.
data PostgresConnDetailsRaw = PostgresConnDetailsRaw
  { connHost :: String,
    connPort :: Int,
    connUser :: String,
    connPassword :: String,
    connDatabase :: String,
    connOptions :: Maybe String
  }
  deriving (Eq, Read, Show)

instance FromJSON PostgresConnDetailsRaw where
  parseJSON = J.withObject "PostgresConnDetailsRaw" \o -> do
    connHost <- o .: "host"
    connPort <- o .: "port"
    connUser <- o .: "user"
    connPassword <- o .: "password"
    connDatabase <- o .: "database"
    connOptions <- o .:? "options"
    pure $ PostgresConnDetailsRaw {..}

instance ToJSON PostgresConnDetailsRaw where
  toJSON PostgresConnDetailsRaw {..} =
    J.object
      $ [ "host" .= connHost,
          "port" .= connPort,
          "user" .= connUser,
          "password" .= connPassword,
          "database" .= connDatabase
        ]
      <> catMaybes [fmap ("options" .=) connOptions]

rawConnDetailsToUrlText :: PostgresConnDetailsRaw -> Text
rawConnDetailsToUrlText PostgresConnDetailsRaw {..} =
  Text.pack
    $ "postgresql://"
    <> connUser
    <> ":"
    <> connPassword
    <> "@"
    <> connHost
    <> ":"
    <> show connPort
    <> "/"
    <> connDatabase
    <> maybe "" ("?options=" <>) connOptions

--------------------------------------------------------------------------------

-- | The HGE Arg parser Command choices.
--
-- This is polymorphic so that we can pack either 'ServeOptionsRaw' or
-- 'ProServeOptionsRaw' in it.
data HGECommand a
  = HCServe a
  | HCExport
  | HCClean
  | HCVersion
  | HCDowngrade !DowngradeOptions
  deriving (Show, Eq)

_HCServe :: Prism' (HGECommand a) a
_HCServe = Lens.prism' HCServe \case
  HCServe a -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

-- | The Serve Command options accumulated from the Arg and Env parsers.
--
-- NOTE: A 'Nothing' value indicates the absence of a particular
-- flag. Hence types such as 'Maybe (HashSet X)' or 'Maybe Bool'.
data ServeOptionsRaw impl = ServeOptionsRaw
  { rsoPort :: Maybe Port,
    rsoHost :: Maybe Warp.HostPreference,
    rsoConnParams :: ConnParamsRaw,
    rsoTxIso :: Maybe Query.TxIsolation,
    rsoAdminSecret :: Maybe Auth.AdminSecretHash,
    rsoAuthHook :: AuthHookRaw,
    rsoJwtSecret :: Maybe Auth.JWTConfig,
    rsoUnAuthRole :: Maybe RoleName,
    rsoCorsConfig :: Maybe Cors.CorsConfig,
    rsoConsoleStatus :: ConsoleStatus,
    rsoConsoleAssetsDir :: Maybe Text,
    rsoConsoleSentryDsn :: Maybe Text,
    rsoEnableTelemetry :: Maybe TelemetryStatus,
    rsoWsReadCookie :: WsReadCookieStatus,
    rsoStringifyNum :: Schema.Options.StringifyNumbers,
    rsoDangerousBooleanCollapse :: Maybe Schema.Options.DangerouslyCollapseBooleans,
    rsoRemoteNullForwardingPolicy :: Maybe Schema.Options.RemoteNullForwardingPolicy,
    rsoEnabledAPIs :: Maybe (HashSet API),
    rsoMxRefetchInt :: Maybe Subscription.Options.RefetchInterval,
    rsoMxBatchSize :: Maybe Subscription.Options.BatchSize,
    -- We have different config options for livequery and streaming subscriptions
    rsoStreamingMxRefetchInt :: Maybe Subscription.Options.RefetchInterval,
    rsoStreamingMxBatchSize :: Maybe Subscription.Options.BatchSize,
    rsoEnableAllowList :: AllowListStatus,
    rsoEnabledLogTypes :: Maybe (HashSet (Logging.EngineLogType impl)),
    rsoLogLevel :: Maybe Logging.LogLevel,
    rsoDevMode :: DevModeStatus,
    rsoAdminInternalErrors :: Maybe AdminInternalErrorsStatus,
    rsoEventsHttpPoolSize :: Maybe (Refined Positive Int),
    rsoEventsFetchInterval :: Maybe (Refined NonNegative Milliseconds),
    rsoAsyncActionsFetchInterval :: Maybe OptionalInterval,
    rsoEnableRemoteSchemaPermissions :: Schema.Options.RemoteSchemaPermissions,
    rsoWebSocketCompression :: WebSockets.CompressionOptions,
    rsoWebSocketKeepAlive :: Maybe KeepAliveDelay,
    rsoInferFunctionPermissions :: Maybe Schema.Options.InferFunctionPermissions,
    rsoEnableMaintenanceMode :: Server.Types.MaintenanceMode (),
    rsoSchemaPollInterval :: Maybe OptionalInterval,
    -- | See Note '$experimentalFeatures' at bottom of module
    rsoExperimentalFeatures :: Maybe (HashSet Server.Types.ExperimentalFeature),
    rsoEventsFetchBatchSize :: Maybe (Refined NonNegative Int),
    rsoGracefulShutdownTimeout :: Maybe (Refined NonNegative Seconds),
    rsoWebSocketConnectionInitTimeout :: Maybe WSConnectionInitTimeout,
    rsoEnableMetadataQueryLoggingEnv :: Server.Logging.MetadataQueryLoggingMode,
    -- | stores global default naming convention
    rsoDefaultNamingConvention :: Maybe NamingCase,
    rsoExtensionsSchema :: Maybe MonadTx.ExtensionsSchema,
    rsoMetadataDefaults :: Maybe MetadataDefaults,
    rsoApolloFederationStatus :: Maybe Server.Types.ApolloFederationStatus,
    rsoCloseWebsocketsOnMetadataChangeStatus :: Maybe Server.Types.CloseWebsocketsOnMetadataChangeStatus,
    rsoMaxTotalHeaderLength :: Maybe Int,
    rsoTriggersErrorLogLevelStatus :: Maybe Server.Types.TriggersErrorLogLevelStatus,
    rsoAsyncActionsFetchBatchSize :: Maybe Int,
    rsoPersistedQueries :: Maybe Server.Types.PersistedQueriesState,
    rsoPersistedQueriesTtl :: Maybe Int
  }

-- | Whether or not to serve Console assets.
data ConsoleStatus = ConsoleEnabled | ConsoleDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData ConsoleStatus

instance Hashable ConsoleStatus

isConsoleEnabled :: ConsoleStatus -> Bool
isConsoleEnabled = \case
  ConsoleEnabled -> True
  ConsoleDisabled -> False

instance FromJSON ConsoleStatus where
  parseJSON = fmap (bool ConsoleDisabled ConsoleEnabled) . J.parseJSON

instance ToJSON ConsoleStatus where
  toJSON = J.toJSON . isConsoleEnabled

-- | Whether or not internal errors will be sent in response to admin.
data AdminInternalErrorsStatus = AdminInternalErrorsEnabled | AdminInternalErrorsDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData AdminInternalErrorsStatus

instance Hashable AdminInternalErrorsStatus

isAdminInternalErrorsEnabled :: AdminInternalErrorsStatus -> Bool
isAdminInternalErrorsEnabled = \case
  AdminInternalErrorsEnabled -> True
  AdminInternalErrorsDisabled -> False

instance FromJSON AdminInternalErrorsStatus where
  parseJSON = fmap (bool AdminInternalErrorsDisabled AdminInternalErrorsEnabled) . J.parseJSON

instance ToJSON AdminInternalErrorsStatus where
  toJSON = J.toJSON . isAdminInternalErrorsEnabled

isWebSocketCompressionEnabled :: WebSockets.CompressionOptions -> Bool
isWebSocketCompressionEnabled = \case
  WebSockets.PermessageDeflateCompression _ -> True
  WebSockets.NoCompression -> False

-- | A representation of whether or not to enable the GraphQL Query AllowList.
--
-- See: https://hasura.io/docs/latest/security/allow-list/#enable-allow-list
data AllowListStatus = AllowListEnabled | AllowListDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData AllowListStatus

instance Hashable AllowListStatus

isAllowListEnabled :: AllowListStatus -> Bool
isAllowListEnabled = \case
  AllowListEnabled -> True
  AllowListDisabled -> False

instance FromJSON AllowListStatus where
  parseJSON = fmap (bool AllowListDisabled AllowListEnabled) . J.parseJSON

instance ToJSON AllowListStatus where
  toJSON = J.toJSON . isAllowListEnabled

-- | A representation of whether or not to enable Hasura Dev Mode.
--
-- See: https://hasura.io/docs/latest/deployment/graphql-engine-flags/config-examples/#dev-mode
data DevModeStatus = DevModeEnabled | DevModeDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData DevModeStatus

instance Hashable DevModeStatus

isDevModeEnabled :: DevModeStatus -> Bool
isDevModeEnabled = \case
  DevModeEnabled -> True
  DevModeDisabled -> False

instance FromJSON DevModeStatus where
  parseJSON = fmap (bool DevModeDisabled DevModeEnabled) . J.parseJSON

instance ToJSON DevModeStatus where
  toJSON = J.toJSON . isDevModeEnabled

-- | A representation of whether or not to enable telemetry that is isomorphic to 'Bool'.
data TelemetryStatus = TelemetryEnabled | TelemetryDisabled
  deriving stock (Show, Eq, Ord, Generic)

instance NFData TelemetryStatus

instance Hashable TelemetryStatus

isTelemetryEnabled :: TelemetryStatus -> Bool
isTelemetryEnabled = \case
  TelemetryEnabled -> True
  TelemetryDisabled -> False

instance FromJSON TelemetryStatus where
  parseJSON = fmap (bool TelemetryDisabled TelemetryEnabled) . J.parseJSON

instance ToJSON TelemetryStatus where
  toJSON = J.toJSON . isTelemetryEnabled

-- | A representation of whether or not to read the websocket cookie
-- on initial handshake that is isomorphic to 'Bool'. See
-- 'wsReadCookieOption' for more details.
data WsReadCookieStatus = WsReadCookieEnabled | WsReadCookieDisabled
  deriving stock (Show, Eq, Generic)

instance NFData WsReadCookieStatus

instance Hashable WsReadCookieStatus

isWsReadCookieEnabled :: WsReadCookieStatus -> Bool
isWsReadCookieEnabled = \case
  WsReadCookieEnabled -> True
  WsReadCookieDisabled -> False

instance FromJSON WsReadCookieStatus where
  parseJSON = fmap (bool WsReadCookieDisabled WsReadCookieEnabled) . J.parseJSON

instance ToJSON WsReadCookieStatus where
  toJSON = J.toJSON . isWsReadCookieEnabled

-- | An 'Int' representing a Port number in the range 0 to 65536.
newtype Port = Port {_getPort :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, NFData, Hashable)

mkPort :: Int -> Maybe Port
mkPort x = case x >= 0 && x < 65536 of
  True -> Just $ Port x
  False -> Nothing

unsafePort :: Int -> Port
unsafePort = Port

instance FromJSON Port where
  parseJSON = J.withScientific "Int" $ \t -> do
    case t > 0 && t < 65536 of
      True -> maybe (fail "integer passed is out of bounds") (pure . Port) $ Scientific.toBoundedInteger t
      False -> fail "integer passed is out of bounds"

data API
  = METADATA
  | GRAPHQL
  | PGDUMP
  | DEVELOPER
  | CONFIG
  | METRICS
  deriving (Show, Eq, Read, Generic)

instance FromJSON API where
  parseJSON = J.withText "API" \case
    "metadata" -> pure METADATA
    "graphql" -> pure GRAPHQL
    "pgdump" -> pure PGDUMP
    "developer" -> pure DEVELOPER
    "config" -> pure CONFIG
    "metrics" -> pure METRICS
    x -> fail $ "unexpected string '" <> show x <> "'."

instance ToJSON API where
  toJSON = \case
    METADATA -> J.String "metadata"
    GRAPHQL -> J.String "graphql"
    PGDUMP -> J.String "pgdump"
    DEVELOPER -> J.String "developer"
    CONFIG -> J.String "config"
    METRICS -> J.String "metrics"

instance Hashable API

data AuthHookRaw = AuthHookRaw
  { ahrUrl :: Maybe Text,
    ahrType :: Maybe Auth.AuthHookType,
    ahrSendRequestBody :: Maybe Bool
  }

-- | Sleep time interval for recurring activities such as (@'asyncActionsProcessor')
--   Presently 'msToOptionalInterval' interprets `0` as Skip.
data OptionalInterval
  = -- | No polling
    Skip
  | -- | Interval time
    Interval (Refined NonNegative Milliseconds)
  deriving (Show, Eq)

msToOptionalInterval :: Refined NonNegative Milliseconds -> OptionalInterval
msToOptionalInterval = \case
  (unrefine -> 0) -> Skip
  s -> Interval s

instance FromJSON OptionalInterval where
  parseJSON v = msToOptionalInterval <$> J.parseJSON v

instance ToJSON OptionalInterval where
  toJSON = \case
    Skip -> J.toJSON @Milliseconds 0
    Interval s -> J.toJSON s

-- | The Raw configuration data from the Arg and Env parsers needed to
-- construct a 'ConnParams'
data ConnParamsRaw = ConnParamsRaw
  { -- NOTE: Should any of these types be 'PositiveInt'?
    rcpStripes :: Maybe (Refined NonNegative Int),
    rcpConns :: Maybe (Refined NonNegative Int),
    rcpIdleTime :: Maybe (Refined NonNegative Int),
    -- | Time from connection creation after which to destroy a connection and
    -- choose a different/new one.
    rcpConnLifetime :: Maybe (Refined NonNegative Time.NominalDiffTime),
    rcpAllowPrepare :: Maybe Bool,
    -- | See @HASURA_GRAPHQL_PG_POOL_TIMEOUT@
    rcpPoolTimeout :: Maybe (Refined NonNegative Time.NominalDiffTime)
  }
  deriving (Show, Eq)

newtype KeepAliveDelay = KeepAliveDelay {unKeepAliveDelay :: Refined NonNegative Seconds}
  deriving (Eq, Show)

instance FromJSON KeepAliveDelay where
  parseJSON = J.withObject "KeepAliveDelay" \o -> do
    unKeepAliveDelay <- o .: "keep_alive_delay"
    pure $ KeepAliveDelay {..}

instance ToJSON KeepAliveDelay where
  toJSON KeepAliveDelay {..} =
    J.object ["keep_alive_delay" .= unKeepAliveDelay]

--------------------------------------------------------------------------------

-- | The timeout duration in 'Seconds' for a WebSocket connection.
newtype WSConnectionInitTimeout = WSConnectionInitTimeout {unWSConnectionInitTimeout :: Refined NonNegative Seconds}
  deriving newtype (Show, Eq, Ord)

instance FromJSON WSConnectionInitTimeout where
  parseJSON = J.withObject "WSConnectionInitTimeout" \o -> do
    unWSConnectionInitTimeout <- o .: "w_s_connection_init_timeout"
    pure $ WSConnectionInitTimeout {..}

instance ToJSON WSConnectionInitTimeout where
  toJSON WSConnectionInitTimeout {..} =
    J.object ["w_s_connection_init_timeout" .= unWSConnectionInitTimeout]

--------------------------------------------------------------------------------

-- | The final Serve Command options accummulated from the Arg Parser
-- and the Environment, fully processed and ready to apply when
-- running the server.
data ServeOptions impl = ServeOptions
  { soPort :: Port,
    soHost :: Warp.HostPreference,
    soConnParams :: Query.ConnParams,
    soTxIso :: Query.TxIsolation,
    soAdminSecret :: HashSet Auth.AdminSecretHash,
    soAuthHook :: Maybe Auth.AuthHook,
    soJwtSecret :: [Auth.JWTConfig],
    soUnAuthRole :: Maybe RoleName,
    soCorsConfig :: Cors.CorsConfig,
    soConsoleStatus :: ConsoleStatus,
    soConsoleAssetsDir :: Maybe Text,
    soConsoleSentryDsn :: Maybe Text,
    soEnableTelemetry :: TelemetryStatus,
    soStringifyNum :: Schema.Options.StringifyNumbers,
    soDangerousBooleanCollapse :: Schema.Options.DangerouslyCollapseBooleans,
    soRemoteNullForwardingPolicy :: Schema.Options.RemoteNullForwardingPolicy,
    soEnabledAPIs :: HashSet API,
    soLiveQueryOpts :: Subscription.Options.LiveQueriesOptions,
    soStreamingQueryOpts :: Subscription.Options.StreamQueriesOptions,
    soEnableAllowList :: AllowListStatus,
    soEnabledLogTypes :: HashSet (Logging.EngineLogType impl),
    soLogLevel :: Logging.LogLevel,
    soEventsHttpPoolSize :: Refined Positive Int,
    soEventsFetchInterval :: Refined NonNegative Milliseconds,
    soAsyncActionsFetchInterval :: OptionalInterval,
    soEnableRemoteSchemaPermissions :: Schema.Options.RemoteSchemaPermissions,
    soConnectionOptions :: WebSockets.ConnectionOptions,
    soWebSocketKeepAlive :: KeepAliveDelay,
    soInferFunctionPermissions :: Schema.Options.InferFunctionPermissions,
    soEnableMaintenanceMode :: Server.Types.MaintenanceMode (),
    soSchemaPollInterval :: OptionalInterval,
    -- | See note '$experimentalFeatures'
    soExperimentalFeatures :: HashSet Server.Types.ExperimentalFeature,
    soEventsFetchBatchSize :: Refined NonNegative Int,
    soDevMode :: DevModeStatus,
    soAdminInternalErrors :: AdminInternalErrorsStatus,
    soGracefulShutdownTimeout :: Refined NonNegative Seconds,
    soWebSocketConnectionInitTimeout :: WSConnectionInitTimeout,
    soEventingMode :: Server.Types.EventingMode,
    -- | See note '$readOnlyMode'
    soReadOnlyMode :: Server.Types.ReadOnlyMode,
    soEnableMetadataQueryLogging :: Server.Logging.MetadataQueryLoggingMode,
    soDefaultNamingConvention :: NamingCase,
    soExtensionsSchema :: MonadTx.ExtensionsSchema,
    soMetadataDefaults :: MetadataDefaults,
    soApolloFederationStatus :: Server.Types.ApolloFederationStatus,
    soCloseWebsocketsOnMetadataChangeStatus :: Server.Types.CloseWebsocketsOnMetadataChangeStatus,
    soMaxTotalHeaderLength :: Int,
    soTriggersErrorLogLevelStatus :: Server.Types.TriggersErrorLogLevelStatus,
    soAsyncActionsFetchBatchSize :: Int,
    soPersistedQueries :: Server.Types.PersistedQueriesState,
    soPersistedQueriesTtl :: Int
  }

-- | 'ResponseInternalErrorsConfig' represents the encoding of the
-- internal errors in the response to the client.
--
-- For more details, see this github comment:
-- https://github.com/hasura/graphql-engine/issues/4031#issuecomment-609747705
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

--------------------------------------------------------------------------------

-- | The Downgrade Command options. These are only sourced from the
-- Arg Parser and are used directly in 'Hasura.Server.Migrate'.
data DowngradeOptions = DowngradeOptions
  { dgoTargetVersion :: Text,
    dgoDryRun :: Bool
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- $experimentalFeatures
-- Note Experimental features:
--
-- The graphql-engine accepts a list of experimental features that can be
-- enabled at the startup. Experimental features are a way to introduce
-- new, but not stable features to our users in a manner in which they have
-- the choice to enable or disable a certain feature(s).
--
-- The objective of an experimental feature should be that when the feature is disabled,
-- the graphql-engine should work the same way as it worked before adding the said feature.
--
-- The experimental feature's flag is `--experimental-features` and the corresponding
-- environment variable is `HASURA_GRAPHQL_EXPERIMENTAL_FEATURES` which expects a comma-seperated
-- value.
--
-- When an experimental feature is stable enough i.e. it's stable through multiple non-beta releases
-- then we make the feature not experimental i.e. it will always be enabled. Note that when we do this
-- we still have to support parsing of the experimental feature because users of the previous version
-- will have it enabled and when they upgrade an error should not be thrown at the startup. For example:
--
-- The inherited roles was an experimental feature when introduced and it was enabled by
-- setting `--experimental-features` to `inherited_roles` and then it was decided to make the inherited roles
-- a stable feature, so it was removed as an experimental feature but the code was modified such that
-- `--experimental-features inherited_roles` to not throw an error.

--------------------------------------------------------------------------------

-- $readOnlyMode
-- Note ReadOnly Mode:
--
-- This mode starts the server in a (database) read-only mode. That is, only
-- read-only queries are allowed on users' database sources, and write
-- queries throw a runtime error. The use-case is for failsafe operations.
-- Metadata APIs are also disabled.
--
-- Following is the precise behaviour -
--   1. For any GraphQL API (relay/hasura; http/websocket) - disable execution of
--   mutations
--   2. Metadata API is disabled
--   3. /v2/query API - insert, delete, update, run_sql are disabled
--   4. /v1/query API - insert, delete, update, run_sql are disabled
--   5. No source catalog migrations are run
--   6. During build schema cache phase, building event triggers are disabled (as
--   they create corresponding database triggers)
