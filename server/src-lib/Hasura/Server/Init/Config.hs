-- | Types and classes related to configuration when the server is initialised
module Hasura.Server.Init.Config
  ( API (..),
    DowngradeOptions (..),
    HGECommand (..),
    _HCServe,
    HGEOptions (..),
    hoCommand,
    HGEOptionsRaw (..),
    horDatabaseUrl,
    horMetadataDbUrl,
    horCommand,
    KeepAliveDelay (..),
    OptionalInterval (..),
    PostgresConnInfo (..),
    pciDatabaseConn,
    pciRetries,
    PostgresRawConnDetails (..),
    PostgresRawConnInfo (..),
    _PGConnDatabaseUrl,
    _PGConnDetails,
    mkUrlConnInfo,
    RawAuthHook,
    RawConnParams (..),
    RawServeOptions (..),
    ResponseInternalErrorsConfig (..),
    ServeOptions (..),
    WSConnectionInitTimeout (..),
    defaultKeepAliveDelay,
    defaultWSConnectionInitTimeout,
    msToOptionalInterval,
    rawConnDetailsToUrl,
    rawConnDetailsToUrlText,
    shouldIncludeInternal,
  )
where

--------------------------------------------------------------------------------

import Control.Lens (Lens', Prism', lens, prism')
import Data.Aeson
import Data.Aeson qualified as J
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.URL.Template
import Database.PG.Query qualified as Q
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.NamingCase (NamingCase)
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.Server.Auth
import Hasura.Server.Cors
import Hasura.Server.Logging
import Hasura.Server.Types
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

-- TODO(SOLOMON): Monomorphize
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
    rsoStringifyNum :: Options.StringifyNumbers,
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
    rsoAsyncActionsFetchInterval :: Maybe OptionalInterval,
    rsoEnableRemoteSchemaPermissions :: Options.RemoteSchemaPermissions,
    rsoWebSocketCompression :: Bool,
    rsoWebSocketKeepAlive :: Maybe KeepAliveDelay,
    rsoInferFunctionPermissions :: Maybe Options.InferFunctionPermissions,
    rsoEnableMaintenanceMode :: MaintenanceMode (),
    rsoSchemaPollInterval :: Maybe OptionalInterval,
    -- see Note [Experimental features]
    rsoExperimentalFeatures :: Maybe [ExperimentalFeature],
    rsoEventsFetchBatchSize :: Maybe NonNegativeInt,
    rsoGracefulShutdownTimeout :: Maybe Seconds,
    rsoWebSocketConnectionInitTimeout :: Maybe WSConnectionInitTimeout,
    rsoEnableMetadataQueryLoggingEnv :: MetadataQueryLoggingMode,
    -- | stores global default naming convention
    rsoDefaultNamingConvention :: Maybe NamingCase
  }

--------------------------------------------------------------------------------

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

instance FromJSON KeepAliveDelay where
  parseJSON = withObject "KeepAliveDelay" \o -> do
    unKeepAliveDelay <- o J..: "keep_alive_delay"
    pure $ KeepAliveDelay {..}

instance ToJSON KeepAliveDelay where
  toJSON KeepAliveDelay {..} =
    J.object ["keep_alive_delay" J..= unKeepAliveDelay]

defaultKeepAliveDelay :: KeepAliveDelay
defaultKeepAliveDelay = KeepAliveDelay $ fromIntegral (5 :: Int)

newtype WSConnectionInitTimeout = WSConnectionInitTimeout {unWSConnectionInitTimeout :: Seconds}
  deriving (Eq, Show)

instance FromJSON WSConnectionInitTimeout where
  parseJSON = withObject "WSConnectionInitTimeout" \o -> do
    unWSConnectionInitTimeout <- o J..: "w_s_connection_init_timeout"
    pure $ WSConnectionInitTimeout {..}

instance ToJSON WSConnectionInitTimeout where
  toJSON WSConnectionInitTimeout {..} =
    J.object ["w_s_connection_init_timeout" J..= unWSConnectionInitTimeout]

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
    soStringifyNum :: Options.StringifyNumbers,
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
    soEnableRemoteSchemaPermissions :: Options.RemoteSchemaPermissions,
    soConnectionOptions :: WS.ConnectionOptions,
    soWebSocketKeepAlive :: KeepAliveDelay,
    soInferFunctionPermissions :: Options.InferFunctionPermissions,
    soEnableMaintenanceMode :: MaintenanceMode (),
    soSchemaPollInterval :: OptionalInterval,
    soExperimentalFeatures :: Set.HashSet ExperimentalFeature,
    soEventsFetchBatchSize :: NonNegativeInt,
    soDevMode :: Bool,
    soGracefulShutdownTimeout :: Seconds,
    soWebSocketConnectionInitTimeout :: WSConnectionInitTimeout,
    soEventingMode :: EventingMode,
    soReadOnlyMode :: ReadOnlyMode,
    soEnableMetadataQueryLogging :: MetadataQueryLoggingMode,
    soDefaultNamingConvention :: Maybe NamingCase
  }

--------------------------------------------------------------------------------

-- | The Downgrade Command options. These are only sourced from the
-- Arg Parser and are used directly in 'Hasura.Server.Migrate'.
data DowngradeOptions = DowngradeOptions
  { dgoTargetVersion :: !Text,
    dgoDryRun :: !Bool
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

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

pciDatabaseConn :: Lens' (PostgresConnInfo a) a
pciDatabaseConn = lens _pciDatabaseConn $ \pci a -> pci {_pciDatabaseConn = a}

pciRetries :: Lens' (PostgresConnInfo a) (Maybe Int)
pciRetries = lens _pciRetries $ \pci mi -> pci {_pciRetries = mi}

--------------------------------------------------------------------------------

-- | Postgres Connection info can come in the form of a templated URI
-- string or structured data.
data PostgresRawConnInfo
  = PGConnDatabaseUrl !URLTemplate
  | PGConnDetails !PostgresRawConnDetails
  deriving (Show, Eq)

mkUrlConnInfo :: String -> PostgresRawConnInfo
mkUrlConnInfo = PGConnDatabaseUrl . mkPlainURLTemplate . T.pack

_PGConnDatabaseUrl :: Prism' PostgresRawConnInfo URLTemplate
_PGConnDatabaseUrl = prism' PGConnDatabaseUrl $ \case
  PGConnDatabaseUrl template -> Just template
  PGConnDetails _ -> Nothing

_PGConnDetails :: Prism' PostgresRawConnInfo PostgresRawConnDetails
_PGConnDetails = prism' PGConnDetails $ \case
  PGConnDatabaseUrl _ -> Nothing
  PGConnDetails prcd -> Just prcd

rawConnDetailsToUrl :: PostgresRawConnDetails -> URLTemplate
rawConnDetailsToUrl =
  mkPlainURLTemplate . rawConnDetailsToUrlText

--------------------------------------------------------------------------------

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

instance FromJSON PostgresRawConnDetails where
  parseJSON = J.withObject "PostgresRawConnDetails" \o -> do
    connHost <- o J..: "host"
    connPort <- o J..: "port"
    connUser <- o J..: "user"
    connPassword <- o J..: "password"
    connDatabase <- o J..: "database"
    connOptions <- o J..:? "options"
    pure $ PostgresRawConnDetails {..}

instance ToJSON PostgresRawConnDetails where
  toJSON PostgresRawConnDetails {..} =
    J.object $
      [ "host" J..= connHost,
        "port" J..= connPort,
        "user" J..= connUser,
        "password" J..= connPassword,
        "database" J..= connDatabase
      ]
        <> catMaybes [fmap ("options" J..=) connOptions]

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

--------------------------------------------------------------------------------

-- | HGE Options from the arg parser and the env.
data HGEOptionsRaw impl = HGEOptionsRaw
  { _horDatabaseUrl :: !(PostgresConnInfo (Maybe PostgresRawConnInfo)),
    _horMetadataDbUrl :: !(Maybe String),
    _horCommand :: !(HGECommand impl)
  }

horDatabaseUrl :: Lens' (HGEOptionsRaw impl) (PostgresConnInfo (Maybe PostgresRawConnInfo))
horDatabaseUrl = lens _horDatabaseUrl $ \hdu a -> hdu {_horDatabaseUrl = a}

horMetadataDbUrl :: Lens' (HGEOptionsRaw impl) (Maybe String)
horMetadataDbUrl = lens _horMetadataDbUrl $ \hdu a -> hdu {_horMetadataDbUrl = a}

horCommand :: Lens' (HGEOptionsRaw impl) (HGECommand impl)
horCommand = lens _horCommand $ \hdu a -> hdu {_horCommand = a}

--------------------------------------------------------------------------------

-- | The final processed HGE options.
data HGEOptions impl = HGEOptions
  { _hoDatabaseUrl :: !(PostgresConnInfo (Maybe UrlConf)),
    _hoMetadataDbUrl :: !(Maybe String),
    _hoCommand :: !(HGECommand impl)
  }

hoCommand :: Lens' (HGEOptions impl) (HGECommand impl)
hoCommand = lens _hoCommand $ \hdu a -> hdu {_hoCommand = a}

--------------------------------------------------------------------------------

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

_HCServe :: Prism' (HGECommand a) a
_HCServe = prism' HCServe \case
  HCServe a -> Just a
  _ -> Nothing
