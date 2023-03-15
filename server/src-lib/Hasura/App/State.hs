module Hasura.App.State
  ( AppEnv (..),
    AppContext (..),
    Loggers (..),
  )
where

import Control.Concurrent.STM qualified as STM
import Data.Environment qualified as Env
import Data.HashSet qualified as S
import Database.PG.Query qualified as PG
import Hasura.Eventing.Common (LockedEventsCtx)
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache (MetadataResourceVersion)
import Hasura.Server.Auth (AuthMode)
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init
import Hasura.Server.Logging
import Hasura.Server.Metrics
import Hasura.Server.Prometheus
import Hasura.Server.SchemaCacheRef
import Hasura.Server.Types
import Hasura.ShutdownLatch
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP
import Network.Wai.Handler.Warp (HostPreference)
import Network.WebSockets.Connection qualified as WebSockets
import Refined (NonNegative, Positive, Refined)

{- Note [Hasura Application State]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Hasura Application state represents the entire state of hasura.

Hasura Application State = AppEnv (static) + AppContext (dynamic)

Hasura Application State can be divided into two parts:

  1. Read-Only State (Static State):
  =================================
  The information required to build this state is provided only during the
  initialization of hasura. This information is immutable. If you want update any
  field in this state, you would need to shutdown the current instance and
  re-launch hausura with new information.

  Eg: If you want to run hasura in read-only mode, you would have to mention
      this information when hasura starts up. There is no way to make hasura
      run in read-only mode once it has booted up.

  2. Runtime Configurable State (Dynamic State):
  ==============================================
  The information present in this state can be updated during the runtime. This state
  is mutable and does not require a restart of hasura instance to take effect.

  The fields in the state are usually updated via Metadata API's or Hasura Console.

  Eg: You can change the entries in Allowlist via console and hasura need not restart
      for the changes to take effect.

-}

-- | Represents the Read-Only Hasura State, these fields are immutable and the state
-- cannot be changed during runtime.
data AppEnv = AppEnv
  { appEnvPort :: Port,
    appEnvHost :: HostPreference,
    appEnvMetadataDbPool :: PG.PGPool,
    appEnvManager :: HTTP.Manager,
    appEnvLoggers :: Loggers,
    appEnvMetadataVersionRef :: STM.TMVar MetadataResourceVersion,
    appEnvInstanceId :: InstanceId,
    appEnvEnableMaintenanceMode :: MaintenanceMode (),
    appEnvLoggingSettings :: LoggingSettings,
    appEnvEventingMode :: EventingMode,
    appEnvEnableReadOnlyMode :: ReadOnlyMode,
    appEnvServerMetrics :: ServerMetrics,
    appEnvShutdownLatch :: ShutdownLatch,
    appEnvMetaVersionRef :: STM.TMVar MetadataResourceVersion,
    appEnvPrometheusMetrics :: PrometheusMetrics,
    appEnvTraceSamplingPolicy :: Tracing.SamplingPolicy,
    appEnvSubscriptionState :: ES.SubscriptionsState,
    appEnvLockedEventsCtx :: LockedEventsCtx,
    appEnvConnParams :: PG.ConnParams,
    appEnvTxIso :: PG.TxIsolation,
    appEnvConsoleAssetsDir :: Maybe Text,
    appEnvConsoleSentryDsn :: Maybe Text,
    appEnvConnectionOptions :: WebSockets.ConnectionOptions,
    appEnvWebSocketKeepAlive :: KeepAliveDelay,
    appEnvWebSocketConnectionInitTimeout :: WSConnectionInitTimeout,
    appEnvGracefulShutdownTimeout :: Refined NonNegative Seconds,
    appEnvCheckFeatureFlag :: (FeatureFlag -> IO Bool)
  }

-- | Represents the Dynamic Hasura State, these field are mutable and can be changed
-- during runtime.
data AppContext = AppContext
  { acCacheRef :: SchemaCacheRef,
    acAuthMode :: AuthMode,
    acSQLGenCtx :: SQLGenCtx,
    acEnabledAPIs :: S.HashSet API,
    acEnableAllowlist :: AllowListStatus,
    acResponseInternalErrorsConfig :: ResponseInternalErrorsConfig,
    acEnvironment :: Env.Environment,
    acRemoteSchemaPermsCtx :: Options.RemoteSchemaPermissions,
    acFunctionPermsCtx :: Options.InferFunctionPermissions,
    acExperimentalFeatures :: S.HashSet ExperimentalFeature,
    acDefaultNamingConvention :: NamingCase,
    acMetadataDefaults :: MetadataDefaults,
    acLiveQueryOptions :: LiveQueriesOptions,
    acStreamQueryOptions :: StreamQueriesOptions,
    acCorsConfig :: Cors.CorsConfig,
    acConsoleStatus :: ConsoleStatus,
    acEnableTelemetry :: TelemetryStatus,
    acEventsHttpPoolSize :: Refined Positive Int,
    acEventsFetchInterval :: Refined NonNegative Milliseconds,
    acEventsFetchBatchSize :: Refined NonNegative Int,
    acAsyncActionsFetchInterval :: OptionalInterval,
    acSchemaPollInterval :: OptionalInterval,
    acApolloFederationStatus :: ApolloFederationStatus
  }

-- | Collection of the LoggerCtx, the regular Logger and the PGLogger
data Loggers = Loggers
  { _lsLoggerCtx :: L.LoggerCtx L.Hasura,
    _lsLogger :: L.Logger L.Hasura,
    _lsPgLogger :: PG.PGLogger
  }
