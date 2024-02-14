module Hasura.RQL.DDL.Schema.Cache.Config
  ( -- * static config
    CacheStaticConfig (..),
    HasCacheStaticConfig (..),

    -- * dynamic config
    CacheDynamicConfig (..),
  )
where

import Hasura.GraphQL.Schema.Common (SchemaSampledFeatureFlags)
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (SQLGenCtx)
import Hasura.RQL.Types.Metadata (MetadataDefaults)
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Types

--------------------------------------------------------------------------------
-- static config

-- | This type aggregates all of the "static" configuration of the cache build.
--
-- Static arguments are the ones that will not change during the execution of
-- the engine. They are a subset of the environment of the engine (see 'AppEnv'
-- and Note [Hasura Application State] for more information).
--
-- While 'AppEnv' has access to the union of *all* the static configuration of
-- the engine, more specific parts of the code should avoid relying directly on
-- it to avoid being tied to unrelated parts of the codebase. (See FIXME).
data CacheStaticConfig = CacheStaticConfig
  { _cscMaintenanceMode :: MaintenanceMode (),
    _cscEventingMode :: EventingMode,
    _cscReadOnlyMode :: ReadOnlyMode,
    _cscLogger :: Logger Hasura,
    -- | Native queries can be enabled or disabled on the fly via a feature
    -- flag, however we only recognise a change on a restart
    _cscAreNativeQueriesEnabled :: BackendType -> Bool,
    -- | Stored procedures can be enabled or disabled on the fly via a feature
    -- flag, however we only recognise a change on a restart
    _cscAreStoredProceduresEnabled :: Bool
  }

class (Monad m) => HasCacheStaticConfig m where
  askCacheStaticConfig :: m CacheStaticConfig

instance (HasCacheStaticConfig m) => HasCacheStaticConfig (ReaderT r m) where
  askCacheStaticConfig = lift askCacheStaticConfig

instance (HasCacheStaticConfig m) => HasCacheStaticConfig (ExceptT e m) where
  askCacheStaticConfig = lift askCacheStaticConfig

instance (HasCacheStaticConfig m) => HasCacheStaticConfig (StateT s m) where
  askCacheStaticConfig = lift askCacheStaticConfig

--------------------------------------------------------------------------------
-- dynamic config

-- | This type aggregates all of the "dynamic" configuration of the cache build.
--
-- Dynamic arguments are the ones that might change during the execution of the
-- engine. They are a subset of the 'AppContext' (see
-- Note [Hasura Application State] for more information).
--
-- While 'AppContext' has access to the union of *all* the dynamic configuration
-- of the engine, more specific parts of the code should avoid relying directly
-- on it to avoid being tied to unrelated parts of the codebase. (See FIXME).
data CacheDynamicConfig = CacheDynamicConfig
  { _cdcFunctionPermsCtx :: Options.InferFunctionPermissions,
    _cdcRemoteSchemaPermsCtx :: Options.RemoteSchemaPermissions,
    _cdcSQLGenCtx :: SQLGenCtx,
    _cdcExperimentalFeatures :: HashSet ExperimentalFeature,
    _cdcDefaultNamingConvention :: NamingCase,
    _cdcMetadataDefaults :: MetadataDefaults,
    _cdcApolloFederationStatus :: ApolloFederationStatus,
    _cdcCloseWebsocketsOnMetadataChangeStatus :: CloseWebsocketsOnMetadataChangeStatus,
    _cdcSchemaSampledFeatureFlags :: SchemaSampledFeatureFlags
  }
  deriving (Eq)
