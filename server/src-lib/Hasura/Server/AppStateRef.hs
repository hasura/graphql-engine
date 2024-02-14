module Hasura.Server.AppStateRef
  ( -- * AppState
    AppStateRef,
    initialiseAppStateRef,
    withSchemaCacheUpdate,
    withAppContextUpdate,
    updateAppStateRef,

    -- * TLS AllowList reference
    TLSAllowListRef,
    createTLSAllowListRef,
    readTLSAllowList,

    -- * Metrics config reference
    MetricsConfigRef,
    createMetricsConfigRef,
    readMetricsConfig,

    -- * Utility
    getSchemaCache,
    getSchemaCacheWithVersion,
    getRebuildableSchemaCacheWithVersion,
    readAppContextRef,
    getAppContext,
    logInconsistentMetadata,
    withSchemaCacheReadUpdate,
  )
where

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import Hasura.App.State
import Hasura.Base.Error
import Hasura.Logging qualified as L
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.DDL.Schema
import Hasura.RQL.Types.Common (MetricsConfig)
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Logging
import Hasura.Server.Metrics
import Network.Types.Extended
import System.Metrics.Gauge (Gauge)
import System.Metrics.Gauge qualified as Gauge

--------------------------------------------------------------------------------
-- AppState

-- | A mutable reference to a 'AppState', plus
--
-- * a write lock,
-- * update version tracking, and
-- * a gauge metric that tracks the metadata version of the 'SchemaCache'.
data AppStateRef impl = AppStateRef
  { -- | The idea behind explicit locking here is to
    --
    --   1. Allow maximum throughput for serving requests (/v1/graphql) (as each
    --      request reads the current schemacache)
    --   2. We don't want to process more than one request at any point of time
    --      which would modify the schema cache as such queries are expensive.
    --
    -- Another option is to consider removing this lock in place of `_scrCache ::
    -- MVar ...` if it's okay or in fact correct to block during schema update in
    -- e.g.  _wseGCtxMap. Vamshi says: It is theoretically possible to have a
    -- situation (in between building new schemacache and before writing it to
    -- the IORef) where we serve a request with a stale schemacache but I guess
    -- it is an okay trade-off to pay for a higher throughput (I remember doing a
    -- bunch of benchmarks to test this hypothesis).
    _scrLock :: MVar (),
    _scrCache :: IORef (AppState impl),
    -- | The gauge metric that tracks the current metadata version.
    --
    -- Invariant: This gauge must be updated via 'updateMetadataVersionGauge'
    -- whenever the _scrCache IORef is updated.
    _scrMetadataVersionGauge :: Gauge
  }

-- | A mutable reference to '(RebuildableSchemaCache, SchemaCacheVer)' and 'RebuildableAppContext'
data AppState impl = AppState
  { asSchemaCache :: RebuildableSchemaCache,
    asAppCtx :: RebuildableAppContext impl
  }

-- | Build a new 'AppStateRef'.
--
-- This function also updates the 'TLSAllowListRef' to make it point to the
-- newly minted 'SchemaCacheRef'.
initialiseAppStateRef ::
  (MonadIO m) =>
  TLSAllowListRef ->
  Maybe MetricsConfigRef ->
  ServerMetrics ->
  RebuildableSchemaCache ->
  RebuildableAppContext impl ->
  m (AppStateRef impl)
initialiseAppStateRef (TLSAllowListRef tlsAllowListRef) metricsConfigRefM serverMetrics rebuildableSchemaCache rebuildableAppCtx = liftIO do
  cacheLock <- newMVar ()
  let appState = AppState rebuildableSchemaCache rebuildableAppCtx
  cacheCell <- newIORef appState
  let metadataVersionGauge = smSchemaCacheMetadataResourceVersion serverMetrics
  updateMetadataVersionGauge metadataVersionGauge rebuildableSchemaCache
  let ref = AppStateRef cacheLock cacheCell metadataVersionGauge
  liftIO $ writeIORef tlsAllowListRef (scTlsAllowlist <$> getSchemaCache ref)
  for_ metricsConfigRefM \(MetricsConfigRef metricsConfigRef) ->
    liftIO $ writeIORef metricsConfigRef (scMetricsConfig <$> getSchemaCache ref)
  pure ref

-- TODO: This function might not be needed at all. This function is used only in `refreshSchemaCache` and we
-- can use `withSchemaCacheReadUpdate` there.
withSchemaCacheUpdate ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  (AppStateRef impl) ->
  L.Logger L.Hasura ->
  Maybe (STM.TVar Bool) ->
  m (a, RebuildableSchemaCache) ->
  m a
withSchemaCacheUpdate asr logger mLogCheckerTVar action =
  withSchemaCacheReadUpdate asr logger mLogCheckerTVar (const action)

-- | Set the 'AppStateRef' to the 'RebuildableSchemaCache' produced by the
-- given action.
--
-- An internal lock ensures that at most one update to the 'AppStateRef' may
-- proceed at a time.
withSchemaCacheReadUpdate ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  (AppStateRef impl) ->
  L.Logger L.Hasura ->
  Maybe (STM.TVar Bool) ->
  (RebuildableSchemaCache -> m (a, RebuildableSchemaCache)) ->
  m a
withSchemaCacheReadUpdate (AppStateRef lock cacheRef metadataVersionGauge) logger mLogCheckerTVar action =
  withMVarMasked lock $ const do
    rebuildableSchemaCache <- asSchemaCache <$> liftIO (readIORef cacheRef)
    (!res, !newSC) <- action rebuildableSchemaCache
    when (scMetadataResourceVersion (lastBuiltSchemaCache newSC) == MetadataResourceVersion (-1))
      $ throw500 "Programming error: attempting to save Schema Cache with incorrect mrv. Please report this to Hasura."
    liftIO do
      -- update schemacache in IO reference
      modifyIORef' cacheRef $ \appState ->
        appState {asSchemaCache = newSC}

      -- update metric with new metadata version
      updateMetadataVersionGauge metadataVersionGauge newSC

      let inconsistentObjectsList = scInconsistentObjs $ lastBuiltSchemaCache newSC
          logInconsistentMetadata' = logInconsistentMetadata logger inconsistentObjectsList
      -- log any inconsistent objects only once and not everytime this method is called
      case mLogCheckerTVar of
        Nothing -> logInconsistentMetadata'
        Just logCheckerTVar -> do
          logCheck <- STM.readTVarIO logCheckerTVar
          if null inconsistentObjectsList && logCheck
            then do
              STM.atomically $ STM.writeTVar logCheckerTVar False
            else do
              unless (logCheck || null inconsistentObjectsList) $ do
                STM.atomically $ STM.writeTVar logCheckerTVar True
                logInconsistentMetadata'
    pure res

-- | Read the contents of the 'AppStateRef' to get the latest 'RebuildableAppContext'
readAppContextRef :: AppStateRef impl -> IO (RebuildableAppContext impl)
readAppContextRef scRef = asAppCtx <$> readIORef (_scrCache scRef)

-- | Read the contents of the 'AppStateRef' to get the latest 'RebuildableSchemaCache' and 'SchemaCacheVer'
getRebuildableSchemaCacheWithVersion :: AppStateRef impl -> IO RebuildableSchemaCache
getRebuildableSchemaCacheWithVersion scRef = asSchemaCache <$> readIORef (_scrCache scRef)

--------------------------------------------------------------------------------
-- TLS Allow List

-- | Reference to a TLS AllowList, used for dynamic TLS settings in the app's
-- HTTP Manager.
--
-- This exists to break a chicken-and-egg problem in the initialisation of the
-- engine: the IO action that dynamically reads the TLS settings reads it from
-- the schema cache; but to build the schema cache we need a HTTP manager that
-- has access to the TLS settings... In the past, we were using a temporary HTTP
-- Manager to create the first schema cache, to then create the *real* Manager
-- that would refer to the list in the schema cache. Now, instead, we only
-- create one Manager, which uses a 'TLSAllowListRef' to dynamically access the
-- Allow List.
newtype TLSAllowListRef = TLSAllowListRef (IORef (IO [TlsAllow]))

-- | Creates a new 'TLSAllowListRef' that points to the given list.
createTLSAllowListRef :: [TlsAllow] -> IO TLSAllowListRef
createTLSAllowListRef = fmap TLSAllowListRef . newIORef . pure

-- | Reads the TLS AllowList by attempting to read from the schema cache, and
-- defaulting to the list given when the ref was created.
readTLSAllowList :: TLSAllowListRef -> IO [TlsAllow]
readTLSAllowList (TLSAllowListRef ref) = join $ readIORef ref

--------------------------------------------------------------------------------
-- Metrics config

-- | Reference to the metadata's 'MetricsConfig'.
--
-- Similarly to the 'TLSAllowListRef', this exists to break a
-- chicken-and-egg problem in the initialisation of the engine: the
-- implementation of several behaviour classes requires access to said
-- config, but those classes are implemented on the app monad, that
-- doesn't have access to the schema cache. This small type allows the
-- app monad to have access to the config, even before we build the
-- first schema cache.
newtype MetricsConfigRef
  = MetricsConfigRef (IORef (IO MetricsConfig))

-- | Creates a new 'MetricsConfigRef' that points to the given config.
createMetricsConfigRef :: MetricsConfig -> IO (MetricsConfigRef)
createMetricsConfigRef = fmap MetricsConfigRef . newIORef . pure

-- | Reads the TLS AllowList by attempting to read from the schema cache, and
-- defaulting to the list given when the ref was created.
readMetricsConfig :: MetricsConfigRef -> IO MetricsConfig
readMetricsConfig (MetricsConfigRef ref) = join $ readIORef ref

--------------------------------------------------------------------------------
-- Utility functions

-- | Read the latest 'SchemaCache' from the 'AppStateRef'.
getSchemaCache :: AppStateRef impl -> IO SchemaCache
getSchemaCache asRef = lastBuiltSchemaCache <$> getRebuildableSchemaCacheWithVersion asRef

-- | Read the latest 'SchemaCache' and its version from the 'AppStateRef'.
getSchemaCacheWithVersion :: AppStateRef impl -> IO (SchemaCache)
getSchemaCacheWithVersion scRef = fmap lastBuiltSchemaCache $ getRebuildableSchemaCacheWithVersion scRef

-- | Read the latest 'AppContext' from the 'AppStateRef'.
getAppContext :: AppStateRef impl -> IO AppContext
getAppContext asRef = lastBuiltAppContext <$> readAppContextRef asRef

-- | Formats and logs a list of inconsistent metadata objects.
logInconsistentMetadata :: L.Logger L.Hasura -> [InconsistentMetadata] -> IO ()
logInconsistentMetadata logger objs =
  unless (null objs)
    $ L.unLogger logger
    $ mkInconsMetadataLog objs

--------------------------------------------------------------------------------
-- Local helpers

-- | Set the gauge metric to the metadata version of the schema cache, if it exists.
updateMetadataVersionGauge :: (MonadIO m) => Gauge -> RebuildableSchemaCache -> m ()
updateMetadataVersionGauge metadataVersionGauge schemaCache = do
  let metadataVersion = scMetadataResourceVersion . lastBuiltSchemaCache $ schemaCache
  liftIO $ Gauge.set metadataVersionGauge $ getMetadataResourceVersion metadataVersion

-- | Set the 'RebuildableAppContext' to the 'AppStateRef' produced by the given
-- action.
--
-- An internal lock ensures that at most one update to the 'AppStateRef' may
-- proceed at a time.
withAppContextUpdate ::
  (MonadIO m, MonadBaseControl IO m) =>
  AppStateRef impl ->
  m (a, RebuildableAppContext impl) ->
  m a
withAppContextUpdate (AppStateRef lock cacheRef _) action =
  withMVarMasked lock $ \() -> do
    (!res, !newCtx) <- action
    liftIO $ do
      -- update app ctx in IO reference
      modifyIORef' cacheRef $ \appState -> appState {asAppCtx = newCtx}
    return res

-- | Set the 'AppStateRef', atomically, to the ('RebuildableSchemaCache',
-- 'RebuildableAppContext') produced by the given action.
--
-- An internal lock ensures that at most one update to the 'AppStateRef' may
-- proceed at a time.
updateAppStateRef ::
  (MonadIO m, MonadBaseControl IO m) =>
  AppStateRef impl ->
  L.Logger L.Hasura ->
  (RebuildableAppContext impl -> m (RebuildableAppContext impl, RebuildableSchemaCache)) ->
  m ()
updateAppStateRef appStateRef@(AppStateRef lock cacheRef metadataVersionGauge) logger action =
  withMVarMasked lock $ const do
    rebuildableAppContext <- liftIO $ readAppContextRef appStateRef
    (!newAppCtx, !newSC) <- action rebuildableAppContext
    liftIO do
      -- update schemacache in IO reference
      modifyIORef' cacheRef $ \appState ->
        appState {asSchemaCache = newSC, asAppCtx = newAppCtx}

      -- update metric with new metadata version
      updateMetadataVersionGauge metadataVersionGauge newSC

      let inconsistentObjectsList = scInconsistentObjs $ lastBuiltSchemaCache newSC
          logInconsistentMetadata' = logInconsistentMetadata logger inconsistentObjectsList
      -- log any inconsistent objects everytime this method is called
      logInconsistentMetadata'
