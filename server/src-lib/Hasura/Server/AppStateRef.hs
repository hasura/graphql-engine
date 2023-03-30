module Hasura.Server.AppStateRef
  ( -- * AppState
    AppStateRef (..),
    AppState (..),
    initialiseAppStateRef,
    withSchemaCacheUpdate,
    readAppContextRef,
    getRebuildableSchemaCacheWithVersion,

    -- * TLS AllowList reference
    TLSAllowListRef,
    createTLSAllowListRef,
    readTLSAllowList,

    -- * Utility
    getSchemaCache,
    getSchemaCacheWithVersion,
    getAppContext,
    logInconsistentMetadata,
  )
where

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import Hasura.App.State
import Hasura.Logging qualified as L
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.DDL.Schema
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Network
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Logging
import Hasura.Server.Metrics
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
  { asSchemaCache :: (RebuildableSchemaCache, SchemaCacheVer),
    asAppCtx :: RebuildableAppContext impl
  }

-- | Build a new 'AppStateRef'.
--
-- This function also updates the 'TLSAllowListRef' to make it point to the
-- newly minted 'SchemaCacheRef'.
initialiseAppStateRef ::
  MonadIO m =>
  TLSAllowListRef impl ->
  ServerMetrics ->
  RebuildableSchemaCache ->
  RebuildableAppContext impl ->
  m (AppStateRef impl)
initialiseAppStateRef (TLSAllowListRef tlsAllowListRef) serverMetrics rebuildableSchemaCache rebuildableAppCtx = liftIO $ do
  cacheLock <- newMVar ()
  let appState = AppState (rebuildableSchemaCache, initSchemaCacheVer) rebuildableAppCtx
  cacheCell <- newIORef appState
  let metadataVersionGauge = smSchemaCacheMetadataResourceVersion serverMetrics
  updateMetadataVersionGauge metadataVersionGauge rebuildableSchemaCache
  liftIO $ writeIORef tlsAllowListRef (Right cacheCell)
  pure $ AppStateRef cacheLock cacheCell metadataVersionGauge

-- | Set the 'AppStateRef' to the 'RebuildableSchemaCache' produced by the
-- given action.
--
-- An internal lock ensures that at most one update to the 'AppStateRef' may
-- proceed at a time.
withSchemaCacheUpdate ::
  (MonadIO m, MonadBaseControl IO m) =>
  (AppStateRef impl) ->
  L.Logger L.Hasura ->
  Maybe (STM.TVar Bool) ->
  m (a, RebuildableSchemaCache) ->
  m a
withSchemaCacheUpdate (AppStateRef lock cacheRef metadataVersionGauge) logger mLogCheckerTVar action =
  withMVarMasked lock $ const do
    (!res, !newSC) <- action
    liftIO do
      -- update schemacache in IO reference
      modifyIORef' cacheRef $ \appState ->
        let !newVer = incSchemaCacheVer (snd $ asSchemaCache appState)
         in appState {asSchemaCache = (newSC, newVer)}

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
getRebuildableSchemaCacheWithVersion :: AppStateRef impl -> IO (RebuildableSchemaCache, SchemaCacheVer)
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
newtype TLSAllowListRef impl
  = TLSAllowListRef
      ( IORef (Either [TlsAllow] (IORef (AppState impl)))
      )

-- | Creates a new 'TLSAllowListRef' that points to the given list.
createTLSAllowListRef :: [TlsAllow] -> IO (TLSAllowListRef impl)
createTLSAllowListRef = fmap TLSAllowListRef . newIORef . Left

-- | Reads the TLS AllowList by attempting to read from the schema cache, and
-- defaulting to the list given when the ref was created.
readTLSAllowList :: TLSAllowListRef impl -> IO [TlsAllow]
readTLSAllowList (TLSAllowListRef ref) =
  readIORef ref >>= \case
    Right scRef -> scTlsAllowlist . lastBuiltSchemaCache . fst . asSchemaCache <$> readIORef scRef
    Left list -> pure list

--------------------------------------------------------------------------------
-- Utility functions

-- | Read the latest 'SchemaCache' from the 'AppStateRef'.
getSchemaCache :: AppStateRef impl -> IO SchemaCache
getSchemaCache asRef = lastBuiltSchemaCache . fst <$> getRebuildableSchemaCacheWithVersion asRef

-- | Read the latest 'SchemaCache' and its version from the 'AppStateRef'.
getSchemaCacheWithVersion :: AppStateRef impl -> IO (SchemaCache, SchemaCacheVer)
getSchemaCacheWithVersion scRef = fmap (\(sc, ver) -> (lastBuiltSchemaCache sc, ver)) $ getRebuildableSchemaCacheWithVersion scRef

-- | Read the latest 'AppContext' from the 'AppStateRef'.
getAppContext :: AppStateRef impl -> IO AppContext
getAppContext asRef = lastBuiltAppContext <$> readAppContextRef asRef

-- | Formats and logs a list of inconsistent metadata objects.
logInconsistentMetadata :: L.Logger L.Hasura -> [InconsistentMetadata] -> IO ()
logInconsistentMetadata logger objs =
  unless (null objs) $
    L.unLogger logger $
      mkInconsMetadataLog objs

--------------------------------------------------------------------------------
-- Local helpers

-- | Set the gauge metric to the metadata version of the schema cache, if it exists.
updateMetadataVersionGauge :: MonadIO m => Gauge -> RebuildableSchemaCache -> m ()
updateMetadataVersionGauge metadataVersionGauge schemaCache = do
  let metadataVersion = scMetadataResourceVersion . lastBuiltSchemaCache $ schemaCache
  liftIO $ Gauge.set metadataVersionGauge $ getMetadataResourceVersion metadataVersion
