module Hasura.Server.SchemaUpdate
  ( startSchemaSyncListenerThread,
    startSchemaSyncProcessorThread,
    SchemaSyncThreadType (..),
  )
where

import Control.Concurrent.Extended qualified as C
import Control.Concurrent.STM qualified as STM
import Control.Immortal qualified as Immortal
import Control.Monad.Loops qualified as L
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Managed (ManagedT)
import Data.Aeson
import Data.Aeson.Casing
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Database.PG.Query qualified as PG
import Hasura.App.State
import Hasura.Base.Error
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Schema (runCacheRWT)
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DDL.Schema.Catalog
import Hasura.RQL.Types.BackendType (BackendType (..))
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Server.AppStateRef
  ( AppStateRef,
    getAppContext,
    getRebuildableSchemaCacheWithVersion,
    withSchemaCacheUpdate,
  )
import Hasura.Server.Logging
import Hasura.Server.Types
import Hasura.Services
import Hasura.Tracing qualified as Tracing
import Refined (NonNegative, Refined, unrefine)

data ThreadError
  = TEPayloadParse !Text
  | TEQueryError !QErr
  deriving (Generic)

instance ToJSON ThreadError where
  toJSON =
    genericToJSON
      defaultOptions
        { constructorTagModifier = snakeCase . drop 2,
          sumEncoding = TaggedObject "type" "info"
        }
  toEncoding =
    genericToEncoding
      defaultOptions
        { constructorTagModifier = snakeCase . drop 2,
          sumEncoding = TaggedObject "type" "info"
        }

logThreadStarted ::
  (MonadIO m) =>
  Logger Hasura ->
  InstanceId ->
  SchemaSyncThreadType ->
  Immortal.Thread ->
  m ()
logThreadStarted logger instanceId threadType thread =
  let msg = tshow threadType <> " thread started"
   in unLogger logger
        $ StartupLog LevelInfo "schema-sync"
        $ object
          [ "instance_id" .= getInstanceId instanceId,
            "thread_id" .= show (Immortal.threadId thread),
            "message" .= msg
          ]

{- Note [Schema Cache Sync]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When multiple graphql-engine instances are serving on same metadata storage,
each instance should have schema cache in sync with latest metadata. Somehow
all instances should communicate each other when any request has modified metadata.

We track the metadata schema version in postgres and poll for this
value in a thread.  When the schema version has changed, the instance
will update its local metadata schema and remove any invalidated schema cache data.

The following steps take place when an API request made to update metadata:

1. After handling the request we insert the new metadata schema json
   into a postgres tablealong with a schema version.

2. On start up, before initialising schema cache, an async thread is
   invoked to continuously poll the Postgres notifications table for
   the latest metadata schema version. The schema version is pushed to
   a shared `TMVar`.

3. Before starting API server, another async thread is invoked to
   process events pushed by the listener thread via the `TMVar`. If
   the instance's schema version is not current with the freshly
   updated TMVar version then we update the local metadata.

Why we need two threads if we can capture and reload schema cache in a single thread?

If we want to implement schema sync in a single async thread we have to invoke the same
after initialising schema cache. We may loose events that published after schema cache
init and before invoking the thread. In such case, schema cache is not in sync with metadata.
So we choose two threads in which one will start listening before schema cache init and the
other after it.

What happens if listen connection to Postgres is lost?

Listener thread will keep trying to establish connection to Postgres for every one second.
Once connection established, it pushes @'SSEListenStart' event with time. We aren't sure
about any metadata modify requests made in meanwhile. So we reload schema cache unconditionally
if listen started after schema cache init start time.

-}

-- | An async thread which listen to Postgres notify to enable schema syncing
-- See Note [Schema Cache Sync]
startSchemaSyncListenerThread ::
  (C.ForkableMonadIO m) =>
  Logger Hasura ->
  PG.PGPool ->
  InstanceId ->
  Refined NonNegative Milliseconds ->
  STM.TMVar MetadataResourceVersion ->
  ManagedT m (Immortal.Thread)
startSchemaSyncListenerThread logger pool instanceId interval metaVersionRef = do
  -- Start listener thread
  listenerThread <-
    C.forkManagedT "SchemeUpdate.listener" logger
      $ listener logger pool metaVersionRef (unrefine interval)
  logThreadStarted logger instanceId TTListener listenerThread
  pure listenerThread

-- | An async thread which processes the schema sync events
-- See Note [Schema Cache Sync]
startSchemaSyncProcessorThread ::
  ( Tracing.MonadTraceContext m,
    C.ForkableMonadIO m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    ProvidesNetwork m
  ) =>
  AppStateRef impl ->
  STM.TVar Bool ->
  ManagedT m Immortal.Thread
startSchemaSyncProcessorThread appStateRef logTVar = do
  AppEnv {..} <- lift askAppEnv
  let logger = _lsLogger appEnvLoggers
  -- Start processor thread
  processorThread <-
    C.forkManagedT "SchemeUpdate.processor" logger
      $ processor appEnvMetadataVersionRef appStateRef logTVar
  logThreadStarted logger appEnvInstanceId TTProcessor processorThread
  pure processorThread

-- TODO: This is also defined in multitenant, consider putting it in a library somewhere
forcePut :: STM.TMVar a -> a -> IO ()
forcePut v a = STM.atomically $ STM.tryTakeTMVar v >> STM.putTMVar v a

schemaVersionCheckHandler ::
  PG.PGPool -> STM.TMVar MetadataResourceVersion -> IO (Either QErr ())
schemaVersionCheckHandler pool metaVersionRef =
  runExceptT
    ( PG.runTx pool (PG.RepeatableRead, Nothing)
        $ fetchMetadataResourceVersionFromCatalog
    )
    >>= \case
      Right version -> Right <$> forcePut metaVersionRef version
      Left err -> pure $ Left err

data ErrorState = ErrorState
  { _esLastErrorSeen :: !(Maybe QErr),
    _esLastMetadataVersion :: !(Maybe MetadataResourceVersion)
  }
  deriving (Eq)

-- NOTE: The ErrorState type is to be used mainly for the `listener` method below.
--       This will help prevent logging the same error with the same MetadataResourceVersion
--       multiple times consecutively. When the `listener` is in ErrorState we don't log the
--       next error until the resource version has changed/updated.

defaultErrorState :: ErrorState
defaultErrorState = ErrorState Nothing Nothing

-- | NOTE: this can be updated to use lenses
updateErrorInState :: ErrorState -> QErr -> MetadataResourceVersion -> ErrorState
updateErrorInState es qerr mrv =
  es
    { _esLastErrorSeen = Just qerr,
      _esLastMetadataVersion = Just mrv
    }

isInErrorState :: ErrorState -> Bool
isInErrorState es =
  (isJust . _esLastErrorSeen) es && (isJust . _esLastMetadataVersion) es

toLogError :: ErrorState -> QErr -> MetadataResourceVersion -> Bool
toLogError es qerr mrv = not $ isQErrLastSeen || isMetadataResourceVersionLastSeen
  where
    isQErrLastSeen = case _esLastErrorSeen es of
      Just lErrS -> lErrS == qerr
      Nothing -> False

    isMetadataResourceVersionLastSeen = case _esLastMetadataVersion es of
      Just lMRV -> lMRV == mrv
      Nothing -> False

-- | An IO action that listens to postgres for events and pushes them to a Queue, in a loop forever.
listener ::
  (MonadIO m) =>
  Logger Hasura ->
  PG.PGPool ->
  STM.TMVar MetadataResourceVersion ->
  Milliseconds ->
  m void
listener logger pool metaVersionRef interval = L.iterateM_ listenerLoop defaultErrorState
  where
    listenerLoop errorState = do
      mrv <- liftIO $ STM.atomically $ STM.tryTakeTMVar metaVersionRef
      resp <- liftIO $ schemaVersionCheckHandler pool metaVersionRef
      let metadataVersion = fromMaybe initialResourceVersion mrv
      nextErr <- case resp of
        Left respErr -> do
          if (toLogError errorState respErr metadataVersion)
            then do
              logError logger TTListener $ TEQueryError respErr
              logInfo logger TTListener $ object ["metadataResourceVersion" .= toJSON metadataVersion]
              pure $ updateErrorInState errorState respErr metadataVersion
            else do
              pure errorState
        Right _ -> do
          when (isInErrorState errorState)
            $ logInfo logger TTListener
            $ object ["message" .= ("SchemaSync Restored..." :: Text)]
          pure defaultErrorState
      liftIO $ C.sleep $ milliseconds interval
      pure nextErr

-- | An IO action that processes events from Queue, in a loop forever.
processor ::
  forall m void impl.
  ( Tracing.MonadTraceContext m,
    C.ForkableMonadIO m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    ProvidesNetwork m
  ) =>
  STM.TMVar MetadataResourceVersion ->
  AppStateRef impl ->
  STM.TVar Bool ->
  m void
processor
  metaVersionRef
  appStateRef
  logTVar = forever do
    metaVersion <- liftIO $ STM.atomically $ STM.takeTMVar metaVersionRef
    refreshSchemaCache metaVersion appStateRef TTProcessor logTVar

refreshSchemaCache ::
  ( Tracing.MonadTraceContext m,
    MonadIO m,
    MonadBaseControl IO m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    ProvidesNetwork m
  ) =>
  MetadataResourceVersion ->
  AppStateRef impl ->
  SchemaSyncThreadType ->
  STM.TVar Bool ->
  m ()
refreshSchemaCache
  resourceVersion
  appStateRef
  threadType
  logTVar = do
    AppEnv {..} <- askAppEnv
    let logger = _lsLogger appEnvLoggers
    respErr <- runExceptT
      $ withSchemaCacheUpdate appStateRef logger (Just logTVar)
      $ do
        rebuildableCache <- liftIO $ getRebuildableSchemaCacheWithVersion appStateRef
        appContext <- liftIO $ getAppContext appStateRef
        let dynamicConfig = buildCacheDynamicConfig appContext
        -- the instance which triggered the schema sync event would have stored
        -- the source introspection, hence we can ignore it here
        (msg, cache, _, _sourcesIntrospection, _schemaRegistryAction) <-
          runCacheRWT dynamicConfig rebuildableCache $ do
            schemaCache <- askSchemaCache
            let engineResourceVersion = scMetadataResourceVersion schemaCache
            unless (engineResourceVersion == resourceVersion) $ do
              logInfoTracing logger threadType
                $ String
                $ T.unwords
                  [ "Received metadata resource version:",
                    showMetadataResourceVersion resourceVersion <> ",",
                    "different from the current engine resource version:",
                    showMetadataResourceVersion engineResourceVersion <> ".",
                    "Trying to update the schema cache."
                  ]

              MetadataWithResourceVersion metadata latestResourceVersion <- liftEitherM fetchMetadata

              logInfoTracing logger threadType
                $ String
                $ T.unwords
                  [ "Fetched metadata with resource version:",
                    showMetadataResourceVersion latestResourceVersion
                  ]

              notifications <- liftEitherM $ fetchMetadataNotifications engineResourceVersion appEnvInstanceId

              case notifications of
                [] -> do
                  logInfoTracing logger threadType
                    $ String
                    $ T.unwords
                      [ "Fetched metadata notifications and received no notifications. Not updating the schema cache.",
                        "Only setting resource version:",
                        showMetadataResourceVersion latestResourceVersion,
                        "in schema cache"
                      ]
                  setMetadataResourceVersionInSchemaCache latestResourceVersion
                _ -> do
                  logInfoTracing logger threadType
                    $ String "Fetched metadata notifications and received some notifications. Updating the schema cache."
                  let cacheInvalidations =
                        if any ((== (engineResourceVersion + 1)) . fst) notifications
                          then -- If (engineResourceVersion + 1) is in the list of notifications then
                          -- we know that we haven't missed any.
                            mconcat $ snd <$> notifications
                          else -- Otherwise we may have missed some notifications so we need to invalidate the
                          -- whole cache.

                            CacheInvalidations
                              { ciMetadata = True,
                                ciRemoteSchemas = HS.fromList $ getAllRemoteSchemas schemaCache,
                                ciSources = HS.fromList $ HashMap.keys $ scSources schemaCache,
                                ciDataConnectors =
                                  maybe mempty (HS.fromList . HashMap.keys . unBackendInfoWrapper)
                                    $ BackendMap.lookup @'DataConnector
                                    $ scBackendCache schemaCache
                              }
                  buildSchemaCacheWithOptions CatalogSync cacheInvalidations metadata (Just latestResourceVersion)
                  setMetadataResourceVersionInSchemaCache latestResourceVersion
                  logInfoTracing logger threadType
                    $ String
                    $ "Schema cache updated with resource version: "
                    <> showMetadataResourceVersion latestResourceVersion
        pure (msg, cache)
    onLeft respErr (logErrorTracing logger threadType . TEQueryError)

logInfo :: (MonadIO m) => Logger Hasura -> SchemaSyncThreadType -> Value -> m ()
logInfo logger threadType val =
  unLogger logger
    $ SchemaSyncLog LevelInfo threadType val

logInfoTracing :: (Tracing.MonadTraceContext m, MonadIO m) => Logger Hasura -> SchemaSyncThreadType -> Value -> m ()
logInfoTracing logger threadType val =
  unLoggerTracing logger
    $ SchemaSyncLog LevelInfo threadType val

logError :: (MonadIO m, ToJSON a) => Logger Hasura -> SchemaSyncThreadType -> a -> m ()
logError logger threadType err =
  unLogger logger
    $ SchemaSyncLog LevelError threadType
    $ object ["error" .= toJSON err]

logErrorTracing :: (Tracing.MonadTraceContext m, MonadIO m, ToJSON a) => Logger Hasura -> SchemaSyncThreadType -> a -> m ()
logErrorTracing logger threadType err =
  unLoggerTracing logger
    $ SchemaSyncLog LevelError threadType
    $ object ["error" .= toJSON err]
