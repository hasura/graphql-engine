{-# LANGUAGE CPP #-}
module Hasura.Server.SchemaUpdate
  ( startSchemaSyncListenerThread
  , startSchemaSyncProcessorThread
  , ThreadType(..)
  , ThreadError(..)
  )
where

import           Hasura.Logging
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema         (runCacheRWT)
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.App             (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Logging
import           Hasura.Server.Types           (InstanceId (..))
import           Hasura.Session

import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Control.Monad.Trans.Managed   (ManagedT)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.IORef

import qualified Control.Concurrent.Extended   as C
import qualified Control.Concurrent.STM        as STM
import qualified Control.Immortal              as Immortal
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Database.PG.Query             as Q
import qualified Network.HTTP.Client           as HTTP

data ThreadType
  = TTListener
  | TTProcessor
  deriving (Eq)

instance Show ThreadType where
  show TTListener  = "listener"
  show TTProcessor = "processor"

data SchemaSyncThreadLog
  = SchemaSyncThreadLog
  { suelLogLevel   :: !LogLevel
  , suelThreadType :: !ThreadType
  , suelInfo       :: !Value
  } deriving (Show, Eq)

instance ToJSON SchemaSyncThreadLog where
  toJSON (SchemaSyncThreadLog _ t info) =
    object [ "thread_type" .= show t
           , "info" .= info
           ]

instance ToEngineLog SchemaSyncThreadLog Hasura where
  toEngineLog threadLog =
    (suelLogLevel threadLog, ELTInternal ILTSchemaSyncThread, toJSON threadLog)

data ThreadError
  = TEPayloadParse !Text
  | TEQueryError !QErr
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
 ''ThreadError)

logThreadStarted
  :: (MonadIO m)
  => Logger Hasura -> InstanceId -> ThreadType -> Immortal.Thread -> m ()
logThreadStarted logger instanceId threadType thread =
  let msg = tshow threadType <> " thread started"
  in unLogger logger $
     StartupLog LevelInfo "schema-sync" $
       object [ "instance_id" .= getInstanceId instanceId
              , "thread_id" .= show (Immortal.threadId thread)
              , "message" .= msg
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
startSchemaSyncListenerThread
  :: C.ForkableMonadIO m
  => Logger Hasura
  -> Q.PGPool
  -> InstanceId
  -> Milliseconds
  -> STM.TMVar MetadataResourceVersion
  -> ManagedT m (Immortal.Thread)
startSchemaSyncListenerThread logger pool instanceId interval metaVersionRef = do
  -- Start listener thread
  listenerThread <- C.forkManagedT "SchemeUpdate.listener" logger $ listener logger pool metaVersionRef interval
  logThreadStarted logger instanceId TTListener listenerThread
  pure listenerThread

-- | An async thread which processes the schema sync events
-- See Note [Schema Cache Sync]
startSchemaSyncProcessorThread
  :: ( C.ForkableMonadIO m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => Logger Hasura
  -> HTTP.Manager
  -> STM.TMVar MetadataResourceVersion
  -> SchemaCacheRef
  -> InstanceId
  -> ServerConfigCtx
  -> ManagedT m Immortal.Thread
startSchemaSyncProcessorThread logger httpMgr
  schemaSyncEventRef cacheRef instanceId serverConfigCtx = do
  -- Start processor thread
  processorThread <- C.forkManagedT "SchemeUpdate.processor" logger $
    processor logger httpMgr schemaSyncEventRef
              cacheRef instanceId serverConfigCtx
  logThreadStarted logger instanceId TTProcessor processorThread
  pure processorThread

-- TODO: This is also defined in multitenant, consider putting it in a library somewhere
forcePut :: STM.TMVar a -> a -> IO ()
forcePut v a = STM.atomically $ STM.tryTakeTMVar v >> STM.putTMVar v a

schemaVersionCheckHandler
  :: Q.PGPool -> STM.TMVar MetadataResourceVersion -> IO (Either QErr ())
schemaVersionCheckHandler pool metaVersionRef =
   (runExceptT $
     Q.runTx pool (Q.RepeatableRead, Nothing) $
       fetchMetadataResourceVersionFromCatalog) >>= \case
    Right version ->
      Right <$> forcePut metaVersionRef version
    Left err -> pure $ Left err

-- | An IO action that listens to postgres for events and pushes them to a Queue, in a loop forever.
listener
  :: MonadIO m
  => Logger Hasura
  -> Q.PGPool
  -> STM.TMVar MetadataResourceVersion
  -> Milliseconds
  -> m void
listener logger pool metaVersionRef interval =
  forever $ do
    respErr <- liftIO $ schemaVersionCheckHandler pool metaVersionRef
    liftIO $ do
      onLeft respErr (logError logger TTListener . TEQueryError)
      C.sleep (milliseconds interval)

-- | An IO action that processes events from Queue, in a loop forever.
processor
  :: forall m void.
     ( C.ForkableMonadIO m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => Logger Hasura
  -> HTTP.Manager
  -> STM.TMVar MetadataResourceVersion
  -> SchemaCacheRef
  -> InstanceId
  -> ServerConfigCtx
  -> m void
processor logger httpMgr metaVersionRef
  cacheRef instanceId serverConfigCtx = forever $ do
  metaVersion <- liftIO $ STM.atomically $ STM.takeTMVar metaVersionRef

  respErr <- runMetadataStorageT $
    refreshSchemaCache metaVersion instanceId logger httpMgr cacheRef TTProcessor serverConfigCtx
  onLeft respErr (logError logger TTProcessor . TEQueryError)

refreshSchemaCache
  :: ( MonadIO m
    , MonadBaseControl IO m
    , MonadMetadataStorage m
    , MonadResolveSource m
    )
  => MetadataResourceVersion
  -> InstanceId
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaCacheRef
  -> ThreadType
  -> ServerConfigCtx
  -> m ()
refreshSchemaCache resourceVersion instanceId logger httpManager
    cacheRef threadType serverConfigCtx = do
      respErr <- runExceptT $ withSCUpdate cacheRef logger $ do
        rebuildableCache <- fst <$> liftIO (readIORef $ _scrCache cacheRef)
        (msg, cache, _) <- peelRun runCtx $ runCacheRWT rebuildableCache $ do
          schemaCache <- askSchemaCache
          let engineResourceVersion = fromMaybe 0 $ scMetadataResourceVersion schemaCache -- TODO: Can we remove the maybe from scMetadataResourceVersion?
          unless (engineResourceVersion == resourceVersion) $ do
            (metadata, latestResourceVersion) <- fetchMetadata
            notifications <- fetchMetadataNotifications engineResourceVersion instanceId

            logDebug logger threadType $ "DEBUG: refreshSchemaCache Called: engineResourceVersion: " <> show engineResourceVersion <> ", fresh resource version: " <> show latestResourceVersion
            case notifications of
              [] -> do
                logInfo logger threadType $ object ["message" .= ("Schema Version changed with no notifications" :: Text)]
                setMetadataResourceVersionInSchemaCache latestResourceVersion
              _ -> do
                let cacheInvalidations =
                      if any ((== (engineResourceVersion + 1)) . fst) notifications
                        then -- If (engineResourceVersion + 1) is in the list of notifications then
                             -- we know that we haven't missed any.
                             mconcat $ snd <$> notifications
                        else -- Otherwise we may have missed some notifications so we need to invalidate the
                             -- whole cache.
                             CacheInvalidations
                               { ciMetadata = True
                               , ciRemoteSchemas = HS.fromList $ getAllRemoteSchemas schemaCache
                               , ciSources = HS.fromList $ HM.keys $ scSources schemaCache
                               }
                logInfo logger threadType $ object ["currentVersion" .= engineResourceVersion, "latestResourceVersion" .= latestResourceVersion]
                buildSchemaCacheWithOptions CatalogSync cacheInvalidations metadata
                setMetadataResourceVersionInSchemaCache latestResourceVersion
                logInfo logger threadType $ object ["message" .= ("Schema Version changed with notifications" :: Text)]
        pure (msg, cache)
      onLeft respErr (logError logger threadType . TEQueryError)
 where
   runCtx = RunCtx adminUserInfo httpManager serverConfigCtx

logInfo :: (MonadIO m) => Logger Hasura -> ThreadType -> Value -> m ()
logInfo logger threadType val = unLogger logger $
  SchemaSyncThreadLog LevelInfo threadType val

logError :: (MonadIO m, ToJSON a) => Logger Hasura -> ThreadType -> a -> m ()
logError logger threadType err =
  unLogger logger $ SchemaSyncThreadLog LevelError threadType $
    object ["error" .= toJSON err]

logDebug :: (MonadIO m) => Logger Hasura -> ThreadType -> String -> m ()
logDebug logger threadType msg = unLogger logger $
  SchemaSyncThreadLog LevelDebug threadType $ object ["message" .= msg]
