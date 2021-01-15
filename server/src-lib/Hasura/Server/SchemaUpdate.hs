{-# LANGUAGE CPP #-}
module Hasura.Server.SchemaUpdate
  ( startSchemaSyncListenerThread
  , startSchemaSyncProcessorThread
  , EventPayload(..)
  , SchemaSyncCtx(..)
  )
where

import           Hasura.Logging
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema       (runCacheRWT)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.App           (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Logging
import           Hasura.Server.Types         (InstanceId (..))
import           Hasura.Session

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Managed (ManagedT)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.IORef
#ifndef PROFILING
import           GHC.AssertNF
#endif

import qualified Control.Concurrent.Extended as C
import qualified Control.Concurrent.STM      as STM
import qualified Control.Immortal            as Immortal
import qualified Data.Text                   as T
import qualified Data.Time                   as UTC
import qualified Database.PG.Query           as PG
import qualified Database.PostgreSQL.LibPQ   as PQ
import qualified Network.HTTP.Client         as HTTP

pgChannel :: PG.PGChannel
pgChannel = "hasura_schema_update"

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

data EventPayload
  = EventPayload
  { _epInstanceId    :: !InstanceId
  , _epOccurredAt    :: !UTC.UTCTime
  , _epInvalidations :: !CacheInvalidations
  }
$(deriveJSON (aesonDrop 3 snakeCase) ''EventPayload)

data SchemaSyncEvent
  = SSEListenStart !UTC.UTCTime
  | SSEPayload !Value

instance ToJSON SchemaSyncEvent where
  toJSON = \case
    SSEListenStart time -> String $ "event listening started at " <> tshow time
    SSEPayload payload -> toJSON payload

data ThreadError
  = TEPayloadParse !Text
  | TEQueryError !QErr
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
 ''ThreadError)

type SchemaSyncEventRef = STM.TVar (Maybe SchemaSyncEvent)

-- | Context required for schema syncing. Listener thread id,
-- event references and cache init time
data SchemaSyncCtx
  = SchemaSyncCtx
  { _sscListenerThreadId   :: !Immortal.Thread
  , _sscSyncEventRef       :: !SchemaSyncEventRef
  , _sscCacheInitStartTime :: !UTC.UTCTime
  }

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
We make use of Postgres listen-notify to notify metadata updates and listen to those
events. Each instance is identified by an @'InstanceId'. We choose 'hasura_schema_update'
as our notify channel. Following steps take place when an API request made to update
metadata:

1. After handling the request we publish an event via a postgres channel with payload
   containing instance id and other information to build schema cache.
   See @'notifySchemaCacheSync'.

2. On start up, before initialising schema cache, an async thread is invoked to
   continuously listen to Postgres events on 'hasura_schema_update' channel. The payload
   present in the event is decoded and pushed to a shared @'SchemaSyncEventRef' reference.
   See @'startSchemaSyncListenerThread'.

3. Before starting API server, another async thread is invoked to process events pushed
   by the listener thread via @'SchemaSyncEventRef' reference. Based on the event instance id
   or listen start time we decide to reload schema cache or not.
   See @'startSchemaSyncProcessorThread'.

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
  => PG.PGPool
  -> Logger Hasura
  -> InstanceId
  -> ManagedT m (Immortal.Thread, SchemaSyncEventRef)
startSchemaSyncListenerThread pool logger instanceId = do
  -- only the latest event is recorded here
  -- we don't want to store and process all the events, only the latest event
  schemaSyncEventRef <- liftIO $ STM.newTVarIO Nothing

  -- Start listener thread
  listenerThread <- C.forkManagedT "SchemeUpdate.listener" logger . liftIO $
                    listener pool logger schemaSyncEventRef
  logThreadStarted logger instanceId TTListener listenerThread
  pure (listenerThread, schemaSyncEventRef)

-- | An async thread which processes the schema sync events
-- See Note [Schema Cache Sync]
startSchemaSyncProcessorThread
  :: ( C.ForkableMonadIO m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => SQLGenCtx
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaSyncEventRef
  -> SchemaCacheRef
  -> InstanceId
  -> UTC.UTCTime
  -> RemoteSchemaPermsCtx
  -> ManagedT m Immortal.Thread
startSchemaSyncProcessorThread sqlGenCtx logger httpMgr
  schemaSyncEventRef cacheRef instanceId cacheInitStartTime remoteSchemaPermsCtx = do
  -- Start processor thread
  processorThread <- C.forkManagedT "SchemeUpdate.processor" logger $
    processor sqlGenCtx logger httpMgr schemaSyncEventRef cacheRef instanceId cacheInitStartTime remoteSchemaPermsCtx
  logThreadStarted logger instanceId TTProcessor processorThread
  pure processorThread

-- | An IO action that listens to postgres for events and pushes them to a Queue, in a loop forever.
listener
  :: PG.PGPool
  -> Logger Hasura
  -> SchemaSyncEventRef
  -> IO void
listener pool logger updateEventRef =
  -- Never exits
  forever $ do
    listenResE <-
      liftIO $ runExceptT $ PG.listen pool pgChannel notifyHandler
    onLeft listenResE onError
    logWarn
    -- Trying to start listening after one second.
    -- See Note [Schema Cache Sync].
    C.sleep $ seconds 1
  where
    threadType = TTListener

    notifyHandler = \case
      PG.PNEOnStart        -> do
        time <- UTC.getCurrentTime
        STM.atomically $ STM.writeTVar updateEventRef $ Just $ SSEListenStart time
      PG.PNEPQNotify notif ->
        case eitherDecodeStrict $ PQ.notifyExtra notif of
          Left e -> logError logger threadType $ TEPayloadParse $ T.pack e
          Right payload -> do
            logInfo logger threadType $ object ["received_event" .= payload]
#ifndef PROFILING
            $assertNFHere payload  -- so we don't write thunks to mutable vars
#endif
            -- Push a notify event to Queue
            STM.atomically $ STM.writeTVar updateEventRef $ Just $ SSEPayload payload

    onError = logError logger threadType . TEQueryError
    -- NOTE: we handle expected error conditions here, while unexpected exceptions will result in
    -- a restart and log from 'forkImmortal'
    logWarn = unLogger logger $
      SchemaSyncThreadLog LevelWarn TTListener $ String
        "error occurred, retrying postgres listen after 1 second"


-- | An IO action that processes events from Queue, in a loop forever.
processor
  :: forall m void.
     ( C.ForkableMonadIO m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => SQLGenCtx
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaSyncEventRef
  -> SchemaCacheRef
  -> InstanceId
  -> UTC.UTCTime
  -> RemoteSchemaPermsCtx
  -> m void
processor sqlGenCtx logger httpMgr updateEventRef
  cacheRef instanceId cacheInitStartTime remoteSchemaPermsCtx =
  -- Never exits
  forever $ do
    event <- liftIO $ STM.atomically getLatestEvent
    logInfo logger threadType $ object ["processed_event" .= event]
    (shouldReload, cacheInvalidations) <- case event of
      SSEListenStart time ->
        -- If listening started after cache initialization, just refresh the schema cache unconditionally.
        -- See Note [Schema Cache Sync]
        pure $ (time > cacheInitStartTime, mempty)
      SSEPayload payload -> do
        eitherResult <- runMetadataStorageT $ processSchemaSyncEventPayload instanceId payload
        case eitherResult of
          Left e -> do
            logError logger threadType $ TEPayloadParse $ qeError e
            pure (False, mempty)
          Right SchemaSyncEventProcessResult{..} ->
            pure (_sseprShouldReload, _sseprCacheInvalidations)

    when shouldReload $
      refreshSchemaCache sqlGenCtx logger httpMgr cacheRef cacheInvalidations
        threadType remoteSchemaPermsCtx "schema cache reloaded"
  where
    -- checks if there is an event
    -- and replaces it with Nothing
    getLatestEvent = do
      eventM <- STM.readTVar updateEventRef
      case eventM of
        Just event -> do
          STM.writeTVar updateEventRef Nothing
          return event
        Nothing -> STM.retry
    threadType = TTProcessor

refreshSchemaCache
  :: ( MonadIO m
     , MonadBaseControl IO m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => SQLGenCtx
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaCacheRef
  -> CacheInvalidations
  -> ThreadType
  -> RemoteSchemaPermsCtx
  -> Text
  -> m ()
refreshSchemaCache sqlGenCtx logger httpManager
    cacheRef invalidations threadType remoteSchemaPermsCtx msg = do
  -- Reload schema cache from catalog
  eitherMetadata <- runMetadataStorageT fetchMetadata
  resE <- runExceptT $ do
    metadata <- liftEither eitherMetadata
    withSCUpdate cacheRef logger do
      rebuildableCache <- fst <$> liftIO (readIORef $ _scrCache cacheRef)
      ((), cache, _) <- buildSchemaCacheWithOptions CatalogSync invalidations metadata
        & runCacheRWT rebuildableCache
        & peelRun runCtx
      pure ((), cache)
  case resE of
    Left e   -> logError logger threadType $ TEQueryError e
    Right () -> logInfo logger threadType $ object ["message" .= msg]
 where
  runCtx = RunCtx adminUserInfo httpManager sqlGenCtx remoteSchemaPermsCtx

logInfo :: (MonadIO m) => Logger Hasura -> ThreadType -> Value -> m ()
logInfo logger threadType val = unLogger logger $
  SchemaSyncThreadLog LevelInfo threadType val

logError :: (MonadIO m, ToJSON a) => Logger Hasura -> ThreadType -> a -> m ()
logError logger threadType err =
  unLogger logger $ SchemaSyncThreadLog LevelError threadType $
    object ["error" .= toJSON err]
