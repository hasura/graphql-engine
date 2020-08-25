{-# LANGUAGE CPP #-}
module Hasura.Server.SchemaUpdate
  ( startSchemaSyncListenerThread
  , startSchemaSyncProcessorThread
  )
where

import           Hasura.Db
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema       (runCacheRWT)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.App           (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Init          (InstanceId (..))
import           Hasura.Server.Logging
import           Hasura.Session

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

data ThreadError
  = TEJsonParse !T.Text
  | TEQueryError !QErr
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
 ''ThreadError)

type SchemaSyncEventRef = STM.TVar (Maybe EventPayload)

logThreadStarted
  :: (MonadIO m)
  => Logger Hasura -> InstanceId -> ThreadType -> Immortal.Thread -> m ()
logThreadStarted logger instanceId threadType thread =
  let msg = T.pack (show threadType) <> " thread started"
  in unLogger logger $
     StartupLog LevelInfo "schema-sync" $
       object [ "instance_id" .= getInstanceId instanceId
              , "thread_id" .= show (Immortal.threadId thread)
              , "message" .= msg
              ]

-- | An async thread which listen to Postgres notify to enable schema syncing
startSchemaSyncListenerThread
  :: (MonadIO m)
  => PG.PGPool
  -> Logger Hasura
  -> InstanceId
  -> m (Immortal.Thread, SchemaSyncEventRef)
startSchemaSyncListenerThread pool logger instanceId = do
  -- only the latest event is recorded here
  -- we don't want to store and process all the events, only the latest event
  schemaSyncEventRef <- liftIO $ STM.newTVarIO Nothing

  -- Start listener thread
  listenerThread <- liftIO $ C.forkImmortal "SchemeUpdate.listener" logger $
                      listener pool logger schemaSyncEventRef
  logThreadStarted logger instanceId TTListener listenerThread
  pure (listenerThread, schemaSyncEventRef)

-- | An async thread which processes the schema sync events
startSchemaSyncProcessorThread
  :: (MonadIO m)
  => SQLGenCtx
  -> PG.PGPool
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaSyncEventRef
  -> SchemaCacheRef
  -> InstanceId
  -> m Immortal.Thread
startSchemaSyncProcessorThread sqlGenCtx pool logger httpMgr
  schemaSyncEventRef cacheRef instanceId = do
  -- Start processor thread
  processorThread <- liftIO $ C.forkImmortal "SchemeUpdate.processor" logger $
                     processor sqlGenCtx pool logger httpMgr schemaSyncEventRef cacheRef instanceId
  logThreadStarted logger instanceId TTProcessor processorThread
  pure processorThread

-- | An IO action that listens to postgres for events and pushes them to a Queue, in a loop forever.
listener
  :: PG.PGPool -> Logger Hasura -> STM.TVar (Maybe EventPayload) -> IO void
listener pool logger schemaSyncEventRef =
  -- Never exits
  forever $ do
    listenResE <-
      liftIO $ runExceptT $ PG.listen pool pgChannel notifyHandler
    either onError return listenResE
    logWarn
    C.sleep $ seconds 1
  where
    threadType = TTListener

    notifyHandler = \case
      PG.PNEOnStart        -> pure ()
      PG.PNEPQNotify notif ->
        case eitherDecodeStrict $ PQ.notifyExtra notif of
          Left e -> logError logger threadType $ TEJsonParse $ T.pack e
          Right payload -> do
            logInfo logger threadType $ object ["received_event" .= payload]
#ifndef PROFILING
            $assertNFHere payload  -- so we don't write thunks to mutable vars
#endif
            -- Push a notify event to Queue
            STM.atomically $ STM.writeTVar schemaSyncEventRef $ Just payload

    onError = logError logger threadType . TEQueryError
    -- NOTE: we handle expected error conditions here, while unexpected exceptions will result in
    -- a restart and log from 'forkImmortal'
    logWarn = unLogger logger $
      SchemaSyncThreadLog LevelWarn TTListener $ String
        "error occurred, retrying postgres listen after 1 second"


-- | An IO action that processes events from Queue, in a loop forever.
processor
  :: SQLGenCtx
  -> PG.PGPool
  -> Logger Hasura
  -> HTTP.Manager
  -> STM.TVar (Maybe EventPayload)
  -> SchemaCacheRef
  -> InstanceId
  -> IO void
processor sqlGenCtx pool logger httpMgr schemaSyncEventRef
  cacheRef instanceId =
  -- Never exits
  forever $ do
    event <- STM.atomically getLatestEvent
    logInfo logger threadType $ object ["processed_event" .= event]
    when (shouldReload event) $
      refreshSchemaCache sqlGenCtx pool logger httpMgr cacheRef (_epInvalidations event)
        threadType "schema cache reloaded"
  where
    -- checks if there is an event
    -- and replaces it with Nothing
    getLatestEvent = do
      eventM <- STM.readTVar schemaSyncEventRef
      case eventM of
        Just event -> do
          STM.writeTVar schemaSyncEventRef Nothing
          return event
        Nothing -> STM.retry
    threadType = TTProcessor

      -- If event is from another server instance
    shouldReload payload = _epInstanceId payload /= instanceId

refreshSchemaCache
  :: SQLGenCtx
  -> PG.PGPool
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaCacheRef
  -> CacheInvalidations
  -> ThreadType
  -> T.Text -> IO ()
refreshSchemaCache sqlGenCtx pool logger httpManager cacheRef invalidations threadType msg = do
  -- Reload schema cache from catalog
  resE <- liftIO $ runExceptT $ withSCUpdate cacheRef logger do
    rebuildableCache <- fst <$> liftIO (readIORef $ _scrCache cacheRef)
    ((), cache, _) <- buildSchemaCacheWithOptions CatalogSync invalidations noMetadataModify
      & runCacheRWT rebuildableCache
      & peelRun runCtx pgCtx PG.ReadWrite Nothing
    pure ((), cache)
  case resE of
    Left e   -> logError logger threadType $ TEQueryError e
    Right () -> logInfo logger threadType $ object ["message" .= msg]
 where
  runCtx = RunCtx adminUserInfo httpManager sqlGenCtx
  pgCtx = mkPGExecCtx PG.Serializable pool

logInfo :: Logger Hasura -> ThreadType -> Value -> IO ()
logInfo logger threadType val = unLogger logger $
  SchemaSyncThreadLog LevelInfo threadType val

logError :: ToJSON a => Logger Hasura -> ThreadType -> a -> IO ()
logError logger threadType err =
  unLogger logger $ SchemaSyncThreadLog LevelError threadType $
    object ["error" .= toJSON err]
