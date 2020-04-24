module Hasura.Server.SchemaUpdate
  (startSchemaSyncThreads)
where

import           Hasura.Prelude

import           Hasura.Logging
import           Hasura.RQL.DDL.Schema       (runCacheRWT)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.API.Query
import           Hasura.Server.App           (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Init          (InstanceId (..))
import           Hasura.Server.Logging

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.IORef

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

-- | An IO action that enables metadata syncing
startSchemaSyncThreads
  :: (MonadIO m)
  => SQLGenCtx
  -> PG.PGPool
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime
  -> m (Immortal.Thread, Immortal.Thread)
  -- ^ Returns: (listener handle, processor handle)
startSchemaSyncThreads sqlGenCtx pool logger httpMgr cacheRef instanceId cacheInitTime = do
  -- only the latest event is recorded here
  -- we don't want to store and process all the events, only the latest event
  updateEventRef <- liftIO $ STM.newTVarIO Nothing

  -- Start listener thread
  lTId <- liftIO $ C.forkImmortal "SchemeUpdate.listener" logger $
    listener sqlGenCtx pool logger httpMgr updateEventRef cacheRef instanceId cacheInitTime
  logThreadStarted TTListener lTId

  -- Start processor thread
  pTId <- liftIO $ C.forkImmortal "SchemeUpdate.processor" logger $
    processor sqlGenCtx pool logger httpMgr updateEventRef cacheRef instanceId
  logThreadStarted TTProcessor pTId

  return (lTId, pTId)

  where
    logThreadStarted threadType thread =
      let msg = T.pack (show threadType) <> " thread started"
      in unLogger logger $
         StartupLog LevelInfo "schema-sync" $
           object [ "instance_id" .= getInstanceId instanceId
                  , "thread_id" .= show (Immortal.threadId thread)
                  , "message" .= msg
                  ]

-- | An IO action that listens to postgres for events and pushes them to a Queue, in a loop forever.
listener
  :: SQLGenCtx
  -> PG.PGPool
  -> Logger Hasura
  -> HTTP.Manager
  -> STM.TVar (Maybe EventPayload)
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime -> IO void
listener sqlGenCtx pool logger httpMgr updateEventRef
  cacheRef instanceId cacheInitTime =
  -- Never exits
  forever $ do
    listenResE <-
      liftIO $ runExceptT $ PG.listen pool pgChannel notifyHandler
    either onError return listenResE
    logWarn
    C.sleep $ seconds 1
  where
    threadType = TTListener

    shouldRefresh dbInstId accrdAt =
      case cacheInitTime of
        Nothing   -> True
        Just time -> (dbInstId /= instanceId) && accrdAt > time

    refreshCache Nothing = return ()
    refreshCache (Just (dbInstId, accrdAt, invalidations)) =
      when (shouldRefresh dbInstId accrdAt) $
        refreshSchemaCache sqlGenCtx pool logger httpMgr cacheRef invalidations
          threadType "schema cache reloaded after postgres listen init"

    notifyHandler = \case
      PG.PNEOnStart -> do
        eRes <- runExceptT $ PG.runTx pool
          (PG.Serializable, Nothing) fetchLastUpdate
        case eRes of
          Left e         -> onError e
          Right mLastUpd -> refreshCache mLastUpd

      PG.PNEPQNotify notif ->
        case eitherDecodeStrict $ PQ.notifyExtra notif of
          Left e -> logError logger threadType $ TEJsonParse $ T.pack e
          Right payload -> do
            logInfo logger threadType $ object ["received_event" .= payload]
            -- Push a notify event to Queue
            STM.atomically $ STM.writeTVar updateEventRef $ Just payload

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
  -> InstanceId -> IO void
processor sqlGenCtx pool logger httpMgr updateEventRef
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
      eventM <- STM.readTVar updateEventRef
      case eventM of
        Just event -> do
          STM.writeTVar updateEventRef Nothing
          return event
        Nothing -> STM.retry
    threadType = TTProcessor

      -- If event is from another server
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
    ((), cache, _) <- buildSchemaCacheWithOptions CatalogSync invalidations
      & runCacheRWT rebuildableCache
      & peelRun runCtx pgCtx PG.ReadWrite
    pure ((), cache)
  case resE of
    Left e   -> logError logger threadType $ TEQueryError e
    Right () -> logInfo logger threadType $ object ["message" .= msg]
 where
  runCtx = RunCtx adminUserInfo httpManager sqlGenCtx
  pgCtx = PGExecCtx pool PG.Serializable

logInfo :: Logger Hasura -> ThreadType -> Value -> IO ()
logInfo logger threadType val = unLogger logger $
  SchemaSyncThreadLog LevelInfo threadType val

logError :: ToJSON a => Logger Hasura -> ThreadType -> a -> IO ()
logError logger threadType err =
  unLogger logger $ SchemaSyncThreadLog LevelError threadType $
    object ["error" .= toJSON err]
