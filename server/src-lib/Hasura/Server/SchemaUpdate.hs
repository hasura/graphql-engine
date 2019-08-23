module Hasura.Server.SchemaUpdate
  (startSchemaSync)
where

import           Hasura.Prelude

import           Hasura.Logging
import           Hasura.RQL.DDL.Schema.Table (buildSCWithoutSetup)
import           Hasura.RQL.Types
import           Hasura.Server.App           (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Init          (InstanceId (..))
import           Hasura.Server.Logging
import           Hasura.Server.Query

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Control.Concurrent          as C
import qualified Control.Concurrent.STM      as STM
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

instance ToEngineLog SchemaSyncThreadLog where
  toEngineLog threadLog =
    (suelLogLevel threadLog, ELTSchemaSyncThread, toJSON threadLog)

data EventPayload
  = EventPayload
  { _epInstanceId :: !InstanceId
  , _epOccurredAt :: !UTC.UTCTime
  } deriving (Show, Eq)
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
startSchemaSync
  :: SQLGenCtx
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime -> IO ()
startSchemaSync sqlGenCtx pool logger httpMgr cacheRef instanceId cacheInitTime = do
  -- only the latest event is recorded here
  -- we don't want to store and process all the events, only the latest event
  updateEventRef <- STM.newTVarIO Nothing

  -- Start listener thread
  lTId <- C.forkIO $ listener sqlGenCtx pool
    logger httpMgr updateEventRef cacheRef instanceId cacheInitTime
  logThreadStarted TTListener lTId

  -- Start processor thread
  pTId <- C.forkIO $ processor sqlGenCtx pool
    logger httpMgr updateEventRef cacheRef instanceId
  logThreadStarted TTProcessor pTId

  where
    logThreadStarted threadType threadId =
      let msg = T.pack (show threadType) <> " thread started"
      in unLogger logger $
         StartupLog LevelInfo "schema-sync" $
           object [ "instance_id" .= getInstanceId instanceId
                  , "thread_id" .= show threadId
                  , "message" .= msg
                  ]

-- | An IO action that listens to postgres for events and pushes them to a Queue
listener
  :: SQLGenCtx
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> STM.TVar (Maybe EventPayload)
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime -> IO ()
listener sqlGenCtx pool logger httpMgr updateEventRef
  cacheRef instanceId cacheInitTime =
  -- Never exits
  forever $ do
    listenResE <-
      liftIO $ runExceptT $ PG.listen pool pgChannel notifyHandler
    either onError return listenResE
    logWarn
    C.threadDelay $ 1 * 1000 * 1000 -- 1 second
  where
    threadType = TTListener

    shouldRefresh dbInstId accrdAt =
      case cacheInitTime of
        Nothing   -> True
        Just time -> (dbInstId /= instanceId) && accrdAt > time

    refreshCache Nothing = return ()
    refreshCache (Just (dbInstId, accrdAt)) =
      when (shouldRefresh dbInstId accrdAt) $
        refreshSchemaCache sqlGenCtx pool logger httpMgr cacheRef
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
    logWarn = unLogger logger $
      SchemaSyncThreadLog LevelWarn TTListener $ String
        "error occurred, retrying postgres listen after 1 second"


-- | An IO action that processes events from Queue
processor
  :: SQLGenCtx
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> STM.TVar (Maybe EventPayload)
  -> SchemaCacheRef
  -> InstanceId -> IO ()
processor sqlGenCtx pool logger httpMgr updateEventRef
  cacheRef instanceId =
  -- Never exits
  forever $ do
    event <- STM.atomically getLatestEvent
    logInfo logger threadType $ object ["processed_event" .= event]
    when (shouldReload event) $
      refreshSchemaCache sqlGenCtx pool logger httpMgr cacheRef
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
  -> Logger
  -> HTTP.Manager
  -> SchemaCacheRef
  -> ThreadType
  -> T.Text -> IO ()
refreshSchemaCache sqlGenCtx pool logger httpManager cacheRef threadType msg = do
  -- Reload schema cache from catalog
  resE <- liftIO $ runExceptT $ withSCUpdate cacheRef logger $
           peelRun emptySchemaCache adminUserInfo
           httpManager sqlGenCtx (PGExecCtx pool PG.Serializable) buildSCWithoutSetup
  case resE of
    Left e -> logError logger threadType $ TEQueryError e
    Right _ ->
      logInfo logger threadType $ object ["message" .= msg]

logInfo :: Logger -> ThreadType -> Value -> IO ()
logInfo logger threadType val = unLogger logger $
  SchemaSyncThreadLog LevelInfo threadType val

logError :: ToJSON a => Logger -> ThreadType -> a -> IO ()
logError logger threadType err =
  unLogger logger $ SchemaSyncThreadLog LevelError threadType $
    object ["error" .= toJSON err]
