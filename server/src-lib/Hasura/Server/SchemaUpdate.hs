module Hasura.Server.SchemaUpdate
  (startSchemaSync)
where

import           Hasura.Prelude

import           Hasura.Logging
import           Hasura.RQL.DDL.Schema.Table (buildSchemaCache)
import           Hasura.RQL.Types
import           Hasura.Server.App           (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Init          (InstanceId (..))
import           Hasura.Server.Logging
import           Hasura.Server.Query

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Control.Concurrent          as C
import qualified Control.Concurrent.Async    as A
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


data SchemaUpdateEventLog
  = SchemaUpdateEventLog
  { suelLogLevel   :: !LogLevel
  , suelThreadType :: !ThreadType
  , suelInfo       :: !Value
  } deriving (Show, Eq)

instance ToJSON SchemaUpdateEventLog where
  toJSON (SchemaUpdateEventLog _ t info) =
    object [ "thread_type" .= show t
           , "info" .= info
           ]

instance ToEngineLog SchemaUpdateEventLog where
  toEngineLog threadLog =
    (suelLogLevel threadLog, "schema_update_event", toJSON threadLog)

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
  :: Bool
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime -> IO ()
startSchemaSync strfyNum pool logger httpMgr cacheRef instanceId cacheInitTime = do
  -- Init events queue
  eventsQueue <- STM.newTQueueIO
  -- Start listener thread
  lAsync <- A.async $ listener strfyNum pool
    logger httpMgr eventsQueue cacheRef instanceId cacheInitTime

  -- Start processor thread
  pAsync <- A.async $ processor strfyNum pool
    logger httpMgr eventsQueue cacheRef instanceId

  void $ A.waitAny [lAsync, pAsync]

-- | An IO action that listens to postgres for events and pushes them to a Queue
listener
  :: Bool
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> STM.TQueue EventPayload
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime -> IO ()
listener strfyNum pool logger httpMgr eventsQueue
  cacheRef instanceId cacheInitTime = do
  logThreadStartup logger instanceId threadType
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
        refreshSchemaCache strfyNum pool logger httpMgr cacheRef
          threadType "reloading schema cache on listen start"

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
            STM.atomically $ STM.writeTQueue eventsQueue payload

    onError = logError logger threadType . TEQueryError
    logWarn = unLogger logger $
      SchemaUpdateEventLog LevelWarn TTListener $ String
        "error occured retrying pg listen after 1 second"


-- | An IO action that processes events from Queue
processor
  :: Bool
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> STM.TQueue EventPayload
  -> SchemaCacheRef
  -> InstanceId -> IO ()
processor strfyNum pool logger httpMgr eventsQueue
  cacheRef instanceId = do
  logThreadStartup logger instanceId threadType
  -- Never exits
  forever $ do
    event <- STM.atomically $ STM.readTQueue eventsQueue
    logInfo logger threadType $ object ["processed_event" .= event]
    when (shouldReload event) $
      refreshSchemaCache strfyNum pool logger httpMgr cacheRef
        threadType "schema cache reloaded"
  where
    threadType = TTProcessor

      -- If event is from another server
    shouldReload payload = _epInstanceId payload /= instanceId

logThreadStartup
  :: Show a
  => Logger
  -> InstanceId
  -> a -> IO ()
logThreadStartup logger instanceId threadType =
  unLogger logger threadLog
  where
    threadLog =
      let msg = T.pack (show threadType) <> " thread started"
      in StartupLog LevelInfo "threads" $
           object [ "instance_id" .= getInstanceId instanceId
                  , "message" .= msg
                  ]

refreshSchemaCache
  :: Bool
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> SchemaCacheRef
  -> ThreadType
  -> T.Text -> IO ()
refreshSchemaCache strfyNum pool logger httpManager cacheRef threadType msg = do
  -- Reload schema cache from catalog
  resE <- liftIO $ runExceptT $ withSCUpdate cacheRef $
           peelRun emptySchemaCache adminUserInfo
           httpManager strfyNum pool PG.Serializable buildSchemaCache
  case resE of
    Left e -> logError logger threadType $ TEQueryError e
    Right _ ->
      logInfo logger threadType $ object ["message" .= msg]

logInfo :: Logger -> ThreadType -> Value -> IO ()
logInfo logger threadType val = unLogger logger $
  SchemaUpdateEventLog LevelInfo threadType val

logError :: ToJSON a => Logger -> ThreadType -> a -> IO ()
logError logger threadType err =
  unLogger logger $ SchemaUpdateEventLog LevelError threadType $
    object ["error" .= toJSON err]
