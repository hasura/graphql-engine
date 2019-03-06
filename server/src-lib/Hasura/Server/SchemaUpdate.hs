module Hasura.Server.SchemaUpdate
  ( ThreadType(..)
  , schemaUpdateEventListener
  , schemaUpdateEventProcessor
  )
where

import           Hasura.Prelude

import           Hasura.Logging
import           Hasura.RQL.DDL.Schema.Table (buildSchemaCache)
import           Hasura.RQL.Types
import           Hasura.Server.App           (SchemaCacheRef (..), withSCUpdate)
import           Hasura.Server.Init          (InstanceId (..))
import           Hasura.Server.Query

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.UUID

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
  { _epInstanceId :: !UUID
  , _epOccurredAt :: !UTC.UTCTime
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''EventPayload)

data SchemaUpdateEvent
  = SUESuccess !EventPayload
  | SUEPGReConn
  deriving (Show, Eq)

instance ToJSON SchemaUpdateEvent where
  toJSON (SUESuccess payload) = toJSON payload
  toJSON SUEPGReConn          = String "postgres reconnection"

data ThreadError
  = TEJsonParse !T.Text
  | TEQueryError !QErr
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
 ''ThreadError)

-- | An IO action that listens to postgres for events and pushes them to a Queue
schemaUpdateEventListener
  :: PG.PGPool
  -> Logger
  -> STM.TQueue SchemaUpdateEvent
  -> IO ()
schemaUpdateEventListener pool logger eventsQueue =
  -- Never exits
  forever $ do
    listenResE <- liftIO $ runExceptT $ PG.listen pool pgChannel notifyHandler
    either onError return listenResE
  where
    notifyHandler = PG.NotifyHandler onReconn onMessage
    threadType = TTListener

    onError = logError logger threadType . TEQueryError

    onReconn = do
      -- emit postgres reconnection event
      let event = SUEPGReConn
      logInfo logger threadType $ object ["received_event" .= event]
      STM.atomically $ STM.writeTQueue eventsQueue event

    -- Postgres notification handler
    onMessage notif =
      case eitherDecodeStrict $ PQ.notifyExtra notif of
        Left e -> logError logger threadType $ TEJsonParse $ T.pack e
        Right payload -> do
          let newEvent = SUESuccess payload
          logInfo logger threadType $ object ["received_event" .= newEvent]
          -- Push a success event to Queue along with event payload
          STM.atomically $ STM.writeTQueue eventsQueue newEvent

-- | An IO action that processes events from Queue
schemaUpdateEventProcessor
  :: Bool
  -> PG.PGPool
  -> Logger
  -> HTTP.Manager
  -> STM.TQueue SchemaUpdateEvent
  -> SchemaCacheRef
  -> InstanceId
  -> Maybe UTC.UTCTime
  -> IO ()
schemaUpdateEventProcessor strfyNum pool logger httpManager
                    eventsQueue cacheRef instanceId cacheInit =
  -- Never exits
  forever $ do
    event <- STM.atomically $ STM.readTQueue eventsQueue
    logInfo logger threadType $ object ["processed_event" .= event]
    when (shouldReload event) $ do
      -- Reload schema cache from catalog
      resE <- liftIO $ runExceptT $ withSCUpdate cacheRef $
               peelRun emptySchemaCache adminUserInfo
               httpManager strfyNum pool PG.Serializable buildSchemaCache
      case resE of
        Left e -> logError logger threadType $ TEQueryError e
        Right _ ->
          logInfo logger threadType $
            object ["message" .= ("schema cache reloaded" :: T.Text)]
  where
    threadType = TTProcessor

    -- If postgres reconnect happens reload schema cache
    shouldReload SUEPGReConn = True
      -- If event is from another server and occurred after
      -- init schema cache built then reload
    shouldReload (SUESuccess payload) =
      (_epInstanceId payload /= getInstanceId instanceId)
      && withCacheInit (_epOccurredAt payload) cacheInit

    withCacheInit _ Nothing                  = False
    withCacheInit occurredAt (Just initTime) = occurredAt > initTime

logInfo :: Logger -> ThreadType -> Value -> IO ()
logInfo logger threadType val = unLogger logger $
  SchemaUpdateEventLog LevelInfo threadType val

logError :: ToJSON a => Logger -> ThreadType -> a -> IO ()
logError logger threadType err = unLogger logger $
  SchemaUpdateEventLog LevelError threadType $ object ["error" .= toJSON err]
