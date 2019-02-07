module Hasura.Server.CacheUpdate
  ( ThreadType(..)
  , cacheUpdateEventListener
  , cacheUpdateEventProcessor
  )
where

import           Hasura.Prelude

import           Hasura.Logging
import           Hasura.RQL.DDL.Schema.Table (buildSchemaCache)
import           Hasura.RQL.Types
import           Hasura.Server.App           (withLock)
import           Hasura.Server.Query
import           Hasura.Server.Utils         (bsToTxt)

import           Control.Concurrent.MVar
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.UUID
import           Database.PG.Query

import qualified Control.Concurrent          as C
import qualified Control.Concurrent.STM      as STM
import qualified Data.IORef                  as IORef
import qualified Data.Text                   as T
import qualified Data.Time                   as UTC
import qualified Database.PostgreSQL.LibPQ   as PQ
import qualified Network.HTTP.Client         as HTTP

pgChannel :: PGChannel
pgChannel = "hasura_cache_update"

data ThreadType
  = TTListener
  | TTProcessor
  deriving (Eq)

instance Show ThreadType where
  show TTListener  = "listener"
  show TTProcessor = "processor"


data CacheUpdateEventLog
  = CacheUpdateEventLog
  { cuelLogLevel   :: !LogLevel
  , cuelThreadType :: !ThreadType
  , cuelInfo       :: !Value
  } deriving (Show, Eq)

instance ToJSON CacheUpdateEventLog where
  toJSON (CacheUpdateEventLog _ t info) =
    object [ "thread_type" .= show t
           , "info" .= info
           ]

instance ToEngineLog CacheUpdateEventLog where
  toEngineLog threadLog =
    (cuelLogLevel threadLog, "cache_update_event", toJSON threadLog)

data EventPayload
  = EventPayload
  { _epServerId   :: !UUID
  , _epOccurredAt :: !UTC.UTCTime
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''EventPayload)

data CacheUpdateEvent
  = CUEListenSuccess !EventPayload
  | CUEListenFail
  deriving (Show, Eq)

instance ToJSON CacheUpdateEvent where
  toJSON (CUEListenSuccess payload) = toJSON payload
  toJSON CUEListenFail              = String "event listening failed"

data ThreadError
  = TEJsonParse !T.Text
  | TEQueryError !QErr
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
 ''ThreadError)

-- | An IO action that listens to postgres for events and pushes them to a Queue
cacheUpdateEventListener
  :: PGPool
  -> Logger
  -> STM.TQueue CacheUpdateEvent
  -> IO ()
cacheUpdateEventListener pool logger eventsQueue =
  -- Never exits
  forever $ do
    listenResE <- liftIO $ runExceptT $ listen pool pgChannel notifyHandler
    either onError return listenResE
  where
    threadType = TTListener

    onError e = do
      logError logger threadType $ TEQueryError e
      -- Push a listen failed event to queue
      STM.atomically $ STM.writeTQueue eventsQueue CUEListenFail
      logInfo logger threadType $
        object ["message" .= ("retrying in 10 seconds" :: T.Text)]
      C.threadDelay $ 10 * 1000 * 1000

    -- Postgres notification handler
    notifyHandler notif = do
      let eventChannel = PGChannel $ bsToTxt $ PQ.notifyRelname notif
      when (eventChannel == pgChannel) $
        case eitherDecodeStrict $ PQ.notifyExtra notif of
          Left e -> logError logger threadType $ TEJsonParse $ T.pack e
          Right payload -> do
            let newEvent = CUEListenSuccess payload
            logInfo logger threadType $ object [ "received_event" .= newEvent]
            -- Push a success event to Queue along with event payload
            STM.atomically $ STM.writeTQueue eventsQueue newEvent

-- | An IO action that processes events from Queue
cacheUpdateEventProcessor
  :: PGPool
  -> Logger
  -> HTTP.Manager
  -> STM.TQueue CacheUpdateEvent
  -> IORef.IORef SchemaCache
  -> MVar ()
  -> UUID
  -> UTC.UTCTime
  -> IO ()
cacheUpdateEventProcessor pool logger httpManager eventsQueue
                       cacheRef lk serverId cacheInit = do
  -- Initiate previous event IO reference
  prevEventRef <- IORef.newIORef Nothing

  -- Never exits
  forever $ do
    event <- STM.atomically $ STM.readTQueue eventsQueue
    prevEvent <- IORef.readIORef prevEventRef
    logInfo logger threadType $
      object [ "previous_event" .= prevEvent
             , "processed_event" .= event
             ]
    when (shouldReload prevEvent event) $ do
      -- Build schema cache from catalog
      scE <- liftIO $ runExceptT $ withLock lk $
             snd <$> peelRun emptySchemaCache adminUserInfo
                     httpManager pool Serializable buildSchemaCache
      case scE of
        Left e -> logError logger threadType $ TEQueryError e
        Right sc -> do
          -- Updata cache reference with newly built cache
          IORef.writeIORef cacheRef sc
          logInfo logger threadType $
            object ["message" .= ("schema cache reloaded" :: T.Text)]

    IORef.writeIORef prevEventRef $ Just event
  where
    threadType = TTProcessor

    -- If previous event is failed and current event is success,
    -- reload irrespective current event payload
    shouldReload (Just CUEListenFail) (CUEListenSuccess _) = True
    -- If current event is failed, do not reload
    shouldReload _                    CUEListenFail        = False
    shouldReload _                    (CUEListenSuccess payload) =
      -- If event is from another server and occurred after
      -- init schema cache built then reload
      (_epServerId payload /= serverId) && (_epOccurredAt payload > cacheInit)

logInfo :: Logger -> ThreadType -> Value -> IO ()
logInfo logger threadType val = unLogger logger $
  CacheUpdateEventLog LevelInfo threadType val

logError :: ToJSON a => Logger -> ThreadType -> a -> IO ()
logError logger threadType err = unLogger logger $
  CacheUpdateEventLog LevelError threadType $ object ["error" .= toJSON err]
