{-# LANGUAGE CPP #-}
module Hasura.Server.SchemaUpdate
  ( startSchemaSyncListenerThread
  , startSchemaSyncProcessorThread
  , logThreadStarted
  , processor
  , refreshSchemaCache
  , pgChannel
  , logInfo
  , logError
  , ThreadError(..)
  , SchemaSyncEventRef
  , SchemaSyncCtx(..)
  , ThreadType(..)
  , SchemaSyncThreadLog(..)
  )
where

import           Hasura.Class
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema          (runCacheRWT)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.App              (SchemaCacheRef (..), updateStateRefs)
import           Hasura.Server.Logging
import           Hasura.Server.Types            (InstanceId (..))
import           Hasura.Session

import           Control.Concurrent.MVar.Lifted (withMVarMasked)
import           Control.Monad.Trans.Control    (MonadBaseControl (..))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.IORef
#ifndef PROFILING
import           GHC.AssertNF
#endif

import qualified Control.Concurrent.Extended    as C
import qualified Control.Concurrent.STM         as STM
import qualified Control.Immortal               as Immortal
import qualified Data.Text                      as T
import qualified Database.PG.Query              as PG
import qualified Database.PostgreSQL.LibPQ      as PQ
import qualified Network.HTTP.Client            as HTTP

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

data ThreadError
  = TEPayloadParse !T.Text
  | TEQueryError !QErr
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "info"
                 }
 ''ThreadError)

type SchemaSyncEventRef = STM.TVar (Maybe Value)

-- | Context required for schema syncing. Listener thread id and event references
-- | ListenerThread is a 'Maybe' for the scenarios where we want to start the thread but do not want to
-- | pass it to the 'runHGEServer'
data SchemaSyncCtx
  = SchemaSyncCtx
  { _sscListenerThreadId :: !(Maybe Immortal.Thread)
  , _sscSyncEventRef     :: !SchemaSyncEventRef
  }

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
  -> m SchemaSyncCtx
startSchemaSyncListenerThread defPgSource logger instanceId = do
  -- only the latest event is recorded here
  -- we don't want to store and process all the events, only the latest event
  schemaSyncEventRef <- liftIO $ STM.newTVarIO Nothing

  -- Start listener thread
  listenerThread <- liftIO $ C.forkImmortal "SchemeUpdate.listener" logger $
                    listener defPgSource logger schemaSyncEventRef
  logThreadStarted logger instanceId TTListener listenerThread
  pure $ SchemaSyncCtx (Just listenerThread) schemaSyncEventRef

-- | An async thread which processes the schema sync events
startSchemaSyncProcessorThread
  :: (C.ForkableMonadIO m, MonadMetadataStorage m)
  => SQLGenCtx
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaSyncEventRef
  -> SchemaCacheRef
  -> InstanceId
  -> m Immortal.Thread
startSchemaSyncProcessorThread sqlGenCtx logger httpMgr
  schemaSyncEventRef cacheRef instanceId = do
  -- Start processor thread
  processorThread <- C.forkImmortal "SchemeUpdate.processor" logger $
                     processor sqlGenCtx logger httpMgr schemaSyncEventRef cacheRef instanceId
  logThreadStarted logger instanceId TTProcessor processorThread
  pure processorThread

-- | An IO action that listens to postgres for events and pushes them to a Queue, in a loop forever.
listener
  :: (C.ForkableMonadIO m)
  => PG.PGPool -> Logger Hasura -> SchemaSyncEventRef -> m void
listener defPgSource logger schemaSyncEventRef =
  -- Never exits
  forever $ do
    listenResE <-
      liftIO $ runExceptT $ PG.listen defPgSource pgChannel notifyHandler
    either onError return listenResE
    logWarn
    liftIO $ C.sleep $ seconds 1
  where
    threadType = TTListener

    notifyHandler = \case
      PG.PNEOnStart        -> pure ()
      PG.PNEPQNotify notif ->
        case eitherDecodeStrict $ PQ.notifyExtra notif of
          Left e -> logError logger threadType $ TEPayloadParse $ T.pack e
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
  :: forall m void. (C.ForkableMonadIO m, MonadMetadataStorage m)
  => SQLGenCtx
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaSyncEventRef
  -> SchemaCacheRef
  -> InstanceId
  -> m void
processor sqlGenCtx logger httpMgr schemaSyncEventRef
  cacheRef instanceId =
  -- Never exits
  forever $ do
    event <- liftIO $ STM.atomically getLatestEvent
    logInfo logger threadType $ object ["processed_event" .= event]
    eitherResult <- runMetadataStorageT $ processSchemaSyncEventPayload instanceId event
    case eitherResult of
      Left e -> logError logger threadType $ TEPayloadParse $ qeError e
      Right (SchemaSyncEventProcessResult shouldReload invalidations) ->
        when shouldReload $
          refreshSchemaCache sqlGenCtx logger httpMgr cacheRef invalidations
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

refreshSchemaCache
  :: ( MonadMetadataStorage m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => SQLGenCtx
  -> Logger Hasura
  -> HTTP.Manager
  -> SchemaCacheRef
  -> CacheInvalidations
  -> ThreadType
  -> T.Text -> m ()
refreshSchemaCache sqlGenCtx logger httpManager cacheRef invalidations threadType msg = do
  -- Reload schema cache from catalog
  resE <- runExceptT $ withRefUpdate $ do
    rebuildableCache <- fst <$> liftIO (readIORef $ _scrCache cacheRef)
    metadata         <- liftEitherM $ runMetadataStorageT getMetadata
    -- metadata         <- getFetchMetadataTx
    --                     >>= (\tx -> liftIO $ runExceptT $ PG.runTx' metadataPool tx)
    --                     >>= liftEither
    (((), cache, _), _) <- buildSchemaCacheWithOptions CatalogSync invalidations noMetadataModify
      & runCacheRWT rebuildableCache APIMetadata
      & peelMetadataRun runCtx metadata
    pure (cache, metadata)
  case resE of
    Left e   -> logError logger threadType $ TEQueryError e
    Right () -> logInfo logger threadType $ object ["message" .= msg]
 where
  runCtx = RunCtx adminUserInfo httpManager sqlGenCtx

  withRefUpdate action =
    withMVarMasked (_scrLock cacheRef) $ \() -> do
      (newSchemaCache, newMetadata) <- action
      updateStateRefs logger newSchemaCache newMetadata cacheRef

logInfo :: MonadIO m => Logger Hasura -> ThreadType -> Value -> m ()
logInfo logger threadType val = unLogger logger $
  SchemaSyncThreadLog LevelInfo threadType val

logError :: MonadIO m => ToJSON a => Logger Hasura -> ThreadType -> a -> m ()
logError logger threadType err =
  unLogger logger $ SchemaSyncThreadLog LevelError threadType $
    object ["error" .= toJSON err]
