module Hasura.Eventing.EventTrigger
  ( initEventEngineCtx
  , processEventQueue
  , unlockAllEvents
  , defaultMaxEventThreads
  , defaultFetchIntervalMilliSec
  , Event(..)
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (async, waitAny)
import           Control.Concurrent.STM.TVar
import           Control.Exception             (bracket_, try)
import           Control.Monad.STM             (STM, atomically, retry)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Has
import           Data.Time.Clock
import           Hasura.Eventing.HTTP
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Version         (HasVersion)
import           Hasura.SQL.Types

import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Data.HashMap.Strict           as M
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import qualified Database.PG.Query             as Q
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP

invocationVersion :: Version
invocationVersion = "2"

data Event
  = Event
  { eId        :: EventId
  , eTable     :: QualifiedTable
  , eTrigger   :: TriggerMetadata
  , eEvent     :: Value
  , eTries     :: Int
  , eCreatedAt :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveFromJSON (aesonDrop 1 snakeCase){omitNothingFields=True} ''Event)

newtype QualifiedTableStrict = QualifiedTableStrict
  { getQualifiedTable :: QualifiedTable
  } deriving (Show, Eq)

instance ToJSON QualifiedTableStrict where
  toJSON (QualifiedTableStrict (QualifiedObject sn tn)) =
     object [ "schema" .= sn
            , "name"  .= tn
           ]

data EventPayload
  = EventPayload
  { epId           :: EventId
  , epTable        :: QualifiedTableStrict
  , epTrigger      :: TriggerMetadata
  , epEvent        :: Value
  , epDeliveryInfo :: DeliveryInfo
  , epCreatedAt    :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventPayload)

data EventEngineCtx
  = EventEngineCtx
  { _eeCtxEventQueue            :: TQ.TQueue Event
  , _eeCtxEventThreads          :: TVar Int
  , _eeCtxMaxEventThreads       :: Int
  , _eeCtxFetchIntervalMilliSec :: Int
  }

defaultMaxEventThreads :: Int
defaultMaxEventThreads = 100

defaultFetchIntervalMilliSec :: Int
defaultFetchIntervalMilliSec = 1000

initEventEngineCtx :: Int -> Int -> STM EventEngineCtx
initEventEngineCtx maxT fetchI = do
  q <- TQ.newTQueue
  c <- newTVar 0
  return $ EventEngineCtx q c maxT fetchI

processEventQueue
  :: (HasVersion) => L.Logger L.Hasura -> LogEnvHeaders -> HTTP.Manager-> Q.PGPool
  -> IO SchemaCache -> EventEngineCtx -> IO ()
processEventQueue logger logenv httpMgr pool getSchemaCache eectx = do
  threads <- mapM async [fetchThread, consumeThread]
  void $ waitAny threads
  where
    fetchThread = pushEvents logger pool eectx
    consumeThread = consumeEvents logger logenv httpMgr pool getSchemaCache eectx

pushEvents
  :: L.Logger L.Hasura -> Q.PGPool -> EventEngineCtx -> IO ()
pushEvents logger pool eectx  = forever $ do
  let EventEngineCtx q _ _ fetchI = eectx
  eventsOrError <- runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) fetchEvents
  case eventsOrError of
    Left err     -> L.unLogger logger $ EventInternalErr err
    Right events -> atomically $ mapM_ (TQ.writeTQueue q) events
  threadDelay (fetchI * 1000)

consumeEvents
  :: (HasVersion) => L.Logger L.Hasura -> LogEnvHeaders -> HTTP.Manager -> Q.PGPool -> IO SchemaCache
  -> EventEngineCtx -> IO ()
consumeEvents logger logenv httpMgr pool getSC eectx  = forever $ do
  event <- atomically $ do
    let EventEngineCtx q _ _ _ = eectx
    TQ.readTQueue q
  async $ runReaderT (processEvent logenv pool getSC event) (logger, httpMgr, eectx)

processEvent
  :: ( HasVersion
     , MonadReader r m
     , Has HTTP.Manager r
     , Has (L.Logger L.Hasura) r
     , Has EventEngineCtx r
     , MonadIO m
     )
  => LogEnvHeaders -> Q.PGPool -> IO SchemaCache -> Event -> m ()
processEvent logenv pool getSchemaCache e = do
  cache <- liftIO getSchemaCache
  eventEngineCtx <- asks getter
  let meti = getEventTriggerInfoFromEvent cache e
  case meti of
    Nothing -> do
      logQErr $ err500 Unexpected "table or event-trigger not found in schema cache"
    Just eti -> do
      let webhook = T.unpack $ wciCachedValue $ etiWebhookInfo eti
          retryConf = etiRetryConf eti
          timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
          respTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
          headerInfos = etiHeaders eti
          etHeaders = map encodeHeader headerInfos
          headers = addDefaultHeaders etHeaders
          ep = createEventPayload retryConf e
          extraLogCtx = ExtraLogContext (epId ep)
      res <- runExceptT $ withEventEngineCtx eventEngineCtx $
        tryWebhook headers respTimeout (toJSON ep) webhook (Just extraLogCtx)
      let decodedHeaders = map (decodeHeader logenv headerInfos) headers
      finally <- either
        (processError pool e retryConf decodedHeaders ep)
        (processSuccess pool e decodedHeaders ep) res
      either logQErr return finally

withEventEngineCtx ::
  ( MonadReader r m
  , Has HTTP.Manager r
  , Has (L.Logger L.Hasura) r
  , MonadIO m
  , MonadError HTTPErr m
  )
  => EventEngineCtx
  -> m HTTPResp
  -> m HTTPResp
withEventEngineCtx eeCtx = bracket_ (incrementThreadCount eeCtx) (decrementThreadCount eeCtx)

incrementThreadCount :: EventEngineCtx -> IO ()
incrementThreadCount (EventEngineCtx _ c maxT _ ) = atomically $ do
  countThreads <- readTVar c
  if countThreads >= maxT
    then retry
    else modifyTVar' c (+1)

decrementThreadCount :: EventEngineCtx -> IO ()
decrementThreadCount (EventEngineCtx _ c _ _) = atomically $ modifyTVar' c (\v -> v - 1)

createEventPayload :: RetryConf -> Event ->  EventPayload
createEventPayload retryConf e = EventPayload
    { epId           = eId e
    , epTable        = QualifiedTableStrict { getQualifiedTable = eTable e}
    , epTrigger      = eTrigger e
    , epEvent        = eEvent e
    , epDeliveryInfo =  DeliveryInfo
                        { diCurrentRetry = eTries e
                        , diMaxRetries   = rcNumRetries retryConf
                        }
    , epCreatedAt    = eCreatedAt e
    }

processSuccess
  :: ( MonadIO m )
  => Q.PGPool -> Event -> [HeaderConf] -> EventPayload -> HTTPResp
  -> m (Either QErr ())
processSuccess pool e decodedHeaders ep resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvocation ep respStatus decodedHeaders respBody respHeaders
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    setSuccess e

processError
  :: ( MonadIO m
     , MonadReader r m
     , Has (L.Logger L.Hasura) r
     )
  => Q.PGPool -> Event -> RetryConf -> [HeaderConf] -> EventPayload -> HTTPErr
  -> m (Either QErr ())
processError pool e retryConf decodedHeaders ep err = do
  logHTTPErr err
  let invocation = case err of
        HClient excp -> do
          let errMsg = TBS.fromLBS $ encode $ show excp
          mkInvocation ep 1000 decodedHeaders errMsg []
        HParse _ detail -> do
          let errMsg = TBS.fromLBS $ encode detail
          mkInvocation ep 1001 decodedHeaders errMsg []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvocation ep respStatus decodedHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = (TBS.fromLBS $ encode detail)
          mkInvocation ep 500 decodedHeaders errMsg []
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    retryOrSetError e retryConf err

retryOrSetError :: Event -> RetryConf -> HTTPErr -> Q.TxE QErr ()
retryOrSetError e retryConf err = do
  let mretryHeader = getRetryAfterHeaderFromHTTPErr err
      tries = eTries e
      mretryHeaderSeconds = join $ parseRetryHeaderValue <$> mretryHeader
      triesExhausted = tries >= rcNumRetries retryConf
      noRetryHeader = isNothing mretryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then do
      setError e
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (rcIntervalSec retryConf) mretryHeaderSeconds
          diff = fromIntegral delay
          retryTime = addUTCTime diff currentTime
      setRetry e retryTime

mkInvocation
  :: EventPayload -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation
mkInvocation ep status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (epId ep)
      status
      (mkWebhookReq (toJSON ep) reqHeaders invocationVersion)
      resp

getEventTriggerInfoFromEvent :: SchemaCache -> Event -> Maybe EventTriggerInfo
getEventTriggerInfoFromEvent sc e = let table = eTable e
                                        tableInfo = M.lookup table $ scTables sc
                                    in M.lookup ( tmName $ eTrigger e) =<< (_tiEventTriggerInfoMap <$> tableInfo)

fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = 't'
      WHERE id IN ( SELECT l.id
                    FROM hdb_catalog.event_log l
                    WHERE l.delivered = 'f' and l.error = 'f' and l.locked = 'f'
                          and (l.next_retry_at is NULL or l.next_retry_at <= now())
                          and l.archived = 'f'
                    FOR UPDATE SKIP LOCKED
                    LIMIT 100 )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
      |] () True
  where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
          Event
          { eId        = id'
          , eTable     = QualifiedObject sn tn
          , eTrigger   = TriggerMetadata trn
          , eEvent     = payload
          , eTries     = tries
          , eCreatedAt = created
          }

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, request, response)
          VALUES ($1, $2, $3, $4)
          |] ( iEventId invo
             , toInt64 $ iStatus invo
             , Q.AltJ $ toJSON $ iRequest invo
             , Q.AltJ $ toJSON $ iResponse invo) True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

setSuccess :: Event -> Q.TxE QErr ()
setSuccess e = Q.unitQE defaultTxErrorHandler [Q.sql|
                        UPDATE hdb_catalog.event_log
                        SET delivered = 't', next_retry_at = NULL, locked = 'f'
                        WHERE id = $1
                        |] (Identity $ eId e) True

setError :: Event -> Q.TxE QErr ()
setError e = Q.unitQE defaultTxErrorHandler [Q.sql|
                        UPDATE hdb_catalog.event_log
                        SET error = 't', next_retry_at = NULL, locked = 'f'
                        WHERE id = $1
                        |] (Identity $ eId e) True

setRetry :: Event -> UTCTime -> Q.TxE QErr ()
setRetry e time =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET next_retry_at = $1, locked = 'f'
          WHERE id = $2
          |] (time, eId e) True

unlockAllEvents :: Q.TxE QErr ()
unlockAllEvents =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET locked = 'f'
          WHERE locked = 't'
          |] () False
