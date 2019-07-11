module Hasura.Events.Lib
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
import           Control.Exception             (try)
import           Control.Monad.STM             (STM, atomically, retry)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Has
import           Data.Int                      (Int64)
import           Data.IORef                    (IORef, readIORef)
import           Data.Time.Clock
import           Hasura.Events.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Version         (currentVersion)
import           Hasura.SQL.Types

import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Data.ByteString               as BS
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as M
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Encoding.Error      as TE
import qualified Data.Time.Clock               as Time
import qualified Database.PG.Query             as Q
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP

type Version = T.Text

invocationVersion :: Version
invocationVersion = "2"

type LogEnvHeaders = Bool

newtype CacheRef
  = CacheRef { unCacheRef :: IORef (SchemaCache, SchemaCacheVer) }

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog EventInternalErr where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, L.ELTEventTrigger, toJSON qerr )

data TriggerMeta
  = TriggerMeta { tmName :: TriggerName }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerMeta)

data DeliveryInfo
  = DeliveryInfo
  { diCurrentRetry :: Int
  , diMaxRetries   :: Int
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DeliveryInfo)

data Event
  = Event
  { eId        :: EventId
  , eTable     :: QualifiedTable
  , eTrigger   :: TriggerMeta
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
  , epTrigger      :: TriggerMeta
  , epEvent        :: Value
  , epDeliveryInfo :: DeliveryInfo
  , epCreatedAt    :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventPayload)

data WebhookRequest
  = WebhookRequest
  { _rqPayload :: Value
  , _rqHeaders :: Maybe [HeaderConf]
  , _rqVersion :: T.Text
  }
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''WebhookRequest)

data WebhookResponse
  = WebhookResponse
  { _wrsBody    :: TBS.TByteString
  , _wrsHeaders :: Maybe [HeaderConf]
  , _wrsStatus  :: Int
  }
$(deriveToJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''WebhookResponse)

data ClientError =  ClientError { _ceMessage :: TBS.TByteString}
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ClientError)

data Response = ResponseType1 WebhookResponse | ResponseType2 ClientError

instance ToJSON Response where
  toJSON (ResponseType1 resp) = object
    [ "type" .= String "webhook_response"
    , "data" .= toJSON resp
    , "version" .= invocationVersion
    ]
  toJSON (ResponseType2 err)  = object
    [ "type" .= String "client_error"
    , "data" .= toJSON err
    , "version" .= invocationVersion
    ]

data Invocation
  = Invocation
  { iEventId  :: EventId
  , iStatus   :: Int
  , iRequest  :: WebhookRequest
  , iResponse :: Response
  }

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

retryAfterHeader :: CI.CI T.Text
retryAfterHeader = "Retry-After"

initEventEngineCtx :: Int -> Int -> STM EventEngineCtx
initEventEngineCtx maxT fetchI = do
  q <- TQ.newTQueue
  c <- newTVar 0
  return $ EventEngineCtx q c maxT fetchI

processEventQueue
  :: L.LoggerCtx -> LogEnvHeaders -> HTTP.Manager-> Q.PGPool
  -> IORef (SchemaCache, SchemaCacheVer) -> EventEngineCtx
  -> IO ()
processEventQueue logctx logenv httpMgr pool cacheRef eectx = do
  threads <- mapM async [fetchThread, consumeThread]
  void $ waitAny threads
  where
    fetchThread = pushEvents (mkHLogger logctx) pool eectx
    consumeThread = consumeEvents (mkHLogger logctx)
                    logenv httpMgr pool (CacheRef cacheRef) eectx

pushEvents
  :: HLogger -> Q.PGPool -> EventEngineCtx -> IO ()
pushEvents logger pool eectx  = forever $ do
  let EventEngineCtx q _ _ fetchI = eectx
  eventsOrError <- runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) fetchEvents
  case eventsOrError of
    Left err     -> logger $ L.toEngineLog $ EventInternalErr err
    Right events -> atomically $ mapM_ (TQ.writeTQueue q) events
  threadDelay (fetchI * 1000)

consumeEvents
  :: HLogger -> LogEnvHeaders -> HTTP.Manager -> Q.PGPool -> CacheRef -> EventEngineCtx
  -> IO ()
consumeEvents logger logenv httpMgr pool cacheRef eectx  = forever $ do
  event <- atomically $ do
    let EventEngineCtx q _ _ _ = eectx
    TQ.readTQueue q
  async $ runReaderT  (processEvent logenv pool event) (logger, httpMgr, cacheRef, eectx)

processEvent
  :: ( MonadReader r m
     , MonadIO m
     , Has HTTP.Manager r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => LogEnvHeaders -> Q.PGPool -> Event -> m ()
processEvent logenv pool e = do
  cacheRef <- asks getter
  cache <- fmap fst $ liftIO $ readIORef $ unCacheRef cacheRef
  let meti = getEventTriggerInfoFromEvent cache e
  case meti of
    Nothing -> do
      logQErr $ err500 Unexpected "table or event-trigger not found in schema cache"
    Just eti -> do
      let webhook = T.unpack $ wciCachedValue $ etiWebhookInfo eti
          retryConf = etiRetryConf eti
          timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
          responseTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
          headerInfos = etiHeaders eti
          etHeaders = map encodeHeader headerInfos
          headers = addDefaultHeaders etHeaders
          ep = createEventPayload retryConf e
      res <- runExceptT $ tryWebhook headers responseTimeout ep webhook
      let decodedHeaders = map (decodeHeader logenv headerInfos) headers
      finally <- either
        (processError pool e retryConf decodedHeaders ep)
        (processSuccess pool e decodedHeaders ep) res
      either logQErr return finally

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
      invocation = mkInvo ep respStatus decodedHeaders respBody respHeaders
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    setSuccess e

processError
  :: ( MonadIO m
     , MonadReader r m
     , Has HLogger r
     )
  => Q.PGPool -> Event -> RetryConf -> [HeaderConf] -> EventPayload -> HTTPErr
  -> m (Either QErr ())
processError pool e retryConf decodedHeaders ep err = do
  logHTTPErr err
  let invocation = case err of
        HClient excp -> do
          let errMsg = TBS.fromLBS $ encode $ show excp
          mkInvo ep 1000 decodedHeaders errMsg []
        HParse _ detail -> do
          let errMsg = TBS.fromLBS $ encode detail
          mkInvo ep 1001 decodedHeaders errMsg []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvo ep respStatus decodedHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = (TBS.fromLBS $ encode detail)
          mkInvo ep 500 decodedHeaders errMsg []
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    retryOrSetError e retryConf err

retryOrSetError :: Event -> RetryConf -> HTTPErr -> Q.TxE QErr ()
retryOrSetError e retryConf err = do
  let mretryHeader = getRetryAfterHeaderFromError err
      tries = eTries e
      mretryHeaderSeconds = parseRetryHeader mretryHeader
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
  where
    getRetryAfterHeaderFromError (HStatus resp) = getRetryAfterHeaderFromResp resp
    getRetryAfterHeaderFromError _              = Nothing

    getRetryAfterHeaderFromResp resp
      = let mHeader = find (\(HeaderConf name _)
                            -> CI.mk name == retryAfterHeader) (hrsHeaders resp)
        in case mHeader of
             Just (HeaderConf _ (HVValue value)) -> Just value
             _                                   -> Nothing
    parseRetryHeader Nothing = Nothing
    parseRetryHeader (Just hValue)
      = let seconds = readMaybe $ T.unpack hValue
        in case seconds of
             Nothing  -> Nothing
             Just sec -> if sec > 0 then Just sec else Nothing

encodeHeader :: EventHeaderInfo -> HTTP.Header
encodeHeader (EventHeaderInfo hconf cache) =
  let (HeaderConf name _) = hconf
      ciname = CI.mk $ T.encodeUtf8 name
      value = T.encodeUtf8 cache
  in  (ciname, value)

decodeHeader
  :: LogEnvHeaders -> [EventHeaderInfo] -> (HTTP.HeaderName, BS.ByteString)
  -> HeaderConf
decodeHeader logenv headerInfos (hdrName, hdrVal)
  = let name = decodeBS $ CI.original hdrName
        getName ehi = let (HeaderConf name' _) = ehiHeaderConf ehi
                      in name'
        mehi = find (\hi -> getName hi == name) headerInfos
    in case mehi of
         Nothing -> HeaderConf name (HVValue (decodeBS hdrVal))
         Just ehi -> if logenv
                     then HeaderConf name (HVValue (ehiCachedValue ehi))
                     else ehiHeaderConf ehi
   where
     decodeBS = TE.decodeUtf8With TE.lenientDecode

addDefaultHeaders :: [HTTP.Header] -> [HTTP.Header]
addDefaultHeaders hdrs = hdrs ++
  [ (CI.mk "Content-Type", "application/json")
  , (CI.mk "User-Agent", "hasura-graphql-engine/" <> T.encodeUtf8 currentVersion)
  ]

mkInvo
  :: EventPayload -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation
mkInvo ep status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (epId ep)
      status
      (mkWebhookReq (toJSON ep) reqHeaders)
      resp

mkResp :: Int -> TBS.TByteString -> [HeaderConf] -> Response
mkResp status payload headers =
  let wr = WebhookResponse payload (mkMaybe headers) status
  in ResponseType1 wr

mkClientErr :: TBS.TByteString -> Response
mkClientErr message =
  let cerr = ClientError message
  in ResponseType2 cerr

mkWebhookReq :: Value -> [HeaderConf] -> WebhookRequest
mkWebhookReq payload headers = WebhookRequest payload (mkMaybe headers) invocationVersion

isClientError :: Int -> Bool
isClientError status = status >= 1000

mkMaybe :: [a] -> Maybe [a]
mkMaybe [] = Nothing
mkMaybe x  = Just x

logQErr :: ( MonadReader r m, MonadIO m,  Has HLogger r) => QErr -> m ()
logQErr err = do
  logger <- asks getter
  liftIO $ logger $ L.toEngineLog $ EventInternalErr err

logHTTPErr :: ( MonadReader r m, MonadIO m,  Has HLogger r) => HTTPErr -> m ()
logHTTPErr err = do
  logger <- asks getter
  liftIO $ logger $ L.toEngineLog err

tryWebhook
  :: ( MonadReader r m
     , MonadIO m
     , MonadError HTTPErr m
     , Has HTTP.Manager r
     , Has HLogger r
     , Has EventEngineCtx r
     )
  => [HTTP.Header] -> HTTP.ResponseTimeout -> EventPayload -> String
  -> m HTTPResp
tryWebhook headers responseTimeout ep webhook = do
  let createdAt = epCreatedAt ep
      eventId =  epId ep
  initReqE <- liftIO $ try $ HTTP.parseRequest webhook
  case initReqE of
    Left excp -> throwError $ HClient excp
    Right initReq -> do
      let req = initReq
                { HTTP.method = "POST"
                , HTTP.requestHeaders = headers
                , HTTP.requestBody = HTTP.RequestBodyLBS (encode ep)
                , HTTP.responseTimeout = responseTimeout
                }
      eeCtx <- asks getter
      -- wait for counter and then increment beforing making http
      liftIO $ atomically $ do
        let EventEngineCtx _ c maxT _ = eeCtx
        countThreads <- readTVar c
        if countThreads >= maxT
          then retry
          else modifyTVar' c (+1)

      eitherResp <- runHTTP req  (Just (ExtraContext createdAt eventId))

      -- decrement counter once http is done
      liftIO $ atomically $ do
        let EventEngineCtx _ c _ _ = eeCtx
        modifyTVar' c (\v -> v - 1)

      onLeft eitherResp throwError

getEventTriggerInfoFromEvent :: SchemaCache -> Event -> Maybe EventTriggerInfo
getEventTriggerInfoFromEvent sc e = let table = eTable e
                                        tableInfo = M.lookup table $ scTables sc
                                    in M.lookup ( tmName $ eTrigger e) =<< (tiEventTriggerInfoMap <$> tableInfo)

fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = 't'
      WHERE id IN ( SELECT l.id
                    FROM hdb_catalog.event_log l
                    JOIN hdb_catalog.event_triggers e
                    ON (l.trigger_name = e.name)
                    WHERE l.delivered ='f' and l.error = 'f' and l.locked = 'f'
                          and (l.next_retry_at is NULL or l.next_retry_at <= now())
                    FOR UPDATE SKIP LOCKED
                    LIMIT 100 )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
      |] () True
  where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
          Event
          { eId        = id'
          , eTable     = QualifiedObject sn tn
          , eTrigger   = TriggerMeta trn
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
          |] () False

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral
