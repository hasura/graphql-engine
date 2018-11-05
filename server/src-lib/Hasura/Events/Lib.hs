{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Lens                  as CL
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
import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Types            as N
import qualified Network.Wreq                  as W
import qualified Network.Wreq.Session          as WS

type Version = T.Text

invocationVersion :: Version
invocationVersion = "2"

type LogEnvHeaders = Bool

type CacheRef = IORef (SchemaCache, GS.GCtxMap)

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog EventInternalErr where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, "event-trigger", toJSON qerr )

data TriggerMeta
  = TriggerMeta
  { tmId   :: TriggerId
  , tmName :: TriggerName
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerMeta)

data Event
  = Event
  { eId        :: EventId
  , eTable     :: QualifiedTable
  , eTrigger   :: TriggerMeta
  , eEvent     :: Value
  , eTries     :: Int
  , eCreatedAt :: Time.UTCTime
  } deriving (Show, Eq)

instance ToJSON Event where
  toJSON (Event eid (QualifiedTable sn tn) trigger event _ created)=
    object [ "id" .= eid
           , "table"  .= object [ "schema" .= sn
                                , "name"  .= tn
                                ]
           , "trigger" .= trigger
           , "event" .= event
           , "created_at" .= created
           ]

$(deriveFromJSON (aesonDrop 1 snakeCase){omitNothingFields=True} ''Event)

data Request
  = Request
  { _rqPayload :: Value
  , _rqHeaders :: Maybe [HeaderConf]
  , _rqVersion :: T.Text
  }
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''Request)

data WebhookResponse
  = WebhookResponse
  { _wrsBody    :: TBS.TByteString
  , _wrsHeaders :: Maybe [HeaderConf]
  , _wrsStatus  :: Int
  }
$(deriveToJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''WebhookResponse)

data InitError =  InitError { _ieMessage :: TBS.TByteString}
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''InitError)

data Response = ResponseType1 WebhookResponse | ResponseType2 InitError

instance ToJSON Response where
  toJSON (ResponseType1 resp) = object ["type" .= String "webhook_response", "data" .= toJSON resp, "version" .= invocationVersion]
  toJSON (ResponseType2 err)  = object ["type" .= String "init_error", "data" .= toJSON err, "version" .= invocationVersion]

data Invocation
  = Invocation
  { iEventId  :: EventId
  , iStatus   :: Int
  , iRequest  :: Request
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

processEventQueue :: L.LoggerCtx -> LogEnvHeaders -> WS.Session -> Q.PGPool -> CacheRef -> EventEngineCtx -> IO ()
processEventQueue logctx logenv httpSess pool cacheRef eectx = do
  putStrLn "event_trigger: starting workers"
  threads <- mapM async [fetchThread , consumeThread]
  void $ waitAny threads
  where
    fetchThread = pushEvents (mkHLogger logctx) pool eectx
    consumeThread = consumeEvents (mkHLogger logctx) logenv httpSess pool cacheRef eectx

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
  :: HLogger -> LogEnvHeaders -> WS.Session -> Q.PGPool -> CacheRef -> EventEngineCtx -> IO ()
consumeEvents logger logenv httpSess pool cacheRef eectx  = forever $ do
  event <- atomically $ do
    let EventEngineCtx q _ _ _ = eectx
    TQ.readTQueue q
  async $ runReaderT  (processEvent logenv pool event) (logger, httpSess, cacheRef, eectx)

processEvent
  :: ( MonadReader r m
     , MonadIO m
     , Has WS.Session r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => LogEnvHeaders -> Q.PGPool -> Event -> m ()
processEvent logenv pool e = do
  (logger:: HLogger) <- asks getter
  res <- tryWebhook logenv pool e
  finally <- either errorFn successFn res
  liftIO $ either (logQErr logger) (void.return) finally
  where
    errorFn
      :: ( MonadReader r m
         , MonadIO m
         , Has WS.Session r
         , Has HLogger r
         , Has CacheRef r
         , Has EventEngineCtx r
         )
      => HTTPErr -> m (Either QErr ())
    errorFn err = do
      (logger:: HLogger) <- asks getter
      liftIO $ logger $ L.toEngineLog err
      checkError err

    successFn
      :: ( MonadReader r m
         , MonadIO m
         , Has WS.Session r
         , Has HLogger r
         , Has CacheRef r
         , Has EventEngineCtx r
         )
      => HTTPResp -> m (Either QErr ())
    successFn _ = liftIO $ runExceptT $ runUnlockQ pool e

    logQErr :: HLogger -> QErr -> IO ()
    logQErr logger err = logger $ L.toEngineLog $ EventInternalErr err

    checkError
      :: ( MonadReader r m
         , MonadIO m
         , Has WS.Session r
         , Has HLogger r
         , Has CacheRef r
         , Has EventEngineCtx r
         )
      => HTTPErr -> m (Either QErr ())
    checkError err = do
      let mretryHeader = getRetryAfterHeaderFromError err
      cacheRef::CacheRef <- asks getter
      (cache, _) <- liftIO $ readIORef cacheRef
      let eti = getEventTriggerInfoFromEvent cache e
          retryConfM = etiRetryConf <$> eti
          retryConf = fromMaybe (RetryConf 0 10) retryConfM
          tries = eTries e
          mretryHeaderSeconds = parseRetryHeader mretryHeader
          triesExhausted = tries >= rcNumRetries retryConf
          noRetryHeader = isNothing mretryHeaderSeconds
      if triesExhausted && noRetryHeader -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
        then liftIO $ runExceptT $ runErrorAndUnlockQ pool e
        else do
        let delay = fromMaybe (rcIntervalSec retryConf) mretryHeaderSeconds -- give precedence to Retry-After header
        liftIO $ runExceptT $ runRetryAfterAndUnlockQ pool e delay

    getRetryAfterHeaderFromError (HStatus resp) = getRetryAfterHeaderFromResp resp
    getRetryAfterHeaderFromError _ = Nothing

    getRetryAfterHeaderFromResp resp
      = let mHeader = find (\(HeaderConf name _) -> CI.mk name == retryAfterHeader) (hrsHeaders resp)
        in case mHeader of
             Just (HeaderConf _ (HVValue value)) -> Just value
             _                                   -> Nothing

    parseRetryHeader Nothing = Nothing
    parseRetryHeader (Just hValue)
      = let seconds = readMaybe $ T.unpack hValue
        in case seconds of
             Nothing  -> Nothing
             Just sec -> if sec > 0 then Just sec else Nothing

tryWebhook
  :: ( MonadReader r m
     , MonadIO m
     , Has WS.Session r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => LogEnvHeaders -> Q.PGPool -> Event -> m (Either HTTPErr HTTPResp)
tryWebhook logenv pool e = do
  logger:: HLogger <- asks getter
  cacheRef::CacheRef <- asks getter
  (cache, _) <- liftIO $ readIORef cacheRef
  let meti = getEventTriggerInfoFromEvent cache e
  case meti of
    Nothing -> return $ Left $ HOther "table or event-trigger not found"
    Just eti -> do
      let webhook = etiWebhook eti
          createdAt = eCreatedAt e
          eventId =  eId e
          headerInfos = etiHeaders eti
          headers = map encodeHeader headerInfos
      eeCtx <- asks getter
      -- wait for counter and then increment beforing making http
      liftIO $ atomically $ do
        let EventEngineCtx _ c maxT _ = eeCtx
        countThreads <- readTVar c
        if countThreads >= maxT
          then retry
          else modifyTVar' c (+1)
      let options = addHeaders headers W.defaults
          decodedHeaders = map (decodeHeader headerInfos) $ options CL.^. W.headers
      eitherResp <- runExceptT $ runHTTP options (mkAnyHTTPPost (T.unpack webhook) (Just $ toJSON e)) (Just (ExtraContext createdAt eventId))

      --decrement counter once http is done
      liftIO $ atomically $ do
        let EventEngineCtx _ c _ _ = eeCtx
        modifyTVar' c (\v -> v - 1)

      finally <- liftIO $ runExceptT $ case eitherResp of
        Left err ->
          case err of
            HClient excp -> let errMsg = TBS.fromLBS $ encode $ show excp
                            in runFailureQ pool $ mkInvo e 1000 decodedHeaders errMsg []
            HParse _ detail -> let errMsg = TBS.fromLBS $ encode detail
                               in runFailureQ pool $ mkInvo e 1001 decodedHeaders errMsg []
            HStatus errResp -> let respPayload = hrsBody errResp
                                   respHeaders = hrsHeaders errResp
                                   respStatus = hrsStatus errResp
                               in runFailureQ pool $ mkInvo e respStatus decodedHeaders respPayload respHeaders
            HOther detail -> let errMsg = (TBS.fromLBS $ encode detail)
                             in runFailureQ pool $ mkInvo e 500 decodedHeaders errMsg []
        Right resp -> let respPayload = hrsBody resp
                          respHeaders = hrsHeaders resp
                          respStatus = hrsStatus resp
                      in runSuccessQ pool e $ mkInvo e respStatus decodedHeaders respPayload respHeaders
      case finally of
        Left err -> liftIO $ logger $ L.toEngineLog $ EventInternalErr err
        Right _  -> return ()
      return eitherResp
  where
    mkInvo :: Event -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf] -> Invocation
    mkInvo e' status reqHeaders respBody respHeaders
      = let resp = if isInitError status then mkErr respBody else mkResp status respBody respHeaders
        in
          Invocation
          (eId e')
          status
          (mkWebhookReq (toJSON e) reqHeaders)
          resp
    addHeaders :: [(N.HeaderName, BS.ByteString)] -> W.Options -> W.Options
    addHeaders headers opts = foldl (\acc h -> acc CL.& W.header (fst h) CL..~ [snd h] ) opts headers

    encodeHeader :: EventHeaderInfo -> (N.HeaderName, BS.ByteString)
    encodeHeader (EventHeaderInfo hconf cache) =
      let (HeaderConf name _) = hconf
          ciname = CI.mk $ T.encodeUtf8 name
          value = T.encodeUtf8 cache
      in  (ciname, value)

    decodeHeader :: [EventHeaderInfo] -> (N.HeaderName, BS.ByteString) -> HeaderConf
    decodeHeader headerInfos (hdrName, hdrVal)
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

    mkWebhookReq :: Value -> [HeaderConf] -> Request
    mkWebhookReq payload headers = Request payload (mkMaybe headers) invocationVersion

    mkResp :: Int -> TBS.TByteString -> [HeaderConf] -> Response
    mkResp status payload headers =
      let wr = WebhookResponse payload (mkMaybe headers) status
      in ResponseType1 wr

    mkErr :: TBS.TByteString -> Response
    mkErr message =
      let ir = InitError message
      in ResponseType2 ir

    mkMaybe :: [a] -> Maybe [a]
    mkMaybe [] = Nothing
    mkMaybe x  = Just x

    isInitError :: Int -> Bool
    isInitError status = status >= 1000


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
                    ON (l.trigger_id = e.id)
                    WHERE l.delivered ='f' and l.error = 'f' and l.locked = 'f' and (l.next_retry_at is NULL or l.next_retry_at <= now())
                    LIMIT 100 )
      RETURNING id, schema_name, table_name, trigger_id, trigger_name, payload::json, tries, created_at
      |] () True
  where uncurryEvent (id', sn, tn, trid, trn, Q.AltJ payload, tries, created) = Event id' (QualifiedTable sn tn) (TriggerMeta trid trn) payload tries created

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, request, response)
          VALUES ($1, $2, $3, $4)
          |] (iEventId invo, toInt64 $ iStatus invo, Q.AltJ $ toJSON $ iRequest invo, Q.AltJ $ toJSON $ iResponse invo) True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

markDelivered :: Event -> Q.TxE QErr ()
markDelivered e =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET delivered = 't', error = 'f'
          WHERE id = $1
          |] (Identity $ eId e) True

markError :: Event -> Q.TxE QErr ()
markError e =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET error = 't'
          WHERE id = $1
          |] (Identity $ eId e) True

unlockEvent :: Event -> Q.TxE QErr ()
unlockEvent e =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET locked = 'f'
          WHERE id = $1
          |] (Identity $ eId e) True

unlockAllEvents :: Q.TxE QErr ()
unlockAllEvents =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET locked = 'f'
          |] () False

setNextRetry :: Event -> UTCTime -> Q.TxE QErr ()
setNextRetry e time =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET next_retry_at = $1
          WHERE id = $2
          |] (time, eId e) True

clearNextRetry :: Event -> Q.TxE QErr ()
clearNextRetry e =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET next_retry_at = NULL
          WHERE id = $1
          |] (Identity $ eId e) True

runFailureQ :: Q.PGPool -> Invocation -> ExceptT QErr IO ()
runFailureQ pool invo = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ insertInvocation invo

runSuccessQ :: Q.PGPool -> Event -> Invocation -> ExceptT QErr IO ()
runSuccessQ pool e invo =  Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
  insertInvocation invo
  clearNextRetry e
  markDelivered e

runErrorAndUnlockQ :: Q.PGPool -> Event -> ExceptT QErr IO ()
runErrorAndUnlockQ pool  e = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
  markError e
  clearNextRetry e
  unlockEvent e

runRetryAfterAndUnlockQ :: Q.PGPool -> Event -> Int -> ExceptT QErr IO ()
runRetryAfterAndUnlockQ pool e delay = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
  currentTime <- liftIO getCurrentTime
  let diff = fromIntegral delay
      retryTime = addUTCTime diff currentTime
  setNextRetry e retryTime
  unlockEvent e

runUnlockQ :: Q.PGPool -> Event -> ExceptT QErr IO ()
runUnlockQ pool e = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ unlockEvent e

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral
