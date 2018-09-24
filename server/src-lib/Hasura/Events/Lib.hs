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
  , defaultPollingIntervalSec
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
import qualified Data.ByteString.Lazy          as B
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as M
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Time.Clock               as Time
import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Types            as N
import qualified Network.Wreq                  as W
import qualified Network.Wreq.Session          as WS


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
  -- , eDelivered   :: Bool
  -- , eError       :: Bool
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

data Invocation
  = Invocation
  { iEventId  :: EventId
  , iStatus   :: Int64
  , iRequest  :: Value
  , iResponse :: TBS.TByteString
  }

data EventEngineCtx
  = EventEngineCtx
  { _eeCtxEventQueue         :: TQ.TQueue Event
  , _eeCtxEventThreads       :: TVar Int
  , _eeCtxMaxEventThreads    :: Int
  , _eeCtxPollingIntervalSec :: Int
  }

defaultMaxEventThreads :: Int
defaultMaxEventThreads = 100

defaultPollingIntervalSec :: Int
defaultPollingIntervalSec = 1

initEventEngineCtx :: Int -> Int -> STM EventEngineCtx
initEventEngineCtx maxT pollI = do
  q <- TQ.newTQueue
  c <- newTVar 0
  return $ EventEngineCtx q c maxT pollI

processEventQueue :: L.LoggerCtx -> WS.Session -> Q.PGPool -> CacheRef -> EventEngineCtx -> IO ()
processEventQueue logctx httpSess pool cacheRef eectx = do
  putStrLn "event_trigger: starting workers"
  threads <- mapM async [pollThread , consumeThread]
  void $ waitAny threads
  where
    pollThread = pollEvents (mkHLogger logctx) pool eectx
    consumeThread = consumeEvents (mkHLogger logctx) httpSess pool cacheRef eectx

pollEvents
  :: HLogger -> Q.PGPool -> EventEngineCtx -> IO ()
pollEvents logger pool eectx  = forever $ do
  let EventEngineCtx q _ _ pollI = eectx
  eventsOrError <- runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) fetchEvents
  case eventsOrError of
    Left err     -> logger $ L.toEngineLog $ EventInternalErr err
    Right events -> atomically $ mapM_ (TQ.writeTQueue q) events
  threadDelay (pollI * 1000 * 1000)

consumeEvents
  :: HLogger -> WS.Session -> Q.PGPool -> CacheRef -> EventEngineCtx -> IO ()
consumeEvents logger httpSess pool cacheRef eectx  = forever $ do
  event <- atomically $ do
    let EventEngineCtx q _ _ _ = eectx
    TQ.readTQueue q
  async $ runReaderT  (processEvent pool event) (logger, httpSess, cacheRef, eectx)

processEvent
  :: ( MonadReader r m
     , MonadIO m
     , Has WS.Session r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => Q.PGPool -> Event -> m ()
processEvent pool e = do
  (logger:: HLogger) <- asks getter
  res <- tryWebhook pool e
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
      checkError

    successFn
      :: ( MonadReader r m
         , MonadIO m
         , Has WS.Session r
         , Has HLogger r
         , Has CacheRef r
         , Has EventEngineCtx r
         )
      => B.ByteString -> m (Either QErr ())
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
      => m (Either QErr ())
    checkError = do
      cacheRef::CacheRef <- asks getter
      (cache, _) <- liftIO $ readIORef cacheRef
      let eti = getEventTriggerInfoFromEvent cache e
          retryConfM = etiRetryConf <$> eti
          retryConf = fromMaybe (RetryConf 0 10) retryConfM
          tries = eTries e
      if tries >= rcNumRetries retryConf -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
        then liftIO $ runExceptT $ runErrorAndUnlockQ pool e
        else liftIO $ runExceptT $ runRetryAfterAndUnlockQ pool e retryConf

tryWebhook
  :: ( MonadReader r m
     , MonadIO m
     , Has WS.Session r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => Q.PGPool -> Event -> m (Either HTTPErr B.ByteString)
tryWebhook pool e = do
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
          headersRaw = etiHeaders eti
          headers = map encodeHeader headersRaw
      eeCtx <- asks getter
      -- wait for counter and then increment beforing making http
      liftIO $ atomically $ do
        let EventEngineCtx _ c maxT _ = eeCtx
        countThreads <- readTVar c
        if countThreads >= maxT
          then retry
          else modifyTVar' c (+1)
      eitherResp <- runExceptT $ runHTTP (addHeaders headers W.defaults) (mkAnyHTTPPost (T.unpack webhook) (Just $ toJSON e)) (Just (ExtraContext createdAt eventId))

      --decrement counter once http is done
      liftIO $ atomically $ do
        let EventEngineCtx _ c _ _ = eeCtx
        modifyTVar' c (\v -> v - 1)

      finally <- liftIO $ runExceptT $ case eitherResp of
        Left err ->
          case err of
            HClient excp -> runFailureQ pool $ Invocation (eId e) 1000 (toJSON e) (TBS.fromLBS $ encode $ show excp)
            HParse _ detail -> runFailureQ pool $ Invocation (eId e) 1001 (toJSON e) (TBS.fromLBS $ encode detail)
            HStatus status detail -> runFailureQ pool $ Invocation (eId e) (fromIntegral $ N.statusCode status) (toJSON e) detail
            HOther detail -> runFailureQ pool $ Invocation (eId e) 500 (toJSON e) (TBS.fromLBS $ encode detail)
        Right resp -> runSuccessQ pool e $ Invocation (eId e) 200 (toJSON e) (TBS.fromLBS resp)
      case finally of
        Left err -> liftIO $ logger $ L.toEngineLog $ EventInternalErr err
        Right _  -> return ()
      return eitherResp
  where
    addHeaders :: [(N.HeaderName, BS.ByteString)] -> W.Options -> W.Options
    addHeaders headers opts = foldl (\acc h -> acc CL.& W.header (fst h) CL..~ [snd h] ) opts headers

    encodeHeader :: (HeaderName, T.Text)-> (N.HeaderName, BS.ByteString)
    encodeHeader header =
      let name = CI.mk $ T.encodeUtf8 $ fst header
          value = T.encodeUtf8 $ snd header
      in  (name, value)

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
          |] (iEventId invo, iStatus invo, Q.AltJ $ toJSON $ iRequest invo, Q.AltJ $ toJSON $ iResponse invo) True
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

runRetryAfterAndUnlockQ :: Q.PGPool -> Event -> RetryConf -> ExceptT QErr IO ()
runRetryAfterAndUnlockQ pool e rconf = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
  currentTime <- liftIO getCurrentTime
  let diff = fromIntegral $ rcIntervalSec rconf
      retryTime = addUTCTime diff currentTime
  setNextRetry e retryTime
  unlockEvent e

runUnlockQ :: Q.PGPool -> Event -> ExceptT QErr IO ()
runUnlockQ pool e = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ unlockEvent e
