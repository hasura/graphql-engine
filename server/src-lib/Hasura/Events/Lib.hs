{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hasura.Events.Lib
  ( initEventEngineCtx
  , processEventQueue
  , unlockAllEvents
  , defaultMaxEventThreads
  , defaultPollingIntervalSec
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (async, waitAny)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM             (STM, atomically, retry)
import           Data.Either                   (isLeft)
import           Data.Has
import           Data.Int                      (Int64)
import           Data.IORef                    (IORef, readIORef)
import           Hasura.Events.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Retry                 as R
import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as B
import qualified Data.HashMap.Strict           as M
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Data.Time.Clock               as Time
import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.Logging                as L
import qualified Network.HTTP.Types            as N
import qualified Network.Wreq                  as W


type CacheRef = IORef (SchemaCache, GS.GCtxMap)

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog EventInternalErr where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, "event-trigger", J.toJSON qerr )

data Event
  = Event
  { eId          :: UUID
  , eTable       :: QualifiedTable
  , eTriggerName :: TriggerName
  , ePayload     :: J.Value
  -- , eDelivered   :: Bool
  -- , eError       :: Bool
  , eTries       :: Int64
  , eCreatedAt   :: Time.UTCTime
  }

type UUID = T.Text

data Invocation
  = Invocation
  { iEventId  :: UUID
  , iStatus   :: Int64
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

processEventQueue :: L.LoggerCtx -> HTTPSessionMgr -> Q.PGPool -> CacheRef -> EventEngineCtx -> IO ()
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
  :: HLogger -> HTTPSessionMgr -> Q.PGPool -> CacheRef -> EventEngineCtx -> IO ()
consumeEvents logger httpSess pool cacheRef eectx  = forever $ do
  event <- atomically $ do
    let EventEngineCtx q _ _ _ = eectx
    TQ.readTQueue q
  async $ runReaderT  (processEvent pool event) (logger, httpSess, cacheRef, eectx)

processEvent
  :: ( MonadReader r m
     , MonadIO m
     , Has HTTPSessionMgr r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => Q.PGPool -> Event -> m ()
processEvent pool e = do
  (logger:: HLogger) <- asks getter
  retryPolicy <- getRetryPolicy e
  res <- R.retrying retryPolicy shouldRetry $ tryWebhook pool e
  liftIO $ either (errorFn logger) (void.return) res
  unlockRes <- liftIO $ runExceptT $ runUnlockQ pool e
  liftIO $ either (logQErr logger) (void.return ) unlockRes
  where
    shouldRetry :: (Monad m ) => R.RetryStatus -> Either HTTPErr a -> m Bool
    shouldRetry _ eitherResp = return $ isLeft eitherResp

    errorFn :: HLogger -> HTTPErr -> IO ()
    errorFn logger err = do
      logger $ L.toEngineLog err
      errorRes <- runExceptT $ runErrorQ pool e
      case errorRes of
        Left err' -> logQErr logger err'
        Right _   -> return ()

    logQErr :: HLogger -> QErr -> IO ()
    logQErr logger err = logger $ L.toEngineLog $ EventInternalErr err

getRetryPolicy
 :: ( MonadReader r m
    , MonadIO m
    , Has HTTPSessionMgr r
    , Has HLogger r
    , Has CacheRef r
    , Has EventEngineCtx r
    )
 => Event -> m (R.RetryPolicyM m)
getRetryPolicy e = do
  cacheRef::CacheRef <- asks getter
  (cache, _) <- liftIO $ readIORef cacheRef
  let eti = getEventTriggerInfoFromEvent cache e
      retryConfM = etiRetryConf <$> eti
      retryConf = fromMaybe (RetryConf 0 10) retryConfM

  let remainingRetries = max 0 $ fromIntegral (rcNumRetries retryConf) - getTries
      delay = fromIntegral (rcIntervalSec retryConf) * 1000000
      policy = R.constantDelay delay <> R.limitRetries remainingRetries
  return policy
  where
    getTries :: Int
    getTries = fromIntegral $ eTries e

tryWebhook
  :: ( MonadReader r m
     , MonadIO m
     , Has HTTPSessionMgr r
     , Has HLogger r
     , Has CacheRef r
     , Has EventEngineCtx r
     )
  => Q.PGPool -> Event -> R.RetryStatus -> m (Either HTTPErr B.ByteString)
tryWebhook pool e _ = do
  logger:: HLogger <- asks getter
  cacheRef::CacheRef <- asks getter
  (cache, _) <- liftIO $ readIORef cacheRef
  let eti = getEventTriggerInfoFromEvent cache e
  case eti of
    Nothing -> return $ Left $ HOther "table or event-trigger not found"
    Just et -> do
      let webhook = etiWebhook et
          mCreatedAt = Just $ eCreatedAt e
          mEventId = Just $ eId e
      eeCtx <- asks getter

      -- wait for counter and then increment beforing making http
      liftIO $ atomically $ do
        let EventEngineCtx _ c maxT _ = eeCtx
        countThreads <- readTVar c
        if countThreads >= maxT
          then retry
          else modifyTVar' c (+1)
      eitherResp <- runExceptT $ runHTTP W.defaults (mkAnyHTTPPost (T.unpack webhook) (Just $ ePayload e)) (Just (ExtraLog mCreatedAt mEventId))

      --decrement counter once http is done
      liftIO $ atomically $ do
        let EventEngineCtx _ c _ _ = eeCtx
        modifyTVar' c (\v -> v - 1)

      finally <- liftIO $ runExceptT $ case eitherResp of
        Left err ->
          case err of
            HClient excp -> runFailureQ pool $ Invocation (eId e) 1000 (TBS.fromLBS $ J.encode $ show excp)
            HParse _ detail -> runFailureQ pool $ Invocation (eId e) 1001 (TBS.fromLBS $ J.encode detail)
            HStatus status detail -> runFailureQ pool $ Invocation (eId e) (fromIntegral $ N.statusCode status) detail
            HOther detail -> runFailureQ pool $ Invocation (eId e) 500 (TBS.fromLBS $ J.encode detail)
        Right resp -> runSuccessQ pool e $ Invocation (eId e) 200 (TBS.fromLBS resp)
      case finally of
        Left err -> liftIO $ logger $ L.toEngineLog $ EventInternalErr err
        Right _  -> return ()
      return eitherResp

getEventTriggerInfoFromEvent :: SchemaCache -> Event -> Maybe EventTriggerInfo
getEventTriggerInfoFromEvent sc e = let table = eTable e
                                        tableInfo = M.lookup table $ scTables sc
                                    in M.lookup (eTriggerName e) =<< (tiEventTriggerInfoMap <$> tableInfo)

fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = 't'
      WHERE id IN ( select id from hdb_catalog.event_log where delivered ='f' and error = 'f' and locked = 'f' LIMIT 100 )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
      |] () True
  where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) = Event id' (QualifiedTable sn tn) trn payload tries created

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, response)
          VALUES ($1, $2, $3)
          |] (iEventId invo, iStatus invo, Q.AltJ $ J.toJSON $ iResponse invo) True
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

lockEvent :: Event -> Q.TxE QErr ()
lockEvent e =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET locked = 't'
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

runFailureQ :: Q.PGPool -> Invocation -> ExceptT QErr IO ()
runFailureQ pool invo = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ insertInvocation invo

runSuccessQ :: Q.PGPool -> Event -> Invocation -> ExceptT QErr IO ()
runSuccessQ pool e invo =  Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
  insertInvocation invo
  markDelivered e

runErrorQ :: Q.PGPool -> Event -> ExceptT QErr IO ()
runErrorQ pool  e = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ markError e

runLockQ :: Q.PGPool -> Event -> ExceptT QErr IO ()
runLockQ pool e = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ lockEvent e

runUnlockQ :: Q.PGPool -> Event -> ExceptT QErr IO ()
runUnlockQ pool e = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ unlockEvent e
