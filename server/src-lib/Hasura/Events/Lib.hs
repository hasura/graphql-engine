{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hasura.Events.Lib
  ( initEventQueue
  , processEventQueue
  , unlockAllEvents
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (async, waitAny)
import qualified Control.Concurrent.STM.TQueue as TQ
import           Control.Monad.STM             (STM, atomically)
import qualified Control.Retry                 as R
import qualified Data.Aeson                    as J
import           Data.Either                   (isLeft)
import           Data.Has
import           Data.Int                      (Int64)
import qualified Data.Text                     as T
import           Data.Time.Clock               (UTCTime)
import qualified Database.PG.Query             as Q
import           Hasura.HTTP
import qualified Hasura.Logging                as L
import           Hasura.Prelude
import           Hasura.RQL.Types
import qualified Network.HTTP.Types            as N
import qualified Network.Wreq                  as W

data Event
  = Event
  { eId          :: UUID
  , eTriggerName :: TriggerName
  , ePayload     :: J.Value
  , eWebhook     :: T.Text
  , eDelivered   :: Maybe Bool
  , eError       :: Maybe Bool
  , eTries       :: Int64
  , eCreatedAt   :: UTCTime
  , eRetryConf   :: (RCNumRetries, RCInterval)
  }

type UUID = T.Text

type RCNumRetries = Int64
type RCInterval = Int64

data Invocation
  = Invocation
  { iEventId  :: UUID
  , iStatus   :: Int64
  , iResponse :: J.Value
  }

type EventQueue = TQ.TQueue Event

initEventQueue :: STM EventQueue
initEventQueue = TQ.newTQueue

processEventQueue :: L.LoggerCtx -> HTTPSessionMgr -> Q.PGPool -> EventQueue -> IO ()
processEventQueue logctx httpSess pool q = do
  putStrLn "starting events..."
  threads <- mapM async [pollThread , consumeThread]
  void $ waitAny threads
  where
    pollThread = pollEvents (mkHLogger logctx) pool q
    consumeThread = consumeEvents (mkHLogger logctx) httpSess pool q


pollEvents
  :: HLogger -> Q.PGPool -> EventQueue -> IO ()
pollEvents logger pool q = forever $ do
  putStrLn "running poller"
  eventsOrError <- runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) fetchEvents
  case eventsOrError of
    Left err     -> putStrLn $ show err
    Right events -> do
      putStrLn "fetched events"
      atomically $ mapM_ (TQ.writeTQueue q) events
  threadDelay (5 * 1000 * 1000)

consumeEvents
  :: HLogger -> HTTPSessionMgr -> Q.PGPool -> EventQueue -> IO ()
consumeEvents logger httpSess pool q  = forever $ do
  putStrLn "running consumer"
  event <- atomically $ TQ.readTQueue q
  putStrLn "got event"
  async $ runReaderT  (processEvent pool event) (logger, httpSess)

processEvent
  :: ( MonadReader r m
     , MonadIO m
     , Has HTTPSessionMgr r
     , Has HLogger r
     )
  => Q.PGPool -> Event -> m ()
processEvent pool e = do
  liftIO $ putStrLn "processing event"
  runExceptT $ runLockQ e
  let remainingRetries = max 0 $ getNumRetries - getTries
      policy = R.constantDelay getDelay <> R.limitRetries remainingRetries
  liftIO $ print remainingRetries
  res <- R.retrying policy shouldRetry tryWebhook
  case res of
    Left err   -> do
      liftIO $ print err
      void $ runExceptT $ runErrorQ e
    Right resp -> return ()
  runExceptT $ runUnlockQ e
  return ()
  where
    tryWebhook
      :: ( MonadReader r m
         , MonadIO m
         , Has HTTPSessionMgr r
         , Has HLogger r
         )
      => R.RetryStatus -> m (Either HTTPErr J.Value)
    tryWebhook retrySt = do
      -- eitherResp <- runExceptT $ runHTTP W.defaults $ mkHTTPPost (T.unpack $ eWebhook undefined) (Just $ ePayload undefined)
      eitherResp <- runExceptT $ runHTTP W.defaults $ mkHTTPPost (T.unpack $ eWebhook e) (Just $ ePayload e)
      finally <- runExceptT $ case eitherResp of
        Left err -> do
          case err of
            HClient excp -> runFailureQ $ Invocation (eId e) 1000 (J.toJSON $ show excp)
            HParse status detail -> runFailureQ $ Invocation (eId e) 1001 (J.toJSON detail)
            HStatus status detail -> runFailureQ $ Invocation (eId e) (fromIntegral $ N.statusCode status) detail
        Right resp -> runSuccessQ e $ Invocation (eId e) 200 resp
      case finally of
        Left err -> liftIO $ print err
        Right _  -> return ()
      return eitherResp

    shouldRetry :: (Monad m ) => R.RetryStatus -> Either HTTPErr a -> m Bool
    shouldRetry retryStatus eitherResp = return $ isLeft eitherResp

    runFailureQ invo = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ insertInvocation invo

    runSuccessQ e' invo' =  Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
      insertInvocation invo'
      markDelivered e'

    runErrorQ e'' = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ markError e''

    runLockQ e'' = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ lockEvent e''

    runUnlockQ e'' = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ unlockEvent e''

    getDelay :: Int
    getDelay = (fromIntegral $ snd (eRetryConf e))* 1000000

    getNumRetries :: Int
    getNumRetries = (fromIntegral $ fst (eRetryConf e))

    getTries :: Int
    getTries = fromIntegral $ eTries e


fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      SELECT e.id, e.trigger_name, e.payload::json, e.webhook, e.tries, e.created_at, r.num_retries, r.interval_seconds
      FROM hdb_catalog.event_log e
      JOIN hdb_catalog.event_triggers_retry_conf r
      ON e.trigger_name = r.name
      WHERE e.delivered ='f' and e.error = 'f' and e.locked = 'f'
      LIMIT 100
      |] () True
  where uncurryEvent (id', trn, Q.AltJ payload, webhook, tries, created, numRetries, retryInterval) = Event id' trn payload webhook Nothing Nothing tries created (numRetries, retryInterval)


insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, response)
          VALUES ($1, $2, $3)
          |] (iEventId invo, iStatus invo, Q.AltJ $ iResponse invo) True
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
