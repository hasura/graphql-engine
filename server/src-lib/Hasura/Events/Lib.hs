{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hasura.Events.Lib
  ( initEventQueue
  , processEventQueue
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (async, waitAny)
import qualified Control.Concurrent.STM.TQueue as TQ
import           Control.Monad.STM             (STM, atomically)
import qualified Data.Aeson                    as J
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

data TestLog = TestLog String
instance L.ToEngineLog  TestLog where
  toEngineLog (TestLog s) = (L.LevelInfo, "test-log", J.toJSON s)

data Event
  = Event
  { eId        :: Int64
  , ePayload   :: J.Value
  , eWebhook   :: T.Text
  , eCreatedAt :: UTCTime
  , eProcessed :: Bool
  }

data Invocation
  = Invocation
  { iEventId  :: Int64
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
  eitherResp <- runExceptT $ runHTTP W.defaults $ mkHTTPPost (T.unpack $ eWebhook e) (Just $ ePayload e)
  case eitherResp of
    Left err   -> do
      liftIO $ putStrLn "error from webhook"
      runExceptT $
        case err of
          HClient excp -> runInvocationQ $ Invocation (eId e) 1000 (J.toJSON $ show excp)

          HParse status detail -> runInvocationQ $ Invocation (eId e) 1001 (J.toJSON detail)
          HStatus status detail -> runInvocationQ $ Invocation (eId e) (fromIntegral $ N.statusCode status) detail

    Right resp -> runExceptT $ do
      runInvocationQ $ Invocation (eId e) 200 resp
      runDeliveredQ e
  return ()
  where
    runInvocationQ invo =  Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ insertInvocation invo
    runDeliveredQ ev =  Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ markDelivered ev


fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map (uncurryEvent Event) <$> Q.listQE defaultTxErrorHandler [Q.sql|
      SELECT id, payload::json, webhook, created_at, delivered
      FROM hdb_catalog.event_log
      WHERE delivered ='f'
      |] () True
  where uncurryEvent e (id', Q.AltJ payload, webhook, created, delivered) = e id' payload webhook created delivered

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo =
   Q.unitQE defaultTxErrorHandler [Q.sql|
           INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, response) VALUES ($1, $2, $3)
                |] (iEventId invo, iStatus invo, Q.AltJ $ iResponse invo) True

markDelivered :: Event -> Q.TxE QErr ()
markDelivered e =
   Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.event_log
           SET delivered = 't'
           WHERE id = $1
                |] (Identity $ eId e) True
