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
import qualified Data.ByteString.Lazy          as B
import           Data.Either                   (isLeft)
import           Data.Has
import qualified Data.HashMap.Strict           as M
import           Data.Int                      (Int64)
import           Data.IORef                    (IORef, readIORef)
import qualified Data.TByteString              as TBS
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Schema         as GS
import           Hasura.HTTP
import qualified Hasura.Logging                as L
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import qualified Network.HTTP.Types            as N
import qualified Network.Wreq                  as W

type CacheRef = IORef (SchemaCache, GS.GCtxMap)

data Event
  = Event
  { eId          :: UUID
  , eTable       :: QualifiedTable
  , eTriggerName :: TriggerName
  , ePayload     :: J.Value
  -- , eDelivered   :: Bool
  -- , eError       :: Bool
  , eTries       :: Int64
  -- , eCreatedAt   :: UTCTime
  }

type UUID = T.Text

data Invocation
  = Invocation
  { iEventId  :: UUID
  , iStatus   :: Int64
  , iResponse :: TBS.TByteString
  }

type EventQueue = TQ.TQueue Event

initEventQueue :: STM EventQueue
initEventQueue = TQ.newTQueue

processEventQueue :: L.LoggerCtx -> HTTPSessionMgr -> Q.PGPool -> CacheRef -> EventQueue -> IO ()
processEventQueue logctx httpSess pool cacheRef q = do
  putStrLn "starting events..."
  threads <- mapM async [pollThread , consumeThread]
  void $ waitAny threads
  where
    pollThread = pollEvents (mkHLogger logctx) pool q
    consumeThread = consumeEvents (mkHLogger logctx) httpSess pool cacheRef q


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
  :: HLogger -> HTTPSessionMgr -> Q.PGPool -> CacheRef -> EventQueue -> IO ()
consumeEvents logger httpSess pool cacheRef q  = forever $ do
  putStrLn "running consumer"
  event <- atomically $ TQ.readTQueue q
  putStrLn "got event"
  async $ runReaderT  (processEvent pool event) (logger, httpSess, cacheRef)

processEvent
  :: ( MonadReader r m
     , MonadIO m
     , Has HTTPSessionMgr r
     , Has HLogger r
     , Has CacheRef r
     )
  => Q.PGPool -> Event -> m ()
processEvent pool e = do
  liftIO $ runExceptT $ runLockQ e
  retryPolicy <- getRetryPolicy
  res <- R.retrying retryPolicy shouldRetry tryWebhook
  case res of
    Left err   -> do
      liftIO $ print err
      void $ liftIO $ runExceptT $ runErrorQ e
    Right resp -> return ()
  liftIO $ runExceptT $ runUnlockQ e
  return ()
  where
    getRetryPolicy
      :: ( MonadReader r m
         , MonadIO m
         , Has HTTPSessionMgr r
         , Has HLogger r
         , Has CacheRef r
         )
      => m (R.RetryPolicyM m)
    getRetryPolicy = do
      cacheRef::CacheRef <- asks getter
      (cache, _) <- liftIO $ readIORef cacheRef
      let table = eTable e
          tableInfo = M.lookup table $ scTables cache
          eti = M.lookup (eTriggerName e) =<< (tiEventTriggerInfoMap <$> tableInfo)
          retryConfM = etiRetryConf <$> eti
          retryConf = fromMaybe (RetryConf 0 10) retryConfM

      let remainingRetries = max 0 $ fromIntegral (rcNumRetries retryConf) - getTries
          delay = fromIntegral (rcIntervalSec retryConf) * 1000000
          policy = R.constantDelay delay <> R.limitRetries remainingRetries
      return policy

    tryWebhook
      :: ( MonadReader r m
         , MonadIO m
         , Has HTTPSessionMgr r
         , Has HLogger r
         , Has CacheRef r
         )
      => R.RetryStatus -> m (Either HTTPErr B.ByteString)
    tryWebhook _ = do
      cacheRef::CacheRef <- asks getter
      (cache, _) <- liftIO $ readIORef cacheRef
      let table = eTable e
          tableInfo = M.lookup table $ scTables cache
      case tableInfo of
        Nothing -> return $ Left $ HOther "table not found"
        Just ti -> do
          let eti = M.lookup (eTriggerName e) $ tiEventTriggerInfoMap ti
          case eti of
            Nothing -> return $ Left $ HOther "event trigger not found"
            Just et -> do
              let webhook = etiWebhook et
              eitherResp <- runExceptT $ runHTTP W.defaults $ mkAnyHTTPPost (T.unpack webhook) (Just $ ePayload e)
              finally <- liftIO $ runExceptT $ case eitherResp of
                Left err ->
                  case err of
                    HClient excp -> runFailureQ $ Invocation (eId e) 1000 (TBS.fromLBS $ J.encode $ show excp)
                    HParse _ detail -> runFailureQ $ Invocation (eId e) 1001 (TBS.fromLBS $ J.encode detail)
                    HStatus status detail -> runFailureQ $ Invocation (eId e) (fromIntegral $ N.statusCode status) detail
                    HOther detail -> runFailureQ $ Invocation (eId e) 500 (TBS.fromLBS $ J.encode detail)
                Right resp -> runSuccessQ e $ Invocation (eId e) 200 (TBS.fromLBS resp)
              case finally of
                Left err -> liftIO $ print err
                Right _  -> return ()
              return eitherResp

    shouldRetry :: (Monad m ) => R.RetryStatus -> Either HTTPErr a -> m Bool
    shouldRetry _ eitherResp = return $ isLeft eitherResp

    runFailureQ invo = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ insertInvocation invo

    runSuccessQ e' invo' =  Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
      insertInvocation invo'
      markDelivered e'

    runErrorQ e'' = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ markError e''

    runLockQ e'' = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ lockEvent e''

    runUnlockQ e'' = Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ unlockEvent e''

    getTries :: Int
    getTries = fromIntegral $ eTries e

fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = 't'
      WHERE id IN ( select id from hdb_catalog.event_log where delivered ='f' and error = 'f' and locked = 'f' LIMIT 100 )
      RETURNING  id, schema_name, table_name, trigger_name, payload::json, tries
      |] () True
  where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries) = Event id' (QualifiedTable sn tn) trn payload tries


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
