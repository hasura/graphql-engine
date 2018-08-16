{-# LANGUAGE QuasiQuotes #-}

module Hasura.Events.Lib
  ( initEventQueue
  , processEventQueue
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (async, waitAny)
import qualified Control.Concurrent.STM.TQueue as TQ
import           Control.Monad.STM             (STM, atomically)
import qualified Data.Aeson                    as J
import           Data.Int                      (Int64)
import qualified Data.Text                     as T
import           Data.Time.Clock               (UTCTime)
import qualified Database.PG.Query             as Q
import           Hasura.Prelude
import           Hasura.RQL.Types

data Event
  = Event
  { eId        :: Int64
  , ePayload   :: J.Value
  , eWebhook   :: T.Text
  , eCreatedAt :: UTCTime
  , eProcessed :: Bool
  }

type EventQueue = TQ.TQueue Event

initEventQueue :: STM EventQueue
initEventQueue = TQ.newTQueue

processEventQueue :: Q.PGPool -> EventQueue -> IO ()
processEventQueue pool q = do
  threads <- mapM async [pollThread , consumeThread]
  void $ waitAny threads
  where
    pollThread = pollEvents pool q
    consumeThread = consumeEvents q

pollEvents :: Q.PGPool -> EventQueue -> IO ()
pollEvents pool q = forever $ do
  eventsOrError <- runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) fetchEvents
  case eventsOrError of
    Left err     -> return ()
    Right events -> atomically $ mapM_ (TQ.writeTQueue q) events
  threadDelay (5 * 1000 * 1000)

consumeEvents :: EventQueue -> IO ()
consumeEvents q = forever $ do
  event <- atomically $ TQ.readTQueue q
  async $ processEvent event

processEvent :: Event -> IO ()
processEvent e = undefined

fetchEvents :: Q.TxE QErr [Event]
fetchEvents =
  map (uncurryEvent Event) <$> Q.listQE defaultTxErrorHandler [Q.sql|
      SELECT id, payload, webhook, created_at, processed
      FROM hdb_catalog.events_log
      WHERE processed ='f'
      |] () True
  where uncurryEvent e (id', Q.AltJ payload, webhook, created, processed) = e id' payload webhook created processed
