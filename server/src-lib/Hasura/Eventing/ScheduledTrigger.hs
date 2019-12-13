module Hasura.Eventing.ScheduledTrigger
  ( processScheduledQueue
  , runScheduledEventsGenerator
  ) where

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.Async
import           Data.Time.Clock
import           Data.Time.Format
import           Hasura.Prelude
import           Hasura.RQL.DDL.TimedTrigger
import           Hasura.RQL.Types
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           System.Cron

import qualified Data.Text                   as T
import qualified Database.PG.Query           as Q
import qualified Text.Builder                as TB (run)

import           Debug.Trace

scheduledEventsTable :: QualifiedTable
scheduledEventsTable =
  QualifiedObject
    hdbCatalogSchema
    (TableName $ T.pack "hdb_scheduled_trigger_events")

data ScheduledEvent
  = ScheduledEvent
  { seName          :: !T.Text
  , seWebhook       :: !T.Text
  , seScheduledTime :: !UTCTime
  } deriving (Show, Eq)

runScheduledEventsGenerator :: Q.PGPool -> IO ()
runScheduledEventsGenerator pgpool = do
  forever $ do
    traceM "entering scheduled events generator"
    runExceptT
      (Q.runTx
         pgpool
         (Q.RepeatableRead, Just Q.ReadWrite)
         generateScheduledEvents) >>= \case
      Right _ -> pure ()
      Left err -> traceShowM err
    threadDelay oneHour
  where
    oneHour = 60 * 60 * 1000000

generateScheduledEvents :: Q.TxE QErr ()
generateScheduledEvents = do
  allSchedules <- map uncurrySchedule <$> Q.listQE defaultTxErrorHandler
      [Q.sql|
       SELECT st.name, st.webhook, st.schedule
        FROM hdb_catalog.hdb_scheduled_trigger st
      |] () False
  currentTime <- liftIO getCurrentTime
  let scheduledEvents = concatMap (mkScheduledEvents currentTime) allSchedules
  case scheduledEvents of
    []     -> pure ()
    events -> do
      let insertScheduledEventsSql = TB.run $ toSQL
            SQLInsert
              { siTable    = scheduledEventsTable
              , siCols     = map (PGCol . T.pack) ["name", "webhook", "scheduled_time"]
              , siValues   = ValuesExp $ map (toTupleExp . toArr) events
              , siConflict = Just $ DoNothing Nothing
              , siRet      = Nothing
              }
      Q.unitQE defaultTxErrorHandler (Q.fromText insertScheduledEventsSql) () False
  where
    toArr (ScheduledEvent n w t) = n : w : (pure $ formatTime' t)
    toTupleExp = TupleExp . map SELit
    uncurrySchedule (n, w, st) =
      TimedTriggerQuery {ttqName = n, ttqWebhook = w, ttqSchedule = st}

mkScheduledEvents :: UTCTime -> TimedTriggerQuery -> [ScheduledEvent]
mkScheduledEvents time (TimedTriggerQuery name webhook schedule) =
  let events =
        case parseCronSchedule $ unNonEmptyText schedule of
          Right cron ->
            generateScheduledEventsBetween
              time
              (addUTCTime nominalDay time)
              cron
          Left _err -> []
   in map (ScheduledEvent (unNonEmptyText name) (unNonEmptyText webhook)) events

-- generates events (from, till] according to CronSchedule
generateScheduledEventsBetween :: UTCTime -> UTCTime -> CronSchedule -> [UTCTime]
generateScheduledEventsBetween from till cron = takeWhile ((>=) till) $ go from
  where
    go init =
      case nextMatch cron init of
        Nothing   -> []
        Just next -> next : (go next)

processScheduledQueue :: Q.PGPool -> IO ()
processScheduledQueue pgpool =
  forever $ do
    traceM "entering processor queue"
    scheduledEventsE <-
      runExceptT $
      Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) getScheduledEvents
    case scheduledEventsE of
      Right events -> sequence_ $ map processScheduledEvent events
      Left err     -> traceShowM err
    threadDelay oneMinute
  where
    oneMinute = 60 * 1000000

processScheduledEvent :: ScheduledEvent -> IO (Async ())
processScheduledEvent = async . traceShowM

getScheduledEvents :: Q.TxE QErr [ScheduledEvent]
getScheduledEvents = do
  allSchedules <- map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_scheduled_trigger_events
      SET locked = 't'
      WHERE name IN ( SELECT t.name
                    FROM hdb_catalog.hdb_scheduled_trigger_events t
                    WHERE t.locked = 'f'
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING name, webhook, scheduled_time
      |] () True
  pure $ allSchedules
  where uncurryEvent (n, w, st) =
          ScheduledEvent
          { seName    = n
          , seWebhook = w
          , seScheduledTime = st
          }

-- RFC822
formatTime' :: UTCTime -> T.Text
formatTime' = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"
