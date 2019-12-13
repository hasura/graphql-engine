{-# LANGUAGE RecordWildCards #-}

module Hasura.Eventing.ScheduledTrigger
  ( processScheduledQueue
  , runScheduledEventsGenerator
  ) where

import           Control.Concurrent              (threadDelay)
import           Control.Exception               (try)
import           Data.Has
import           Data.Time.Clock
import           Data.Time.Format
import           Hasura.Eventing.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.Types
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           System.Cron

import qualified Data.Aeson                      as J
import qualified Data.Text                       as T
import qualified Database.PG.Query               as Q
import qualified Hasura.Logging                  as L
import qualified Network.HTTP.Client             as HTTP
import qualified Text.Builder                    as TB (run)

import           Debug.Trace

oneSecond :: Int
oneSecond = 1000000

oneMinute :: Int
oneMinute = 60 * oneSecond

oneHour :: Int
oneHour = 60 * oneMinute

-- type LogEnvHeaders = Bool

type ScheduledEventPayload = J.Value

scheduledEventsTable :: QualifiedTable
scheduledEventsTable =
  QualifiedObject
    hdbCatalogSchema
    (TableName $ T.pack "hdb_scheduled_trigger_events")

data ScheduledEvent
  = ScheduledEvent
  { seId            :: !(Maybe Text)
  , seName          :: !T.Text
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
         (Q.ReadCommitted, Just Q.ReadWrite)
         generateScheduledEvents) >>= \case
      Right _ -> pure ()
      Left err -> traceShowM err
    threadDelay oneMinute

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
    toArr (ScheduledEvent _ n w t) = n : w : (pure $ formatTime' t)
    toTupleExp = TupleExp . map SELit
    uncurrySchedule (n, w, st) =
      ScheduledTriggerQuery {stqName = n, stqWebhook = w, stqSchedule = st}

mkScheduledEvents :: UTCTime -> ScheduledTriggerQuery -> [ScheduledEvent]
mkScheduledEvents time ScheduledTriggerQuery{..} =
  let events =
        case parseCronSchedule $ unNonEmptyText stqSchedule of
          Right cron ->
            generateScheduledEventsBetween
              time
              (addUTCTime nominalDay time)
              cron
          Left _err -> []
   in map (ScheduledEvent Nothing (unNonEmptyText stqName) (unNonEmptyText stqWebhook)) events

-- generates events (from, till] according to CronSchedule
generateScheduledEventsBetween :: UTCTime -> UTCTime -> CronSchedule -> [UTCTime]
generateScheduledEventsBetween from till cron = takeWhile ((>=) till) $ go from
  where
    go init =
      case nextMatch cron init of
        Nothing   -> []
        Just next -> next : (go next)

processScheduledQueue :: L.Logger L.Hasura -> Q.PGPool -> HTTP.Manager -> IO ()
processScheduledQueue logger pgpool httpMgr =
  forever $ do
    traceM "entering processor queue"
    scheduledEventsE <-
      runExceptT $
      Q.runTx pgpool (Q.ReadCommitted, Just Q.ReadWrite) getScheduledEvents
    case scheduledEventsE of
      Right events ->
        sequence_ $
        map
          (\ev -> runReaderT (processScheduledEvent pgpool httpMgr ev) (logger))
          events
      Left err -> traceShowM err
    threadDelay (10 * oneSecond)

processScheduledEvent ::
     (MonadReader r m, Has (L.Logger L.Hasura) r, MonadIO m)
  => Q.PGPool
  -> HTTP.Manager
  -> ScheduledEvent
  -> m ()
processScheduledEvent pgpool httpMgr se@ScheduledEvent{..} = do
  -- let webhook = T.unpack $ wciCachedValue $ etiWebhookInfo eti
  --     retryConf = etiRetryConf eti
  let timeoutSeconds = 60
      responseTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
  --     headerInfos = etiHeaders eti
  --     etHeaders = map encodeHeader headerInfos
  --     headers = addDefaultHeaders etHeaders
  --     ep = createEventPayload retryConf e
      eventPayload = J.Null
  res <- runExceptT $ tryWebhook httpMgr responseTimeout eventPayload seWebhook
  -- let decodedHeaders = map (decodeHeader logenv headerInfos) headers
  finally <- either
    (processError pgpool se)
    (processSuccess pgpool se) (trace ("error is " ++ show res) res)
  either logQErr return finally

tryWebhook ::
     ( MonadReader r m
     , Has (L.Logger L.Hasura) r
     , MonadIO m
     , MonadError HTTPErr m
     )
  => HTTP.Manager
  -> HTTP.ResponseTimeout
  -> ScheduledEventPayload
  -> T.Text
  -> m HTTPResp
tryWebhook httpMgr timeout payload webhook = do
  initReqE <- liftIO $ try $ HTTP.parseRequest (T.unpack webhook)
  case initReqE of
    Left excp -> throwError $ HClient excp
    Right initReq -> do
      let req =
            initReq
              { HTTP.method = "POST"
              -- , HTTP.requestHeaders = []
              , HTTP.requestBody = HTTP.RequestBodyLBS (J.encode payload)
              , HTTP.responseTimeout = timeout
              }
      eitherResp <- runHTTP httpMgr req Nothing
      onLeft eitherResp throwError

processError :: (MonadIO m) => Q.PGPool -> ScheduledEvent -> HTTPErr -> m (Either QErr ())
processError pgpool se err = do
  liftIO $
    runExceptT $
    Q.runTx
      pgpool
      (Q.RepeatableRead, Just Q.ReadWrite)
      markError
  where
    markError =
      Q.unitQE defaultTxErrorHandler
      [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_trigger_events
        SET error = 't', locked = 'f'
        WHERE id = $1
      |] (Identity $ seId se) True

processSuccess :: (MonadIO m) => Q.PGPool -> ScheduledEvent -> HTTPResp -> m (Either QErr ())
processSuccess pgpool se resp = do
  liftIO $
    runExceptT $
    Q.runTx
      pgpool
      (Q.RepeatableRead, Just Q.ReadWrite)
      markSuccess
  where
    markSuccess =
      Q.unitQE defaultTxErrorHandler
      [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_trigger_events
        SET delivered = 't', locked = 'f'
        WHERE id = $1
      |] (Identity $ seId se) True

getScheduledEvents :: Q.TxE QErr [ScheduledEvent]
getScheduledEvents = do
  currentTime <- liftIO getCurrentTime
  allSchedules <- map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_scheduled_trigger_events
      SET locked = 't'
      WHERE name IN ( SELECT t.name
                    FROM hdb_catalog.hdb_scheduled_trigger_events t
                    WHERE ( t.locked = 'f'
                            and t.delivered = 'f'
                            and t.error = 'f'
                            and t.scheduled_time <= $1
                          )
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING id, name, webhook, scheduled_time
      |] (Identity currentTime) True
  pure $ allSchedules
  where uncurryEvent (i, n, w, st) =
          ScheduledEvent
          { seId      = i
          , seName    = n
          , seWebhook = w
          , seScheduledTime = st
          }

-- RFC822
formatTime' :: UTCTime -> T.Text
formatTime' = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"
