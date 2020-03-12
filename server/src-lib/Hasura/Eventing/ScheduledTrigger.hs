{-|
= Scheduled Triggers

This module implements the functionality of invoking webhooks during specified time events aka scheduled events.
Scheduled events are modeled using rows in Postgres with a @timestamp@ column.

== Implementation

During startup, two threads are started:

1. Generator: Fetches the list of scheduled triggers from cache and generates scheduled events
if there are less than 100 upcoming events.
This effectively corresponds to doing an INSERT with values containing specific timestamp.
2. Processor: Fetches the scheduled events from db which are @<=NOW()@ and not delivered and delivers them.
The delivery mechanism is similar to Event Triggers; see "Hasura.Eventing.EventTrigger"
-}
module Hasura.Eventing.ScheduledTrigger
  ( processScheduledQueue
  , runScheduledEventsGenerator

  , ScheduledEventSeed(..)
  , generateScheduleTimes
  , insertScheduledEvents
  , defaultMaxScheduledEventThreads
  , initScheduledEventEngineCtx
  ) where

import           Control.Concurrent.Extended   (sleep)
import           Control.Concurrent.Async      (wait, withAsync, async, link)
import           Control.Concurrent.STM.TVar
import           Control.Exception.Lifted      (finally, mask_)
import           Control.Monad.STM
import           Control.Arrow.Extended          (dup)
import           Control.Concurrent              (threadDelay)
import           Data.Has
import           Data.Int (Int64)
import           Data.List                       (unfoldr)
import           Data.String
import           Data.Time.Clock
import           Hasura.Eventing.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           System.Cron
import           Hasura.HTTP
import           Hasura.Server.Version         (HasVersion)

import qualified Data.Aeson                      as J
import qualified Data.Aeson.Casing               as J
import qualified Data.Aeson.TH                   as J
import qualified Data.TByteString                as TBS
import qualified Data.Text                       as T
import qualified Database.PG.Query               as Q
import qualified Hasura.Logging                  as L
import qualified Network.HTTP.Client             as HTTP
import qualified Text.Builder                    as TB (run)
import qualified Data.HashMap.Strict             as Map

invocationVersion :: Version
invocationVersion = "1"

oneSecond :: Int
oneSecond = 1000000

oneMinute :: Int
oneMinute = 60 * oneSecond

newtype ScheduledTriggerInternalErr
  = ScheduledTriggerInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog ScheduledTriggerInternalErr L.Hasura where
  toEngineLog (ScheduledTriggerInternalErr qerr) =
    (L.LevelError, L.scheduledTriggerLogType, J.toJSON qerr)

scheduledEventsTable :: QualifiedTable
scheduledEventsTable =
  QualifiedObject
    hdbCatalogSchema
    (TableName $ T.pack "hdb_scheduled_events")

data ScheduledTriggerStats
  = ScheduledTriggerStats
  { stsName                :: !TriggerName
  , stsUpcomingEventsCount :: !Int
  , stsMaxScheduledTime    :: !UTCTime
  } deriving (Show, Eq)

data ScheduledEventSeed
  = ScheduledEventSeed
  { sesName          :: !TriggerName
  , sesScheduledTime :: !UTCTime
  } deriving (Show, Eq)

data ScheduledEventPartial
  = ScheduledEventPartial
  { sepId            :: !Text
  , sepName          :: !TriggerName
  , sepScheduledTime :: !UTCTime
  , sepPayload       :: !(Maybe J.Value)
  , sepTries         :: !Int
  } deriving (Show, Eq)

data ScheduledEventFull
  = ScheduledEventFull
  { sefId            :: !Text
  , sefName          :: !TriggerName
  , sefScheduledTime :: !UTCTime
  , sefTries         :: !Int
  , sefWebhook       :: !T.Text
  , sefPayload       :: !J.Value
  , sefRetryConf     :: !RetryConfST
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) {J.omitNothingFields = True} ''ScheduledEventFull)

defaultMaxScheduledEventThreads :: Int
defaultMaxScheduledEventThreads = 10

data ScheduledEventEngineCtx
    = ScheduledEventEngineCtx
    { _seeCtxEventThreadsCapacity :: TVar Int
    }

initScheduledEventEngineCtx :: Int -> STM ScheduledEventEngineCtx
initScheduledEventEngineCtx maxT = do
  _seeCtxEventThreadsCapacity <- newTVar maxT
  return $ ScheduledEventEngineCtx{..}

runScheduledEventsGenerator ::
     L.Logger L.Hasura
  -> Q.PGPool
  -> IO SchemaCache
  -> IO ()
runScheduledEventsGenerator logger pgpool getSC = do
  forever $ do
    sc <- getSC
    -- get scheduled triggers from cache
    let scheduledTriggers = Map.elems $ scScheduledTriggers sc

    -- get scheduled trigger stats from db
    runExceptT
      (Q.runTx pgpool (Q.ReadCommitted, Just Q.ReadOnly) getScheduledTriggerStats) >>= \case
      Left err -> L.unLogger logger $
        ScheduledTriggerInternalErr $ err500 Unexpected (T.pack $ show err)
      Right scheduledTriggerStats -> do

        -- join scheduled triggers with stats and produce @[(ScheduledTriggerInfo, ScheduledTriggerStats)]@
        scheduledTriggersWithStats' <- mapM (withStats scheduledTriggerStats) scheduledTriggers
        let scheduledTriggersWithStats = catMaybes scheduledTriggersWithStats'

        -- filter out scheduled trigger which have more than 100 upcoming events already
        let scheduledTriggersForHydration =
              filter (\(_sti, stats) -> stsUpcomingEventsCount stats < 100) scheduledTriggersWithStats

        -- insert scheduled events for scheduled triggers that need hydration
        runExceptT
          (Q.runTx pgpool (Q.ReadCommitted, Just Q.ReadWrite) $
          insertScheduledEventsFor scheduledTriggersForHydration) >>= \case
          Right _ -> pure ()
          Left err ->
            L.unLogger logger $ ScheduledTriggerInternalErr $ err500 Unexpected (T.pack $ show err)
    threadDelay oneMinute
    where
      getScheduledTriggerStats = liftTx $ do
        map uncurryStats <$>
          Q.listQE defaultTxErrorHandler
          [Q.sql|
           SELECT name, upcoming_events_count, max_scheduled_time
            FROM hdb_catalog.hdb_scheduled_events_stats
           |] () True
      uncurryStats (n, count, maxTs) = ScheduledTriggerStats n count maxTs
      withStats stStats sti = do
        let mStats = find (\ScheduledTriggerStats{stsName} -> stsName == stiName sti) stStats
        case mStats of
          Nothing -> do
            L.unLogger logger $
              ScheduledTriggerInternalErr $ err500 Unexpected "could not find scheduled trigger in stats"
            pure Nothing
          Just stats -> pure $ Just (sti, stats)

insertScheduledEventsFor :: [(ScheduledTriggerInfo, ScheduledTriggerStats)] -> Q.TxE QErr ()
insertScheduledEventsFor scheduledTriggersWithStats = do
  let scheduledEvents = flip concatMap scheduledTriggersWithStats $ \(sti, stats) ->
        generateScheduledEventsFrom (stsMaxScheduledTime stats) sti
  case scheduledEvents of
    []     -> pure ()
    events -> do
      let insertScheduledEventsSql = TB.run $ toSQL
            SQLInsert
              { siTable    = scheduledEventsTable
              , siCols     = map unsafePGCol ["name", "scheduled_time"]
              , siValues   = ValuesExp $ map (toTupleExp . toArr) events
              , siConflict = Just $ DoNothing Nothing
              , siRet      = Nothing
              }
      Q.unitQE defaultTxErrorHandler (Q.fromText insertScheduledEventsSql) () False
  where
    toArr (ScheduledEventSeed n t) = [(triggerNameToTxt n), (formatTime' t)]
    toTupleExp = TupleExp . map SELit

insertScheduledEvents :: [ScheduledEventSeed] -> Q.TxE QErr ()
insertScheduledEvents events = do
  let insertScheduledEventsSql = TB.run $ toSQL
        SQLInsert
          { siTable    = scheduledEventsTable
          , siCols     = map unsafePGCol ["name", "scheduled_time"]
          , siValues   = ValuesExp $ map (toTupleExp . toArr) events
          , siConflict = Just $ DoNothing Nothing
          , siRet      = Nothing
          }
  Q.unitQE defaultTxErrorHandler (Q.fromText insertScheduledEventsSql) () False
  where
    toArr (ScheduledEventSeed n t) = [(triggerNameToTxt n), (formatTime' t)]
    toTupleExp = TupleExp . map SELit

generateScheduledEventsFrom :: UTCTime -> ScheduledTriggerInfo-> [ScheduledEventSeed]
generateScheduledEventsFrom startTime ScheduledTriggerInfo{..} =
  let events =
        case stiSchedule of
          AdHoc _ -> empty -- ad-hoc scheduled events are created through 'create_scheduled_event' API
          Cron cron -> generateScheduleTimes startTime 100 cron -- by default, generate next 100 events
   in map (ScheduledEventSeed stiName) events

-- | Generates next @n events starting @from according to 'CronSchedule'
generateScheduleTimes :: UTCTime -> Int -> CronSchedule -> [UTCTime]
generateScheduleTimes from n cron = take n $ go from
  where
    go = unfoldr (fmap dup . nextMatch cron)

-- | processScheduledQueue works like processEventQueue(defined in Eventing/EventTrigger.hs)
-- | Here, the sleep time is hard-coded to 1 minute unlike in processEventQueue where it can
-- | be a value that can be set by the user.
-- | The number of threads to be spawned can be set through the `HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE`,
-- | by default it's set to 10
processScheduledQueue
  :: HasVersion
  => L.Logger L.Hasura
  -> LogEnvHeaders
  -> HTTP.Manager
  -> Q.PGPool
  -> IO SchemaCache
  -> ScheduledEventEngineCtx
  -> IO void
processScheduledQueue logger logenv httpMgr pgpool getSC ScheduledEventEngineCtx{..} = do
    events0 <- popEventsBatch
    go events0 0 False
    where
      fetchBatchSize = 100
      logInternalError err = L.unLogger logger $ ScheduledTriggerInternalErr err
      popEventsBatch = do
        let run = runExceptT . Q.runTx pgpool (Q.ReadCommitted, Just Q.ReadWrite)
        run (getScheduledEvents fetchBatchSize) >>= \case
            Left err -> do
              logInternalError err
              return []
            Right events -> return events

      go :: [ScheduledEventPartial] -> Int -> Bool -> IO void
      go events !fullFetchCount !alreadyWarned = do
        when (null events) $ sleep (fromIntegral oneMinute)
        scheduledTriggersInfo <- scScheduledTriggers <$> getSC
        eventsNext <- withAsync popEventsBatch $ \eventsNextA -> do
          forM_ events $ \(ScheduledEventPartial id' name st payload tries) -> do
            case Map.lookup name scheduledTriggersInfo of
              Nothing -> logInternalError $ err500 Unexpected "could not find scheduled trigger in cache"
              Just stInfo@ScheduledTriggerInfo{..} ->
                mask_ $ do
                  atomically $ do
                    capacity <- readTVar _seeCtxEventThreadsCapacity
                    check $ capacity > 0
                    writeTVar _seeCtxEventThreadsCapacity (capacity - 1)
                  let restoreCapacity = liftIO $ atomically $ modifyTVar' _seeCtxEventThreadsCapacity (+ 1)
                      webhook = wciCachedValue stiWebhookInfo
                      payload' = fromMaybe (fromMaybe J.Null stiPayload) payload
                      scheduledEvent = ScheduledEventFull id' name st tries webhook payload' stiRetryConf
                  t <- async $ runExceptT $ flip runReaderT (logger, httpMgr) $
                         (processScheduledEvent logenv pgpool stInfo scheduledEvent) `finally` restoreCapacity
                  link t

          wait eventsNextA

        let lenEvents = length events
        if | lenEvents == fetchBatchSize -> do
             -- If we've seen N fetches in a row from the DB come back full (i.e. only limited
             -- by our LIMIT clause), then we say we're clearly falling behind:
               let clearlyBehind = fullFetchCount >= 3
               unless alreadyWarned $
                 when clearlyBehind $
                   L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                     "Scheduled Events processor may not be keeping up with events generated in postgres, " <>
                     "or we're working on a backlog of scheduled events. Consider increasing " <>
                     "HASURA_GRAPHQL_SCHEDULED_EVENTS_HTTP_POOL_SIZE"
               go eventsNext (fullFetchCount+1) (alreadyWarned || clearlyBehind)

           | otherwise -> do
               when (lenEvents /= fetchBatchSize && alreadyWarned) $
                 -- emit as warning in case users are only logging warning severity and saw above
                 L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                   "It looks like the scheduled events processor is keeping up again."
               go eventsNext 0 False

processScheduledEvent ::
  ( MonadReader r m
  , Has HTTP.Manager r
  , Has (L.Logger L.Hasura) r
  , HasVersion
  , MonadIO m
  , MonadError QErr m
  )
  => LogEnvHeaders
  -> Q.PGPool
  -> ScheduledTriggerInfo
  -> ScheduledEventFull
  -> m ()
processScheduledEvent logEnv pgpool ScheduledTriggerInfo {..} se@ScheduledEventFull {..} = do
  currentTime <- liftIO getCurrentTime
  if diffUTCTime currentTime sefScheduledTime > rcstTolerance stiRetryConf
    then processDead pgpool se
    else do
      let timeoutSeconds = round $ rcstTimeoutSec stiRetryConf
          httpTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
          headers = addDefaultHeaders $ map encodeHeader stiHeaders
          extraLogCtx = ExtraContext currentTime sefId
      res <- runExceptT $ tryWebhook headers httpTimeout sefPayload (T.unpack sefWebhook)
      logHTTPForST res (Just extraLogCtx)
      let decodedHeaders = map (decodeHeader logEnv stiHeaders) headers
      either
        (processError pgpool se decodedHeaders)
        (processSuccess pgpool se decodedHeaders)
        res

processError
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> ScheduledEventFull -> [HeaderConf] -> HTTPErr a -> m ()
processError pgpool se decodedHeaders err = do
  let invocation = case err of
        HClient excp -> do
          let errMsg = TBS.fromLBS $ J.encode $ show excp
          mkInvocation se 1000 decodedHeaders errMsg []
        HParse _ detail -> do
          let errMsg = TBS.fromLBS $ J.encode detail
          mkInvocation se 1001 decodedHeaders errMsg []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvocation se respStatus decodedHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = (TBS.fromLBS $ J.encode detail)
          mkInvocation se 500 decodedHeaders errMsg []
  liftExceptTIO $
    Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    retryOrMarkError se err

retryOrMarkError :: ScheduledEventFull -> HTTPErr a -> Q.TxE QErr ()
retryOrMarkError se@ScheduledEventFull {..} err = do
  let mRetryHeader = getRetryAfterHeaderFromHTTPErr err
      mRetryHeaderSeconds = parseRetryHeaderValue =<< mRetryHeader
      triesExhausted = sefTries >= rcstNumRetries sefRetryConf
      noRetryHeader = isNothing mRetryHeaderSeconds
  if triesExhausted && noRetryHeader
    then do
      markError
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (round $ rcstIntervalSec sefRetryConf) mRetryHeaderSeconds
          diff = fromIntegral delay
          retryTime = addUTCTime diff currentTime
      setRetry se retryTime
  where
    markError =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_events
        SET error = 't', locked = 'f'
        WHERE id = $1
      |] (Identity sefId) True

processSuccess
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> ScheduledEventFull -> [HeaderConf] -> HTTPResp a -> m ()
processSuccess pgpool se decodedHeaders resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvocation se respStatus decodedHeaders respBody respHeaders
  liftExceptTIO $
    Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    markSuccess
  where
    markSuccess =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_events
        SET delivered = 't', locked = 'f'
        WHERE id = $1
      |] (Identity $ sefId se) True

processDead :: (MonadIO m, MonadError QErr m) => Q.PGPool -> ScheduledEventFull -> m ()
processDead pgpool se =
  liftExceptTIO $
  Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) markDead
  where
    markDead =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET dead = 't', locked = 'f'
          WHERE id = $1
        |] (Identity $ sefId se) False

setRetry :: ScheduledEventFull -> UTCTime -> Q.TxE QErr ()
setRetry se time =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET next_retry_at = $1, locked = 'f'
          WHERE id = $2
          |] (time, sefId se) True

mkInvocation
  :: ScheduledEventFull -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation
mkInvocation se status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (sefId se)
      status
      (mkWebhookReq (J.toJSON se) reqHeaders invocationVersion)
      resp

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.hdb_scheduled_event_invocation_logs
          (event_id, status, request, response)
          VALUES ($1, $2, $3, $4)
          |] ( iEventId invo
             , fromIntegral $ iStatus invo :: Int64
             , Q.AltJ $ J.toJSON $ iRequest invo
             , Q.AltJ $ J.toJSON $ iResponse invo) True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

getScheduledEvents :: Int -> Q.TxE QErr [ScheduledEventPartial]
getScheduledEvents limitI = do
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_scheduled_events
      SET locked = 't'
      WHERE id IN ( SELECT t.id
                    FROM hdb_catalog.hdb_scheduled_events t
                    WHERE ( t.locked = 'f'
                            and t.cancelled = 'f'
                            and t.delivered = 'f'
                            and t.error = 'f'
                            and (
                             (t.next_retry_at is NULL and t.scheduled_time <= now()) or
                             (t.next_retry_at is not NULL and t.next_retry_at <= now())
                            )
                            and t.dead = 'f'
                          )
                    LIMIT $1
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING id, name, scheduled_time, additional_payload, tries
      |] (Identity limit) True
  where uncurryEvent (i, n, st, p, tries) = ScheduledEventPartial i n st (Q.getAltJ <$> p) tries
        limit = fromIntegral limitI :: Word64

liftExceptTIO :: (MonadError e m, MonadIO m) => ExceptT e IO a -> m a
liftExceptTIO m = liftEither =<< liftIO (runExceptT m)
