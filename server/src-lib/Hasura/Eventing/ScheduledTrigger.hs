{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Eventing.ScheduledTrigger
  ( processScheduledQueue
  , runScheduledEventsGenerator
  ) where

import           Control.Concurrent              (threadDelay)
import           Data.Has
import           Data.Time.Clock
import           Data.Time.Clock.Units
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

import           Debug.Trace

invocationVersion :: Version
invocationVersion = "1"

oneSecond :: Int
oneSecond = 1000000

oneMinute :: Int
oneMinute = 60 * oneSecond

oneHour :: Int
oneHour = 60 * oneMinute

scheduledEventsTable :: QualifiedTable
scheduledEventsTable =
  QualifiedObject
    hdbCatalogSchema
    (TableName $ T.pack "hdb_scheduled_events")

data ScheduledEventSeed
 = ScheduledEventSeed
 { sesName          :: !TriggerName
 , sesScheduledTime :: !UTCTime
 } deriving (Show, Eq)

-- | ScheduledEvents can be "partial" or "full"
-- Partial represents the event as present in db
-- Full represents the partial event combined with schema cache configuration elements
data SE_P = SE_PARTIAL | SE_FULL

type family Param (p :: k) x

data ScheduledEvent (p :: SE_P)
  = ScheduledEvent
  { seId            :: !Text
  , seName          :: !TriggerName
  , seScheduledTime :: !UTCTime
  , seTries         :: !Int
  , seWebhook       :: !(Param p T.Text)
  , sePayload       :: !(Param p J.Value)
  , seRetryConf     :: !(Param p RetryConfST)
  }

deriving instance Show (ScheduledEvent 'SE_PARTIAL)
deriving instance Show (ScheduledEvent 'SE_FULL)

type instance Param 'SE_PARTIAL a = ()
type instance Param 'SE_FULL a = a

-- empty splice to bring all the above definitions in scope
$(pure [])

instance ( J.ToJSON (Param p T.Text)
         , J.ToJSON (Param p J.Value)
         , J.ToJSON (Param p Int)
         , J.ToJSON (Param p RetryConfST)
         ) =>
         J.ToJSON (ScheduledEvent p) where
  toJSON = $(J.mkToJSON (J.aesonDrop 2 J.snakeCase) {J.omitNothingFields = True} ''ScheduledEvent)

runScheduledEventsGenerator ::
     L.Logger L.Hasura
  -> Q.PGPool
  -> IO SchemaCache
  -> IO ()
runScheduledEventsGenerator logger pgpool getSC = do
  forever $ do
    traceM "entering scheduled events generator"
    sc <- getSC
    let scheduledTriggers = Map.elems $ scScheduledTriggers sc
    runExceptT
      (Q.runTx
         pgpool
         (Q.ReadCommitted, Just Q.ReadWrite)
         (insertScheduledEventsFor scheduledTriggers) ) >>= \case
      Right _ -> pure ()
      Left err ->
        L.unLogger logger $ EventInternalErr $ err500 Unexpected (T.pack $ show err)
    threadDelay oneHour

insertScheduledEventsFor :: [ScheduledTriggerInfo] -> Q.TxE QErr ()
insertScheduledEventsFor scheduledTriggers = do
  currentTime <- liftIO getCurrentTime
  let scheduledEvents = concatMap (generateScheduledEventsFrom currentTime) scheduledTriggers
  case scheduledEvents of
    []     -> pure ()
    events -> do
      let insertScheduledEventsSql = TB.run $ toSQL
            SQLInsert
              { siTable    = scheduledEventsTable
              , siCols     = map (PGCol . T.pack) ["name", "scheduled_time"]
              , siValues   = ValuesExp $ map (toTupleExp . toArr) events
              , siConflict = Just $ DoNothing Nothing
              , siRet      = Nothing
              }
      Q.unitQE defaultTxErrorHandler (Q.fromText insertScheduledEventsSql) () False
  where
    toArr (ScheduledEventSeed n t) = [(triggerNameToTxt n), (formatTime' t)]
    toTupleExp = TupleExp . map SELit

generateScheduledEventsFrom :: UTCTime -> ScheduledTriggerInfo-> [ScheduledEventSeed]
generateScheduledEventsFrom time ScheduledTriggerInfo{..} =
  let events =
        case stiSchedule of
          OneOff _ -> empty -- one-off scheduled events are generated during creation
          Cron cron ->
            generateScheduleTimesBetween
              time
              (addUTCTime nominalDay time)
              cron
   in map (ScheduledEventSeed stiName) events

-- | Generates events @(from, till]@ according to 'CronSchedule'
generateScheduleTimesBetween :: UTCTime -> UTCTime -> CronSchedule -> [UTCTime]
generateScheduleTimesBetween from till cron = takeWhile (<= till) $ go from
  where
    go init =
      case nextMatch cron init of
        Nothing   -> []
        Just next -> next : (go next)

processScheduledQueue
  :: HasVersion
  => L.Logger L.Hasura
  -> LogEnvHeaders
  -> HTTP.Manager
  -> Q.PGPool
  -> IO SchemaCache
  -> IO ()
processScheduledQueue logger logEnv httpMgr pgpool getSC =
  forever $ do
    traceM "entering processor queue"
    scheduledTriggersInfo <- scScheduledTriggers <$> getSC
    scheduledEventsE <-
      runExceptT $
      Q.runTx pgpool (Q.ReadCommitted, Just Q.ReadWrite) getScheduledEvents
    case scheduledEventsE of
      Right partialEvents ->
        sequence_ $
        flip map partialEvents $ \(ScheduledEvent id' name st tries _ _ _) -> do
          let sti' = Map.lookup name scheduledTriggersInfo
          case sti' of
            Nothing -> traceM "ERROR: couldn't find scheduled trigger in cache"
            Just sti -> do
              let webhook = wciCachedValue $ stiWebhookInfo sti
                  payload = fromMaybe J.Null $ stiPayload sti
                  retryConf = stiRetryConf sti
                  se = ScheduledEvent id' name st tries webhook payload retryConf
              runReaderT (processScheduledEvent logEnv pgpool sti se) (logger, httpMgr)
      Left err -> traceShowM err
    threadDelay oneMinute

processScheduledEvent ::
  ( MonadReader r m
  , Has HTTP.Manager r
  , Has (L.Logger L.Hasura) r
  , HasVersion
  , MonadIO m
  )
  => LogEnvHeaders
  -> Q.PGPool
  -> ScheduledTriggerInfo
  -> ScheduledEvent 'SE_FULL
  -> m ()
processScheduledEvent logEnv pgpool ScheduledTriggerInfo {..} se@ScheduledEvent {..} = do
  currentTime <- liftIO getCurrentTime
  if diffUTCTime currentTime seScheduledTime > rcstTolerance stiRetryConf
    then processDead'
    else do
      let timeoutSeconds = diffTimeToSeconds $ rcstTimeoutSec stiRetryConf
          httpTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
          headers = map encodeHeader stiHeaders
          headers' = addDefaultHeaders headers
          extraLogCtx = ExtraLogContext seId
      res <-
        runExceptT $
        tryWebhook headers' httpTimeout sePayload (T.unpack seWebhook) (Just extraLogCtx)
      let decodedHeaders = map (decodeHeader logEnv stiHeaders) headers'
      finally <- either
                 (processError pgpool se decodedHeaders)
                 (processSuccess pgpool se decodedHeaders)
                 res
      either logQErr return finally
  where
    processDead' =
      processDead pgpool se >>= \case
        Left err -> logQErr err
        Right _ -> pure ()

processError :: (MonadIO m) => Q.PGPool -> ScheduledEvent 'SE_FULL -> [HeaderConf] -> HTTPErr -> m (Either QErr ())
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
  liftIO $
    runExceptT $
    Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) $ do
      insertInvocation invocation
      retryOrMarkError se err

retryOrMarkError :: ScheduledEvent 'SE_FULL -> HTTPErr -> Q.TxE QErr ()
retryOrMarkError se@ScheduledEvent{..} err = do
  let mRetryHeader = getRetryAfterHeaderFromHTTPErr err
      mRetryHeaderSeconds = join $ parseRetryHeaderValue <$> mRetryHeader
      triesExhausted = seTries >= rcstNumRetries seRetryConf
      noRetryHeader = isNothing mRetryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then do
      markError
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (diffTimeToSeconds $ rcstIntervalSec seRetryConf) mRetryHeaderSeconds
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
      |] (Identity seId) True

processSuccess :: (MonadIO m) => Q.PGPool -> ScheduledEvent 'SE_FULL -> [HeaderConf] -> HTTPResp -> m (Either QErr ())
processSuccess pgpool se decodedHeaders resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvocation se respStatus decodedHeaders respBody respHeaders
  liftIO $
    runExceptT $
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
      |] (Identity $ seId se) True

processDead :: (MonadIO m) => Q.PGPool -> ScheduledEvent 'SE_FULL -> m (Either QErr ())
processDead pgpool se =
  liftIO $
  runExceptT $ Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) markDead
  where
    markDead =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET dead = 't', locked = 'f'
          WHERE id = $1
        |] (Identity $ seId se) False

setRetry :: ScheduledEvent 'SE_FULL -> UTCTime -> Q.TxE QErr ()
setRetry se time =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET next_retry_at = $1, locked = 'f'
          WHERE id = $2
          |] (time, seId se) True

mkInvocation
  :: ScheduledEvent 'SE_FULL -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation
mkInvocation se status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (seId se)
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
             , toInt64 $ iStatus invo
             , Q.AltJ $ J.toJSON $ iRequest invo
             , Q.AltJ $ J.toJSON $ iResponse invo) True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

getScheduledEvents :: Q.TxE QErr [ScheduledEvent 'SE_PARTIAL]
getScheduledEvents = do
  partialSchedules <- map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_scheduled_events
      SET locked = 't'
      WHERE id IN ( SELECT t.id
                    FROM hdb_catalog.hdb_scheduled_events t
                    WHERE ( t.locked = 'f'
                            and t.delivered = 'f'
                            and t.error = 'f'
                            and (
                             (t.next_retry_at is NULL and t.scheduled_time <= now()) or
                             (t.next_retry_at is not NULL and t.next_retry_at <= now())
                            )
                            and t.dead = 'f'
                          )
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING id, name, scheduled_time, tries
      |] () True
  pure $ partialSchedules
  where uncurryEvent (i, n, st, tries) = ScheduledEvent i n st tries () () ()
