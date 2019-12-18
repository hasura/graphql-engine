{-# LANGUAGE RecordWildCards #-}

module Hasura.Eventing.ScheduledTrigger
  ( processScheduledQueue
  , runScheduledEventsGenerator
  ) where

import           Control.Concurrent              (threadDelay)
import           Control.Exception               (try)
import           Data.Has
import           Data.Time.Clock
import           Hasura.Eventing.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.Types
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           System.Cron

import qualified Data.Aeson                      as J
import qualified Data.Aeson.Casing               as J
import qualified Data.Aeson.TH                   as J
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.TByteString                as TBS
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import qualified Database.PG.Query               as Q
import qualified Hasura.Logging                  as L
import qualified Network.HTTP.Client             as HTTP
import qualified Text.Builder                    as TB (run)

import           Debug.Trace

invocationVersion :: Version
invocationVersion = "1"

oneSecond :: Int
oneSecond = 1000000

oneMinute :: Int
oneMinute = 60 * oneSecond

oneHour :: Int
oneHour = 60 * oneMinute

endOfTime :: UTCTime
endOfTime = read "2999-12-31 00:00:00 Z"

-- type LogEnvHeaders = Bool

type ScheduledEventPayload = J.Value

scheduledEventsTable :: QualifiedTable
scheduledEventsTable =
  QualifiedObject
    hdbCatalogSchema
    (TableName $ T.pack "hdb_scheduled_events")

data ScheduledEvent
  = ScheduledEvent
  { seId            :: !(Maybe Text)
  , seName          :: !T.Text
  , seWebhook       :: !T.Text
  , sePayload       :: !J.Value
  , seScheduledTime :: !UTCTime
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 2 J.snakeCase){J.omitNothingFields=True} ''ScheduledEvent)

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
       SELECT st.name, st.webhook, st.schedule, st.payload
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
              , siCols     = map (PGCol . T.pack) ["name", "webhook", "payload", "scheduled_time"]
              , siValues   = ValuesExp $ map (toTupleExp . toArr) events
              , siConflict = Just $ DoNothing Nothing
              , siRet      = Nothing
              }
      Q.unitQE defaultTxErrorHandler (Q.fromText insertScheduledEventsSql) () False
  where
    toArr (ScheduledEvent _ n w p t) =
      n : w : (TE.decodeUtf8 . LBS.toStrict $ J.encode p) : (pure $ formatTime' t)
    toTupleExp = TupleExp . map SELit
    uncurrySchedule (n, w, st, p) =
      ScheduledTrigger
      { stName = n,
        stWebhook = w,
        stSchedule = fromBS st,
        stPayload = Q.getAltJ <$> p
      }
    fromBS st = fromMaybe (OneOff endOfTime) $ J.decodeStrict' st

mkScheduledEvents :: UTCTime -> ScheduledTrigger-> [ScheduledEvent]
mkScheduledEvents time ScheduledTrigger{..} =
  let events =
        case stSchedule of
          OneOff schedTime -> [schedTime] -- one-off scheduled events need not be generated
          Cron cron ->
            generateScheduledEventsBetween
              time
              (addUTCTime nominalDay time)
              cron
   in map (ScheduledEvent Nothing stName stWebhook (fromMaybe J.Null stPayload)) events

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
  res <- runExceptT $ tryWebhook httpMgr responseTimeout sePayload seWebhook
  -- let decodedHeaders = map (decodeHeader logenv headerInfos) headers
  finally <- either
    (processError pgpool se)
    (processSuccess pgpool se) res
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
  let decodedHeaders = []
      invocation = case err of
       HClient excp -> do
         let errMsg = TBS.fromLBS $ J.encode $ show excp
         mkInvo se 1000 decodedHeaders errMsg []
       HParse _ detail -> do
         let errMsg = TBS.fromLBS $ J.encode detail
         mkInvo se 1001 decodedHeaders errMsg []
       HStatus errResp -> do
         let respPayload = hrsBody errResp
             respHeaders = hrsHeaders errResp
             respStatus = hrsStatus errResp
         mkInvo se respStatus decodedHeaders respPayload respHeaders
       HOther detail -> do
         let errMsg = (TBS.fromLBS $ J.encode detail)
         mkInvo se 500 decodedHeaders errMsg []
  liftIO $
    runExceptT $
    Q.runTx pgpool (Q.RepeatableRead, Just Q.ReadWrite) $ do
      insertInvocation invocation
      markError
  where
    markError =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_events
        SET error = 't', locked = 'f'
        WHERE id = $1
      |] (Identity $ seId se) True

processSuccess :: (MonadIO m) => Q.PGPool -> ScheduledEvent -> HTTPResp -> m (Either QErr ())
processSuccess pgpool se resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      decodedHeaders = []
      invocation = mkInvo se respStatus decodedHeaders respBody respHeaders
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

mkInvo
  :: ScheduledEvent -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation
mkInvo se status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (fromMaybe "unknown" $ seId se) -- WARN: should never happen?
      status
      (mkWebhookReq (J.toJSON se) reqHeaders invocationVersion)
      resp

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.hdb_scheduled_event_invocation_logs (event_id, status, request, response)
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

getScheduledEvents :: Q.TxE QErr [ScheduledEvent]
getScheduledEvents = do
  allSchedules <- map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_scheduled_events
      SET locked = 't'
      WHERE id IN ( SELECT t.id
                    FROM hdb_catalog.hdb_scheduled_events t
                    WHERE ( t.locked = 'f'
                            and t.delivered = 'f'
                            and t.error = 'f'
                            and t.scheduled_time <= now()
                          )
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING id, name, webhook, payload, scheduled_time
      |] () True
  pure $ allSchedules
  where uncurryEvent (i, n, w, Q.AltJ p, st) =
          ScheduledEvent
          { seId      = i
          , seName    = n
          , seWebhook = w
          , sePayload = p
          , seScheduledTime = st
          }
