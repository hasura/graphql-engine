{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Hasura.Events.Lib
  ( initEventEngineCtx
  , processEventQueue
  , unlockAllEvents
  , defaultMaxEventThreads
  , defaultFetchIntervalMilliSec
  , Event(..)
  ) where

import           Control.Concurrent.Async    (async, link, wait, withAsync)
import           Control.Concurrent.Extended (sleep)
import           Control.Concurrent.STM.TVar
import           Control.Exception.Lifted    (finally, mask_, try)
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Has
import           Data.Int                    (Int64)
import           Data.String
import           Data.Time.Clock
import           Data.Word
import           Hasura.Events.HTTP
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Version       (HasVersion)
import           Hasura.SQL.Types

import qualified Data.ByteString             as BS
import qualified Data.CaseInsensitive        as CI
import qualified Data.HashMap.Strict         as M
import qualified Data.Set                    as Set
import qualified Data.TByteString            as TBS
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Time.Clock             as Time
import qualified Database.PG.Query           as Q
import qualified Hasura.Logging              as L
import qualified Network.HTTP.Client         as HTTP
import qualified Network.HTTP.Types          as HTTP

type Version = T.Text

invocationVersion :: Version
invocationVersion = "2"

type LogEnvHeaders = Bool

newtype EventInternalErr
  = EventInternalErr QErr
  deriving (Show, Eq)

instance L.ToEngineLog EventInternalErr L.Hasura where
  toEngineLog (EventInternalErr qerr) = (L.LevelError, L.eventTriggerLogType, toJSON qerr)

data TriggerMeta
  = TriggerMeta { tmName :: TriggerName }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''TriggerMeta)

data DeliveryInfo
  = DeliveryInfo
  { diCurrentRetry :: Int
  , diMaxRetries   :: Int
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DeliveryInfo)

-- | Change data for a particular row
--
-- https://docs.hasura.io/1.0/graphql/manual/event-triggers/payload.html
data Event
  = Event
  { eId        :: EventId
  , eTable     :: QualifiedTable
  , eTrigger   :: TriggerMeta
  , eEvent     :: Value
  , eTries     :: Int
  , eCreatedAt :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveFromJSON (aesonDrop 1 snakeCase){omitNothingFields=True} ''Event)

newtype QualifiedTableStrict = QualifiedTableStrict
  { getQualifiedTable :: QualifiedTable
  } deriving (Show, Eq)

instance ToJSON QualifiedTableStrict where
  toJSON (QualifiedTableStrict (QualifiedObject sn tn)) =
     object [ "schema" .= sn
            , "name"  .= tn
           ]

-- | See 'Event'.
data EventPayload
  = EventPayload
  { epId           :: EventId
  , epTable        :: QualifiedTableStrict
  , epTrigger      :: TriggerMeta
  , epEvent        :: Value
  , epDeliveryInfo :: DeliveryInfo
  , epCreatedAt    :: Time.UTCTime
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''EventPayload)

data WebhookRequest
  = WebhookRequest
  { _rqPayload :: Value
  , _rqHeaders :: Maybe [HeaderConf]
  , _rqVersion :: T.Text
  }
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''WebhookRequest)

data WebhookResponse
  = WebhookResponse
  { _wrsBody    :: TBS.TByteString
  , _wrsHeaders :: Maybe [HeaderConf]
  , _wrsStatus  :: Int
  }
$(deriveToJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''WebhookResponse)

data ClientError =  ClientError { _ceMessage :: TBS.TByteString}
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ClientError)

data Response = ResponseType1 WebhookResponse | ResponseType2 ClientError

instance ToJSON Response where
  toJSON (ResponseType1 resp) = object
    [ "type" .= String "webhook_response"
    , "data" .= toJSON resp
    , "version" .= invocationVersion
    ]
  toJSON (ResponseType2 err)  = object
    [ "type" .= String "client_error"
    , "data" .= toJSON err
    , "version" .= invocationVersion
    ]

data Invocation
  = Invocation
  { iEventId  :: EventId
  , iStatus   :: Int
  , iRequest  :: WebhookRequest
  , iResponse :: Response
  }

data EventEngineCtx
  = EventEngineCtx
  { _eeCtxEventThreadsCapacity :: TVar Int
  , _eeCtxFetchInterval        :: DiffTime
  , _eeCtxLockedEvents         :: TVar (Set.Set EventId)
  }

defaultMaxEventThreads :: Int
defaultMaxEventThreads = 100

defaultFetchIntervalMilliSec :: Milliseconds
defaultFetchIntervalMilliSec = 1000

retryAfterHeader :: CI.CI T.Text
retryAfterHeader = "Retry-After"

initEventEngineCtx :: Int -> DiffTime -> STM EventEngineCtx
initEventEngineCtx maxT _eeCtxFetchInterval = do
  _eeCtxEventThreadsCapacity <- newTVar maxT
  _eeCtxLockedEvents <- newTVar Set.empty
  return $ EventEngineCtx{..}

-- | Service events from our in-DB queue.
--
-- There are a few competing concerns and constraints here; we want to...
--   - fetch events in batches for lower DB pressure
--   - don't fetch more than N at a time (since that can mean: space leak, less
--     effective scale out, possible double sends for events we've checked out
--     on exit (TODO clean shutdown procedure))
--   - try not to cause webhook workers to stall waiting on DB fetch
--   - limit webhook HTTP concurrency per HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE
processEventQueue
  :: (HasVersion) => L.Logger L.Hasura -> LogEnvHeaders -> HTTP.Manager-> Q.PGPool
  -> IO SchemaCache -> EventEngineCtx
  -> IO void
processEventQueue logger logenv httpMgr pool getSchemaCache EventEngineCtx{..} = do
  events0 <- popEventsBatch
  go events0 0 False
  where
    fetchBatchSize = 100
    popEventsBatch = do
      let run = runExceptT . Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite)
      run (fetchEvents fetchBatchSize) >>= \case
          Left err -> do
            L.unLogger logger $ EventInternalErr err
            return []
          Right events ->
            return events

    -- work on this batch of events while prefetching the next. Recurse after we've forked workers
    -- for each in the batch, minding the requested pool size.
    go :: [Event] -> Int -> Bool -> IO void
    go events !fullFetchCount !alreadyWarned = do
      -- process events ASAP until we've caught up; only then can we sleep
      when (null events) $ sleep _eeCtxFetchInterval

      -- Prefetch next events payload while concurrently working through our current batch.
      -- NOTE: we probably don't need to prefetch so early, but probably not
      -- worth the effort for something more fine-tuned
      eventsNext <- withAsync popEventsBatch $ \eventsNextA -> do
        -- process approximately in order, minding HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE:
        forM_ events $ \event ->
          mask_ $ do
            atomically $ do  -- block until < HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE threads:
              capacity <- readTVar _eeCtxEventThreadsCapacity
              check $ capacity > 0
              writeTVar _eeCtxEventThreadsCapacity (capacity - 1)
            -- since there is some capacity in our worker threads, we can launch another:
            let restoreCapacity = liftIO $ atomically $
                  modifyTVar' _eeCtxEventThreadsCapacity (+ 1)
            t <- async $ flip runReaderT (logger, httpMgr) $
                    processEvent event `finally` restoreCapacity
            link t

        -- return when next batch ready; some 'processEvent' threads may be running.
        wait eventsNextA

      let lenEvents = length events
      if | lenEvents == fetchBatchSize -> do
             -- If we've seen N fetches in a row from the DB come back full (i.e. only limited
             -- by our LIMIT clause), then we say we're clearly falling behind:
             let clearlyBehind = fullFetchCount >= 3
             unless alreadyWarned $
               when clearlyBehind $
                 L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                   "Events processor may not be keeping up with events generated in postgres, " <>
                   "or we're working on a backlog of events. Consider increasing " <>
                   "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
             go eventsNext (fullFetchCount+1) (alreadyWarned || clearlyBehind)

         | otherwise -> do
             when (lenEvents /= fetchBatchSize && alreadyWarned) $
               -- emit as warning in case users are only logging warning severity and saw above
               L.unLogger logger $ L.UnstructuredLog L.LevelWarn $ fromString $
                 "It looks like the events processor is keeping up again."
             go eventsNext 0 False

    processEvent
      :: ( HasVersion
         , MonadReader r m
         , Has HTTP.Manager r
         , Has (L.Logger L.Hasura) r
         , MonadIO m
         )
      => Event -> m ()
    processEvent e = do
      cache <- liftIO getSchemaCache
      let meti = getEventTriggerInfoFromEvent cache e
      case meti of
        Nothing -> do
          logQErr $ err500 Unexpected "table or event-trigger not found in schema cache"
        Just eti -> do
          let webhook = T.unpack $ wciCachedValue $ etiWebhookInfo eti
              retryConf = etiRetryConf eti
              timeoutSeconds = fromMaybe defaultTimeoutSeconds (rcTimeoutSec retryConf)
              responseTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
              headerInfos = etiHeaders eti
              etHeaders = map encodeHeader headerInfos
              headers = addDefaultHeaders etHeaders
              ep = createEventPayload retryConf e
          res <- runExceptT $ tryWebhook headers responseTimeout ep webhook
          let decodedHeaders = map (decodeHeader logenv headerInfos) headers
          either
            (processError pool e retryConf decodedHeaders ep)
            (processSuccess pool e decodedHeaders ep) res
            >>= flip onLeft logQErr

createEventPayload :: RetryConf -> Event ->  EventPayload
createEventPayload retryConf e = EventPayload
    { epId           = eId e
    , epTable        = QualifiedTableStrict { getQualifiedTable = eTable e}
    , epTrigger      = eTrigger e
    , epEvent        = eEvent e
    , epDeliveryInfo =  DeliveryInfo
      { diCurrentRetry = eTries e
      , diMaxRetries   = rcNumRetries retryConf
      }
    , epCreatedAt    = eCreatedAt e
    }

processSuccess
  :: ( MonadIO m )
  => Q.PGPool -> Event -> [HeaderConf] -> EventPayload -> HTTPResp
  -> m (Either QErr ())
processSuccess pool e decodedHeaders ep resp = do
  let respBody = hrsBody resp
      respHeaders = hrsHeaders resp
      respStatus = hrsStatus resp
      invocation = mkInvo ep respStatus decodedHeaders respBody respHeaders
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    setSuccess e

processError
  :: ( MonadIO m
     , MonadReader r m
     , Has (L.Logger L.Hasura) r
     )
  => Q.PGPool -> Event -> RetryConf -> [HeaderConf] -> EventPayload -> HTTPErr
  -> m (Either QErr ())
processError pool e retryConf decodedHeaders ep err = do
  logHTTPErr err
  let invocation = case err of
        HClient excp -> do
          let errMsg = TBS.fromLBS $ encode $ show excp
          mkInvo ep 1000 decodedHeaders errMsg []
        HParse _ detail -> do
          let errMsg = TBS.fromLBS $ encode detail
          mkInvo ep 1001 decodedHeaders errMsg []
        HStatus errResp -> do
          let respPayload = hrsBody errResp
              respHeaders = hrsHeaders errResp
              respStatus = hrsStatus errResp
          mkInvo ep respStatus decodedHeaders respPayload respHeaders
        HOther detail -> do
          let errMsg = (TBS.fromLBS $ encode detail)
          mkInvo ep 500 decodedHeaders errMsg []
  liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Just Q.ReadWrite) $ do
    insertInvocation invocation
    retryOrSetError e retryConf err

retryOrSetError :: Event -> RetryConf -> HTTPErr -> Q.TxE QErr ()
retryOrSetError e retryConf err = do
  let mretryHeader = getRetryAfterHeaderFromError err
      tries = eTries e
      mretryHeaderSeconds = mretryHeader >>= parseRetryHeader
      triesExhausted = tries >= rcNumRetries retryConf
      noRetryHeader = isNothing mretryHeaderSeconds
  -- current_try = tries + 1 , allowed_total_tries = rcNumRetries retryConf + 1
  if triesExhausted && noRetryHeader
    then do
      setError e
    else do
      currentTime <- liftIO getCurrentTime
      let delay = fromMaybe (rcIntervalSec retryConf) mretryHeaderSeconds
          diff = fromIntegral delay
          retryTime = addUTCTime diff currentTime
      setRetry e retryTime
  where
    getRetryAfterHeaderFromError (HStatus resp) = getRetryAfterHeaderFromResp resp
    getRetryAfterHeaderFromError _              = Nothing

    getRetryAfterHeaderFromResp resp
      = let mHeader = find (\(HeaderConf name _)
                            -> CI.mk name == retryAfterHeader) (hrsHeaders resp)
        in case mHeader of
             Just (HeaderConf _ (HVValue value)) -> Just value
             _                                   -> Nothing

    parseRetryHeader = mfilter (> 0) . readMaybe . T.unpack

encodeHeader :: EventHeaderInfo -> HTTP.Header
encodeHeader (EventHeaderInfo hconf cache) =
  let (HeaderConf name _) = hconf
      ciname = CI.mk $ T.encodeUtf8 name
      value = T.encodeUtf8 cache
  in  (ciname, value)

decodeHeader
  :: LogEnvHeaders -> [EventHeaderInfo] -> (HTTP.HeaderName, BS.ByteString)
  -> HeaderConf
decodeHeader logenv headerInfos (hdrName, hdrVal)
  = let name = bsToTxt $ CI.original hdrName
        getName ehi = let (HeaderConf name' _) = ehiHeaderConf ehi
                      in name'
        mehi = find (\hi -> getName hi == name) headerInfos
    in case mehi of
         Nothing -> HeaderConf name (HVValue (bsToTxt hdrVal))
         Just ehi -> if logenv
                     then HeaderConf name (HVValue (ehiCachedValue ehi))
                     else ehiHeaderConf ehi

mkInvo
  :: EventPayload -> Int -> [HeaderConf] -> TBS.TByteString -> [HeaderConf]
  -> Invocation
mkInvo ep status reqHeaders respBody respHeaders
  = let resp = if isClientError status
          then mkClientErr respBody
          else mkResp status respBody respHeaders
    in
      Invocation
      (epId ep)
      status
      (mkWebhookReq (toJSON ep) reqHeaders)
      resp

mkResp :: Int -> TBS.TByteString -> [HeaderConf] -> Response
mkResp status payload headers =
  let wr = WebhookResponse payload (mkMaybe headers) status
  in ResponseType1 wr

mkClientErr :: TBS.TByteString -> Response
mkClientErr message =
  let cerr = ClientError message
  in ResponseType2 cerr

mkWebhookReq :: Value -> [HeaderConf] -> WebhookRequest
mkWebhookReq payload headers = WebhookRequest payload (mkMaybe headers) invocationVersion

isClientError :: Int -> Bool
isClientError status = status >= 1000

mkMaybe :: [a] -> Maybe [a]
mkMaybe [] = Nothing
mkMaybe x  = Just x

logQErr :: ( MonadReader r m, Has (L.Logger L.Hasura) r, MonadIO m) => QErr -> m ()
logQErr err = do
  logger :: L.Logger L.Hasura <- asks getter
  L.unLogger logger $ EventInternalErr err

logHTTPErr
  :: ( MonadReader r m
     , Has (L.Logger L.Hasura) r
     , MonadIO m
     )
  => HTTPErr -> m ()
logHTTPErr err = do
  logger :: L.Logger L.Hasura <- asks getter
  L.unLogger logger $ err

-- These run concurrently on their respective EventPayloads
tryWebhook
  :: ( Has (L.Logger L.Hasura) r
     , Has HTTP.Manager r
     , MonadReader r m
     , MonadIO m
     , MonadError HTTPErr m
     )
  => [HTTP.Header] -> HTTP.ResponseTimeout -> EventPayload -> String
  -> m HTTPResp
tryWebhook headers responseTimeout ep webhook = do
  let context = ExtraContext (epCreatedAt ep) (epId ep)
  initReqE <- liftIO $ try $ HTTP.parseRequest webhook
  case initReqE of
    Left excp -> throwError $ HClient excp
    Right initReq -> do
      let req = initReq
                { HTTP.method = "POST"
                , HTTP.requestHeaders = headers
                , HTTP.requestBody = HTTP.RequestBodyLBS (encode ep)
                , HTTP.responseTimeout = responseTimeout
                }

      eitherResp <- runHTTP req (Just context)
      onLeft eitherResp throwError

getEventTriggerInfoFromEvent :: SchemaCache -> Event -> Maybe EventTriggerInfo
getEventTriggerInfoFromEvent sc e = let table = eTable e
                                        tableInfo = M.lookup table $ scTables sc
                                    in M.lookup ( tmName $ eTrigger e) =<< (_tiEventTriggerInfoMap <$> tableInfo)

---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

-- | Lock and return events not yet being processed or completed, up to some
-- limit. Process events approximately in created_at order, but we make no
-- ordering guarentees; events can and will race. Nevertheless we want to
-- ensure newer change events don't starve older ones.
fetchEvents :: Int -> Q.TxE QErr [Event]
fetchEvents limitI =
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = 't'
      WHERE id IN ( SELECT l.id
                    FROM hdb_catalog.event_log l
                    WHERE l.delivered = 'f' and l.error = 'f' and l.locked = 'f'
                          and (l.next_retry_at is NULL or l.next_retry_at <= now())
                          and l.archived = 'f'
                    ORDER BY created_at
                    LIMIT $1
                    FOR UPDATE SKIP LOCKED )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
      |] (Identity limit) True
  where uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
          Event
          { eId        = id'
          , eTable     = QualifiedObject sn tn
          , eTrigger   = TriggerMeta trn
          , eEvent     = payload
          , eTries     = tries
          , eCreatedAt = created
          }
        limit = fromIntegral limitI :: Word64

insertInvocation :: Invocation -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, request, response)
          VALUES ($1, $2, $3, $4)
          |] ( iEventId invo
             , toInt64 $ iStatus invo
             , Q.AltJ $ toJSON $ iRequest invo
             , Q.AltJ $ toJSON $ iResponse invo) True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

setSuccess :: Event -> Q.TxE QErr ()
setSuccess e = Q.unitQE defaultTxErrorHandler [Q.sql|
                        UPDATE hdb_catalog.event_log
                        SET delivered = 't', next_retry_at = NULL, locked = 'f'
                        WHERE id = $1
                        |] (Identity $ eId e) True

setError :: Event -> Q.TxE QErr ()
setError e = Q.unitQE defaultTxErrorHandler [Q.sql|
                        UPDATE hdb_catalog.event_log
                        SET error = 't', next_retry_at = NULL, locked = 'f'
                        WHERE id = $1
                        |] (Identity $ eId e) True

setRetry :: Event -> UTCTime -> Q.TxE QErr ()
setRetry e time =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET next_retry_at = $1, locked = 'f'
          WHERE id = $2
          |] (time, eId e) True

unlockAllEvents :: Q.TxE QErr ()
unlockAllEvents =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET locked = 'f'
          WHERE locked = 't'
          |] () False

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral
