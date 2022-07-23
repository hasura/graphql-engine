{-# LANGUAGE TemplateHaskell #-}

-- | Top-level management of subscription poller threads.
-- The implementation of the polling itself is
-- in "Hasura.GraphQL.Execute.Subscription.Poll". See "Hasura.GraphQL.Execute.Subscription" for high-level
-- details.
module Hasura.GraphQL.Execute.Subscription.State
  ( SubscriptionsState (..),
    initSubscriptionsState,
    dumpSubscriptionsState,
    SubscriberDetails,
    SubscriptionPostPollHook,
    addLiveQuery,
    addStreamSubscriptionQuery,
    removeLiveQuery,
    removeStreamingQuery,
    LiveAsyncActionQueryOnSource (..),
    LiveAsyncActionQueryWithNoRelationships (..),
    LiveAsyncActionQuery (..),
    AsyncActionQueryLive (..),
    AsyncActionSubscriptionState,
    addAsyncActionLiveQuery,
    removeAsyncActionLiveQuery,
    LiveQuerySubscriberDetails,
    StreamingSubscriberDetails,
  )
where

import Control.Concurrent.Extended (forkImmortal, sleep)
import Control.Concurrent.STM qualified as STM
import Control.Exception (mask_)
import Control.Immortal qualified as Immortal
import Data.Aeson.Extended qualified as J
import Data.String
import Data.Text.Extended
import Data.UUID.V4 qualified as UUID
import GHC.AssertNF.CPP
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Execute.Subscription.Poll
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol (OperationName)
import Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Common (SourceName, unNonNegativeDiffTime)
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Prometheus (PrometheusMetrics (..))
import Hasura.Server.Types (RequestId)
import Language.GraphQL.Draft.Syntax qualified as G
import StmContainers.Map qualified as STMMap
import System.Metrics.Gauge qualified as EKG.Gauge
import System.Metrics.Prometheus.Gauge qualified as Prometheus.Gauge

-- | The top-level datatype that holds the state for all active subscriptions.
--
-- NOTE!: This must be kept consistent with a websocket connection's
-- 'OperationMap', in 'onClose' and 'onStart'.
data SubscriptionsState = SubscriptionsState
  { _ssLiveQueryOptions :: LiveQueriesOptions,
    _ssStreamQueryOptions :: StreamQueriesOptions,
    _ssLiveQueryMap :: PollerMap (),
    _ssStreamQueryMap :: PollerMap (STM.TVar CursorVariableValues),
    -- | A hook function which is run after each fetch cycle
    _ssPostPollHook :: SubscriptionPostPollHook,
    _ssAsyncActions :: AsyncActionSubscriptionState
  }

initSubscriptionsState ::
  LiveQueriesOptions -> StreamQueriesOptions -> SubscriptionPostPollHook -> IO SubscriptionsState
initSubscriptionsState liveQOptions streamQOptions pollHook =
  STM.atomically $
    SubscriptionsState liveQOptions <$> pure streamQOptions <*> STMMap.new <*> STMMap.new <*> pure pollHook <*> TMap.new

dumpSubscriptionsState :: Bool -> SubscriptionsState -> IO J.Value
dumpSubscriptionsState extended (SubscriptionsState liveQOpts streamQOpts lqMap streamMap _ _) = do
  lqMapJ <- dumpPollerMap extended lqMap
  streamMapJ <- dumpPollerMap extended streamMap
  return $
    J.object
      [ "options" J..= liveQOpts,
        "live_queries_map" J..= lqMapJ,
        "stream_queries_map" J..= streamMapJ,
        "stream_queries_options" J..= streamQOpts
      ]

-- | SubscriberDetails contains the data required to locate a subscriber
--   in the correct cohort within the correct poller in the operation map.
data SubscriberDetails a = SubscriberDetails
  { _sdPoller :: !PollerKey,
    _sdCohort :: !a,
    _sdSubscriber :: !SubscriberId
  }
  deriving (Show)

type LiveQuerySubscriberDetails = SubscriberDetails CohortKey

-- | The `CohortKey` contains the variables with which the subscription was started
--   and which will remain unchanged. The second type contains the mutable reference
--   through which we can get the latest value of the cursor and using both the `CohortKey`
--   and the latest cursor value, we locate the subscriber in the operation map to find its
--   details and then stop it.
type StreamingSubscriberDetails = SubscriberDetails (CohortKey, STM.TVar CursorVariableValues)

-- | `findPollerForSubscriber` places a subscriber in the correct poller.
--   If the poller doesn't exist then we create one otherwise we return the
--   existing one.
findPollerForSubscriber ::
  Subscriber ->
  CohortId ->
  PollerMap streamCursorVars ->
  PollerKey ->
  CohortKey ->
  (Subscriber -> Cohort streamCursorVars -> STM.STM streamCursorVars) ->
  (Subscriber -> CohortId -> Poller streamCursorVars -> STM.STM streamCursorVars) ->
  STM.STM ((Maybe (Poller streamCursorVars)), streamCursorVars)
findPollerForSubscriber subscriber cohortId pollerMap pollerKey cohortKey addToCohort addToPoller =
  -- a handler is returned only when it is newly created
  STMMap.lookup pollerKey pollerMap >>= \case
    Just poller -> do
      -- Found a poller, now check if a cohort also exists
      cursorVars <-
        TMap.lookup cohortKey (_pCohorts poller) >>= \case
          -- cohort found too! Simply add the subscriber to the cohort
          Just cohort -> addToCohort subscriber cohort
          -- cohort not found. Create a cohort with the subscriber and add
          -- the cohort to the poller
          Nothing -> addToPoller subscriber cohortId poller
      return (Nothing, cursorVars)
    Nothing -> do
      -- no poller found, so create one with the cohort
      -- and the subscriber within it.
      !poller <- Poller <$> TMap.new <*> STM.newEmptyTMVar
      cursorVars <- addToPoller subscriber cohortId poller
      STMMap.insert poller pollerKey pollerMap
      return $ (Just poller, cursorVars)

-- | Fork a thread handling a regular (live query) subscription
addLiveQuery ::
  forall b.
  BackendTransport b =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriberMetadata ->
  SubscriptionsState ->
  SourceName ->
  ParameterizedQueryHash ->
  -- | operation name of the query
  Maybe OperationName ->
  RequestId ->
  SubscriptionQueryPlan b (MultiplexedQuery b) ->
  -- | the action to be executed when result changes
  OnChange ->
  IO LiveQuerySubscriberDetails
addLiveQuery
  logger
  serverMetrics
  prometheusMetrics
  subscriberMetadata
  subscriptionState
  source
  parameterizedQueryHash
  operationName
  requestId
  plan
  onResultAction = do
    -- CAREFUL!: It's absolutely crucial that we can't throw any exceptions here!

    -- disposable UUIDs:
    cohortId <- newCohortId
    subscriberId <- newSubscriberId

    let !subscriber = Subscriber subscriberId subscriberMetadata requestId operationName onResultAction

    $assertNFHere subscriber -- so we don't write thunks to mutable vars
    (pollerMaybe, ()) <-
      STM.atomically $
        findPollerForSubscriber
          subscriber
          cohortId
          lqMap
          handlerId
          cohortKey
          addToCohort
          addToPoller

    -- we can then attach a polling thread if it is new the livequery can only be
    -- cancelled after putTMVar
    onJust pollerMaybe $ \poller -> do
      pollerId <- PollerId <$> UUID.nextRandom
      threadRef <- forkImmortal ("pollLiveQuery." <> show pollerId) logger $
        forever $ do
          pollLiveQuery @b pollerId lqOpts (source, sourceConfig) role parameterizedQueryHash query (_pCohorts poller) postPollHook
          sleep $ unNonNegativeDiffTime $ unRefetchInterval refetchInterval
      let !pState = PollerIOState threadRef pollerId
      $assertNFHere pState -- so we don't write thunks to mutable vars
      STM.atomically $ STM.putTMVar (_pIOState poller) pState

    liftIO $ EKG.Gauge.inc $ smActiveSubscriptions serverMetrics
    liftIO $ Prometheus.Gauge.inc $ pmActiveSubscriptions prometheusMetrics

    pure $ SubscriberDetails handlerId cohortKey subscriberId
    where
      SubscriptionsState lqOpts _ lqMap _ postPollHook _ = subscriptionState
      SubscriptionsOptions _ refetchInterval = lqOpts
      SubscriptionQueryPlan (ParameterizedSubscriptionQueryPlan role query) sourceConfig cohortKey _ = plan

      handlerId = PollerKey source role $ toTxt query

      addToCohort subscriber handlerC =
        TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers handlerC

      addToPoller subscriber cohortId handler = do
        !newCohort <-
          Cohort cohortId
            <$> STM.newTVar Nothing
            <*> TMap.new
            <*> TMap.new
            <*> pure ()
        addToCohort subscriber newCohort
        TMap.insert newCohort cohortKey $ _pCohorts handler

-- | Fork a thread handling a streaming subscription
addStreamSubscriptionQuery ::
  forall b.
  BackendTransport b =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriberMetadata ->
  SubscriptionsState ->
  SourceName ->
  ParameterizedQueryHash ->
  -- | operation name of the query
  Maybe OperationName ->
  RequestId ->
  -- | root field name
  G.Name ->
  SubscriptionQueryPlan b (MultiplexedQuery b) ->
  -- | the action to be executed when result changes
  OnChange ->
  IO StreamingSubscriberDetails
addStreamSubscriptionQuery
  logger
  serverMetrics
  prometheusMetrics
  subscriberMetadata
  subscriptionState
  source
  parameterizedQueryHash
  operationName
  requestId
  rootFieldName
  plan
  onResultAction = do
    -- CAREFUL!: It's absolutely crucial that we can't throw any exceptions here!

    -- disposable UUIDs:
    cohortId <- newCohortId
    subscriberId <- newSubscriberId

    let !subscriber = Subscriber subscriberId subscriberMetadata requestId operationName onResultAction

    $assertNFHere subscriber -- so we don't write thunks to mutable vars
    (handlerM, cohortCursorTVar) <-
      STM.atomically $
        findPollerForSubscriber
          subscriber
          cohortId
          streamQueryMap
          handlerId
          cohortKey
          addToCohort
          addToPoller

    -- we can then attach a polling thread if it is new the subscription can only be
    -- cancelled after putTMVar
    onJust handlerM $ \handler -> do
      pollerId <- PollerId <$> UUID.nextRandom
      threadRef <- forkImmortal ("pollStreamingQuery." <> show (unPollerId pollerId)) logger $
        forever $ do
          pollStreamingQuery @b pollerId streamQOpts (source, sourceConfig) role parameterizedQueryHash query (_pCohorts handler) rootFieldName postPollHook Nothing
          sleep $ unNonNegativeDiffTime $ unRefetchInterval refetchInterval
      let !pState = PollerIOState threadRef pollerId
      $assertNFHere pState -- so we don't write thunks to mutable vars
      STM.atomically $ STM.putTMVar (_pIOState handler) pState

    liftIO $ EKG.Gauge.inc $ smActiveSubscriptions serverMetrics
    liftIO $ Prometheus.Gauge.inc $ pmActiveSubscriptions prometheusMetrics

    pure $ SubscriberDetails handlerId (cohortKey, cohortCursorTVar) subscriberId
    where
      SubscriptionsState _ streamQOpts _ streamQueryMap postPollHook _ = subscriptionState
      SubscriptionsOptions _ refetchInterval = streamQOpts
      SubscriptionQueryPlan (ParameterizedSubscriptionQueryPlan role query) sourceConfig cohortKey _ = plan

      handlerId = PollerKey source role $ toTxt query

      addToCohort subscriber handlerC = do
        TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers handlerC
        pure $ _cStreamCursorVariables handlerC

      addToPoller subscriber cohortId handler = do
        latestCursorValues <-
          STM.newTVar (CursorVariableValues (_unValidatedVariables (_cvCursorVariables cohortKey)))
        !newCohort <- Cohort cohortId <$> STM.newTVar Nothing <*> TMap.new <*> TMap.new <*> pure latestCursorValues
        cohortCursorVals <- addToCohort subscriber newCohort
        TMap.insert newCohort cohortKey $ _pCohorts handler
        pure cohortCursorVals

removeLiveQuery ::
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriptionsState ->
  -- the query and the associated operation
  LiveQuerySubscriberDetails ->
  IO ()
removeLiveQuery logger serverMetrics prometheusMetrics lqState lqId@(SubscriberDetails handlerId cohortId sinkId) = mask_ $ do
  mbCleanupIO <- STM.atomically $ do
    detM <- getQueryDet lqMap
    fmap join $
      forM detM $ \(Poller cohorts ioState, cohort) ->
        cleanHandlerC cohorts ioState cohort
  sequence_ mbCleanupIO
  liftIO $ EKG.Gauge.dec $ smActiveSubscriptions serverMetrics
  liftIO $ Prometheus.Gauge.dec $ pmActiveSubscriptions prometheusMetrics
  where
    lqMap = _ssLiveQueryMap lqState

    getQueryDet subMap = do
      pollerM <- STMMap.lookup handlerId subMap
      fmap join $
        forM pollerM $ \poller -> do
          cohortM <- TMap.lookup cohortId (_pCohorts poller)
          return $ (poller,) <$> cohortM

    cleanHandlerC cohortMap ioState handlerC = do
      let curOps = _cExistingSubscribers handlerC
          newOps = _cNewSubscribers handlerC
      TMap.delete sinkId curOps
      TMap.delete sinkId newOps
      cohortIsEmpty <-
        (&&)
          <$> TMap.null curOps
          <*> TMap.null newOps
      when cohortIsEmpty $ TMap.delete cohortId cohortMap
      handlerIsEmpty <- TMap.null cohortMap
      -- when there is no need for handler i.e, this happens to be the last
      -- operation, take the ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId lqMap
          threadRefM <- fmap _pThread <$> STM.tryReadTMVar ioState
          return $
            Just $ -- deferred IO:
              case threadRefM of
                Just threadRef -> Immortal.stop threadRef
                -- This would seem to imply addLiveQuery broke or a bug
                -- elsewhere. Be paranoid and log:
                Nothing ->
                  L.unLogger logger $
                    L.UnstructuredLog L.LevelError $
                      fromString $
                        "In removeLiveQuery no worker thread installed. Please report this as a bug: "
                          <> show lqId
        else return Nothing

removeStreamingQuery ::
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriptionsState ->
  -- the query and the associated operation
  StreamingSubscriberDetails ->
  IO ()
removeStreamingQuery logger serverMetrics prometheusMetrics subscriptionState (SubscriberDetails handlerId (cohortId, cursorVariableTV) sinkId) = mask_ $ do
  mbCleanupIO <- STM.atomically $ do
    detM <- getQueryDet streamQMap
    fmap join $
      forM detM $ \(Poller cohorts ioState, currentCohortId, cohort) ->
        cleanHandlerC cohorts ioState (cohort, currentCohortId)
  sequence_ mbCleanupIO
  liftIO $ EKG.Gauge.dec $ smActiveSubscriptions serverMetrics
  liftIO $ Prometheus.Gauge.dec $ pmActiveSubscriptions prometheusMetrics
  where
    streamQMap = _ssStreamQueryMap subscriptionState

    getQueryDet subMap = do
      pollerM <- STMMap.lookup handlerId subMap
      (CursorVariableValues currentCohortCursorVal) <- STM.readTVar cursorVariableTV
      let updatedCohortId = modifyCursorCohortVariables (mkUnsafeValidateVariables currentCohortCursorVal) cohortId
      fmap join $
        forM pollerM $ \poller -> do
          cohortM <- TMap.lookup updatedCohortId (_pCohorts poller)
          return $ (poller,updatedCohortId,) <$> cohortM

    cleanHandlerC cohortMap ioState (handlerC, currentCohortId) = do
      let curOps = _cExistingSubscribers handlerC
          newOps = _cNewSubscribers handlerC
      TMap.delete sinkId curOps
      TMap.delete sinkId newOps
      cohortIsEmpty <-
        (&&)
          <$> TMap.null curOps
          <*> TMap.null newOps
      when cohortIsEmpty $ TMap.delete currentCohortId cohortMap
      handlerIsEmpty <- TMap.null cohortMap
      -- when there is no need for handler i.e,
      -- operation, take the ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId streamQMap
          threadRefM <- fmap _pThread <$> STM.tryReadTMVar ioState
          return $
            Just $ -- deferred IO:
              case threadRefM of
                Just threadRef -> Immortal.stop threadRef
                -- This would seem to imply addStreamSubscriptionQuery broke or a bug
                -- elsewhere. Be paranoid and log:
                Nothing ->
                  L.unLogger logger $
                    L.UnstructuredLog L.LevelError $
                      fromString $
                        "In removeLiveQuery no worker thread installed. Please report this as a bug: "
                          <> " poller_id: "
                          <> show handlerId
                          <> ", cohort_id: "
                          <> show cohortId
                          <> ", subscriber_id:"
                          <> show sinkId
        else return Nothing

-- | An async action query whose relationships are refered to table in a source.
-- We need to generate an SQL statement with the action response and execute it
-- in the source database so as to fetch response joined with relationship rows.
-- For more details see Note [Resolving async action query]
data LiveAsyncActionQueryOnSource = LiveAsyncActionQueryOnSource
  { _laaqpCurrentLqId :: !LiveQuerySubscriberDetails,
    _laaqpPrevActionLogMap :: !ActionLogResponseMap,
    -- | An IO action to restart the live query poller with updated action log responses fetched from metadata storage
    -- Restarting a live query re-generates the SQL statement with new action log responses to send latest action
    -- response to the client.
    _laaqpRestartLq :: !(LiveQuerySubscriberDetails -> ActionLogResponseMap -> IO (Maybe LiveQuerySubscriberDetails))
  }

data LiveAsyncActionQueryWithNoRelationships = LiveAsyncActionQueryWithNoRelationships
  { -- | An IO action to send response to the websocket client
    _laaqwnrSendResponse :: !(ActionLogResponseMap -> IO ()),
    -- | An IO action to send "completed" message to the websocket client
    _laaqwnrSendCompleted :: !(IO ())
  }

data LiveAsyncActionQuery
  = LAAQNoRelationships !LiveAsyncActionQueryWithNoRelationships
  | LAAQOnSourceDB !LiveAsyncActionQueryOnSource

data AsyncActionQueryLive = AsyncActionQueryLive
  { _aaqlActionIds :: !(NonEmpty ActionId),
    -- | An IO action to send error message (in case of any exception) to the websocket client
    _aaqlOnException :: !(QErr -> IO ()),
    _aaqlLiveExecution :: !LiveAsyncActionQuery
  }

-- | A share-able state map which stores an async action live query with it's subscription operation id
type AsyncActionSubscriptionState = TMap.TMap OperationId AsyncActionQueryLive

addAsyncActionLiveQuery ::
  AsyncActionSubscriptionState ->
  OperationId ->
  NonEmpty ActionId ->
  (QErr -> IO ()) ->
  LiveAsyncActionQuery ->
  IO ()
addAsyncActionLiveQuery queriesState opId actionIds onException liveQuery =
  STM.atomically $
    TMap.insert (AsyncActionQueryLive actionIds onException liveQuery) opId queriesState

removeAsyncActionLiveQuery ::
  AsyncActionSubscriptionState -> OperationId -> IO ()
removeAsyncActionLiveQuery queriesState opId =
  STM.atomically $ TMap.delete opId queriesState
