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
import Data.Aeson.Ordered qualified as JO
import Data.Monoid (Endo)
import Data.String
import Data.Text.Extended
import Data.UUID.V4 qualified as UUID
import GHC.AssertNF.CPP
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Execute.Subscription.Poll
import Hasura.GraphQL.Execute.Subscription.Poll.Common (PollerResponseState (PRSError, PRSSuccess))
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol (OperationName)
import Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Common (SourceName)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Prometheus
  ( DynamicSubscriptionLabel (..),
    PrometheusMetrics (..),
    SubscriptionLabel (..),
    SubscriptionMetrics (..),
    liveQuerySubscriptionLabel,
    recordMetricWithLabel,
    streamingSubscriptionLabel,
  )
import Hasura.Server.Types (GranularPrometheusMetricsState (..), ModelInfoLogState, RequestId)
import Language.GraphQL.Draft.Syntax qualified as G
import Refined (unrefine)
import StmContainers.Map qualified as STMMap
import System.Metrics.Gauge qualified as EKG.Gauge
import System.Metrics.Prometheus.Gauge qualified as Prometheus.Gauge
import System.Metrics.Prometheus.GaugeVector qualified as GaugeVector

-- | The top-level datatype that holds the state for all active subscriptions.
--
-- NOTE!: This must be kept consistent with a websocket connection's
-- 'OperationMap', in 'onClose' and 'onStart'.
data SubscriptionsState = SubscriptionsState
  { _ssLiveQueryMap :: PollerMap (),
    _ssStreamQueryMap :: PollerMap (STM.TVar CursorVariableValues),
    -- | A hook function which is run after each fetch cycle
    _ssPostPollHook :: SubscriptionPostPollHook,
    _ssAsyncActions :: AsyncActionSubscriptionState
  }

initSubscriptionsState :: SubscriptionPostPollHook -> IO SubscriptionsState
initSubscriptionsState pollHook =
  STM.atomically
    $ SubscriptionsState
    <$> STMMap.new
    <*> STMMap.new
    <*> pure pollHook
    <*> TMap.new

-- | For dev debugging, output subject to change.
dumpSubscriptionsState :: Bool -> LiveQueriesOptions -> StreamQueriesOptions -> SubscriptionsState -> IO J.Value
dumpSubscriptionsState extended liveQOpts streamQOpts (SubscriptionsState lqMap streamMap _ _) = do
  lqMapJ <- dumpPollerMap extended lqMap
  streamMapJ <- dumpPollerMap extended streamMap
  return
    $ J.object
      [ "options" J..= liveQOpts,
        "live_queries_map" J..= lqMapJ,
        "stream_queries_map" J..= streamMapJ,
        "stream_queries_options" J..= streamQOpts
      ]

-- | SubscriberDetails contains the data required to locate a subscriber
--   in the correct cohort within the correct poller in the operation map.
data SubscriberDetails a = SubscriberDetails
  { _sdPoller :: BackendPollerKey,
    _sdCohort :: a,
    _sdSubscriber :: SubscriberId
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
  PollerMap streamCursorVars ->
  BackendPollerKey ->
  CohortKey ->
  (Subscriber -> Cohort streamCursorVars -> STM.STM streamCursorVars) ->
  (Subscriber -> Poller streamCursorVars -> STM.STM streamCursorVars) ->
  ParameterizedQueryHash ->
  Maybe OperationName ->
  STM.STM ((Maybe (Poller streamCursorVars)), streamCursorVars)
findPollerForSubscriber subscriber pollerMap pollerKey cohortKey addToCohort addToPoller parameterizedQueryHash maybeOperationName =
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
          Nothing -> addToPoller subscriber poller
      -- Add the operation name of the subcription to the poller, if it doesn't exist
      -- else increment the count for the operation name
      TMap.lookup maybeOperationName (_pOperationNamesMap poller) >>= \case
        Nothing -> TMap.insert 1 maybeOperationName (_pOperationNamesMap poller)
        Just _ -> TMap.adjust (+ 1) maybeOperationName (_pOperationNamesMap poller)
      return (Nothing, cursorVars)
    Nothing -> do
      -- no poller found, so create one with the cohort
      -- and the subscriber within it.
      operationNamesMap <- TMap.new
      TMap.insert 1 maybeOperationName operationNamesMap
      !poller <- Poller <$> TMap.new <*> STM.newTVar PRSSuccess <*> STM.newEmptyTMVar <*> pure parameterizedQueryHash <*> pure operationNamesMap
      cursorVars <- addToPoller subscriber poller
      STMMap.insert poller pollerKey pollerMap
      return $ (Just poller, cursorVars)

-- | Fork a thread handling a regular (live query) subscription
addLiveQuery ::
  forall b.
  (BackendTransport b) =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriberMetadata ->
  SubscriptionsState ->
  IO (LiveQueriesOptions, StreamQueriesOptions) ->
  SourceName ->
  ParameterizedQueryHash ->
  -- | operation name of the query
  Maybe OperationName ->
  RequestId ->
  SubscriptionQueryPlan b (MultiplexedQuery b) ->
  IO GranularPrometheusMetricsState ->
  -- | the action to be executed when result changes
  OnChange ->
  (Maybe (Endo JO.Value)) ->
  [ModelInfoPart] ->
  IO ModelInfoLogState ->
  IO LiveQuerySubscriberDetails
addLiveQuery
  logger
  serverMetrics
  prometheusMetrics
  subscriberMetadata
  subscriptionState
  getSubscriptionOptions
  source
  parameterizedQueryHash
  operationName
  requestId
  plan
  granularPrometheusMetricsState
  onResultAction
  modifier
  modelInfo
  modelInfoLogStatus = do
    -- CAREFUL!: It's absolutely crucial that we can't throw any exceptions here!

    -- disposable subscriber UUID:
    subscriberId <- newSubscriberId
    let !subscriber = Subscriber subscriberId subscriberMetadata requestId operationName onResultAction

    $assertNFHere subscriber -- so we don't write thunks to mutable vars
    (pollerMaybe, ()) <-
      STM.atomically
        $ findPollerForSubscriber
          subscriber
          lqMap
          handlerId
          cohortKey
          addToCohort
          addToPoller
          parameterizedQueryHash
          operationName

    -- we can then attach a polling thread if it is new the livequery can only be
    -- cancelled after putTMVar
    for_ pollerMaybe $ \poller -> do
      pollerId <- PollerId <$> UUID.nextRandom
      threadRef <- forkImmortal ("pollLiveQuery." <> show pollerId) logger
        $ forever
        $ do
          (lqOpts, _) <- getSubscriptionOptions
          let SubscriptionsOptions _ refetchInterval = lqOpts
          pollLiveQuery @b
            pollerId
            (_pPollerState poller)
            lqOpts
            (source, sourceConfig)
            role
            parameterizedQueryHash
            query
            (_pCohorts poller)
            postPollHook
            prometheusMetrics
            granularPrometheusMetricsState
            (_pOperationNamesMap poller)
            resolvedConnectionTemplate
            modifier
            logger
            modelInfo
            modelInfoLogStatus
          sleep $ unrefine $ unRefetchInterval refetchInterval
      let !pState = PollerIOState threadRef pollerId
      $assertNFHere pState -- so we don't write thunks to mutable vars
      STM.atomically $ STM.putTMVar (_pIOState poller) pState
      liftIO $ Prometheus.Gauge.inc $ submActiveLiveQueryPollers $ pmSubscriptionMetrics $ prometheusMetrics

    liftIO $ EKG.Gauge.inc $ smActiveSubscriptions serverMetrics
    let promMetricGranularLabel = SubscriptionLabel liveQuerySubscriptionLabel (Just $ DynamicSubscriptionLabel (Just parameterizedQueryHash) operationName)
        promMetricLabel = SubscriptionLabel liveQuerySubscriptionLabel Nothing
    let numSubscriptionMetric = submActiveSubscriptions $ pmSubscriptionMetrics $ prometheusMetrics
    recordMetricWithLabel
      granularPrometheusMetricsState
      True
      (GaugeVector.inc numSubscriptionMetric promMetricGranularLabel)
      (GaugeVector.inc numSubscriptionMetric promMetricLabel)
    liftIO $ EKG.Gauge.inc $ smActiveLiveQueries serverMetrics

    pure $ SubscriberDetails handlerId cohortKey subscriberId
    where
      SubscriptionsState lqMap _ postPollHook _ = subscriptionState
      SubscriptionQueryPlan (ParameterizedSubscriptionQueryPlan role query) sourceConfig cohortId resolvedConnectionTemplate cohortKey _ = plan

      handlerId = BackendPollerKey $ AB.mkAnyBackend @b $ PollerKey source role (toTxt query) resolvedConnectionTemplate parameterizedQueryHash

      addToCohort subscriber handlerC =
        TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers handlerC

      addToPoller subscriber handler = do
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
  (BackendTransport b) =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriberMetadata ->
  SubscriptionsState ->
  IO (LiveQueriesOptions, StreamQueriesOptions) ->
  SourceName ->
  ParameterizedQueryHash ->
  -- | operation name of the query
  Maybe OperationName ->
  RequestId ->
  -- | root field name
  G.Name ->
  SubscriptionQueryPlan b (MultiplexedQuery b) ->
  IO GranularPrometheusMetricsState ->
  -- | the action to be executed when result changes
  OnChange ->
  -- | the modifier for adding typename for namespaced queries
  (Maybe (Endo JO.Value)) ->
  [ModelInfoPart] ->
  IO ModelInfoLogState ->
  IO StreamingSubscriberDetails
addStreamSubscriptionQuery
  logger
  serverMetrics
  prometheusMetrics
  subscriberMetadata
  subscriptionState
  getSubscriptionOptions
  source
  parameterizedQueryHash
  operationName
  requestId
  rootFieldName
  plan
  granularPrometheusMetricsState
  onResultAction
  modifier
  modelInfo
  modelInfoLogStatus = do
    -- CAREFUL!: It's absolutely crucial that we can't throw any exceptions here!

    -- disposable subscriber UUID:
    subscriberId <- newSubscriberId
    let !subscriber = Subscriber subscriberId subscriberMetadata requestId operationName onResultAction

    $assertNFHere subscriber -- so we don't write thunks to mutable vars
    (handlerM, cohortCursorTVar) <-
      STM.atomically
        $ findPollerForSubscriber
          subscriber
          streamQueryMap
          handlerId
          cohortKey
          addToCohort
          addToPoller
          parameterizedQueryHash
          operationName

    -- we can then attach a polling thread if it is new the subscription can only be
    -- cancelled after putTMVar
    for_ handlerM $ \handler -> do
      pollerId <- PollerId <$> UUID.nextRandom
      threadRef <- forkImmortal ("pollStreamingQuery." <> show (unPollerId pollerId)) logger
        $ forever
        $ do
          (_, streamQOpts) <- getSubscriptionOptions
          let SubscriptionsOptions _ refetchInterval = streamQOpts
          pollStreamingQuery @b
            pollerId
            (_pPollerState handler)
            streamQOpts
            (source, sourceConfig)
            role
            parameterizedQueryHash
            query
            (_pCohorts handler)
            rootFieldName
            postPollHook
            Nothing
            prometheusMetrics
            granularPrometheusMetricsState
            (_pOperationNamesMap handler)
            resolvedConnectionTemplate
            modifier
            logger
            modelInfo
            modelInfoLogStatus
          sleep $ unrefine $ unRefetchInterval refetchInterval
      let !pState = PollerIOState threadRef pollerId
      $assertNFHere pState -- so we don't write thunks to mutable vars
      STM.atomically $ STM.putTMVar (_pIOState handler) pState
      liftIO $ Prometheus.Gauge.inc $ submActiveStreamingPollers $ pmSubscriptionMetrics $ prometheusMetrics

    liftIO $ do
      EKG.Gauge.inc $ smActiveSubscriptions serverMetrics
      EKG.Gauge.inc $ smActiveStreamingSubscriptions serverMetrics

    let promMetricGranularLabel = SubscriptionLabel streamingSubscriptionLabel (Just $ DynamicSubscriptionLabel (Just parameterizedQueryHash) operationName)
        promMetricLabel = SubscriptionLabel streamingSubscriptionLabel Nothing
        numSubscriptionMetric = submActiveSubscriptions $ pmSubscriptionMetrics $ prometheusMetrics
    recordMetricWithLabel
      granularPrometheusMetricsState
      True
      (GaugeVector.inc numSubscriptionMetric promMetricGranularLabel)
      (GaugeVector.inc numSubscriptionMetric promMetricLabel)

    pure $ SubscriberDetails handlerId (cohortKey, cohortCursorTVar) subscriberId
    where
      SubscriptionsState _ streamQueryMap postPollHook _ = subscriptionState
      SubscriptionQueryPlan (ParameterizedSubscriptionQueryPlan role query) sourceConfig cohortId resolvedConnectionTemplate cohortKey _ = plan

      handlerId = BackendPollerKey $ AB.mkAnyBackend @b $ PollerKey source role (toTxt query) resolvedConnectionTemplate parameterizedQueryHash

      addToCohort subscriber handlerC = do
        TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers handlerC
        pure $ _cStreamCursorVariables handlerC

      addToPoller subscriber handler = do
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
  IO GranularPrometheusMetricsState ->
  Maybe OperationName ->
  IO ()
removeLiveQuery logger serverMetrics prometheusMetrics lqState lqId@(SubscriberDetails handlerId cohortId sinkId) granularPrometheusMetricsState maybeOperationName = mask_ $ do
  join
    $ STM.atomically
    $ do
      detM <- getQueryDet lqMap
      case detM of
        Nothing -> return (pure ())
        Just (Poller cohorts pollerState ioState parameterizedQueryHash operationNamesMap, cohort) -> do
          TMap.lookup maybeOperationName operationNamesMap >>= \case
            -- If only one operation name is present in the map, delete it
            Just 1 -> TMap.delete maybeOperationName operationNamesMap
            -- If the count of a operation name is more than 1, then it means there
            -- are more subscriptions with the same name and we should keep emitting
            -- the metrics until the all the subscription with that operaion name are
            -- removed
            Just _ -> TMap.adjust (\v -> v - 1) maybeOperationName operationNamesMap
            Nothing -> return ()
          cleanHandlerC cohorts pollerState ioState cohort parameterizedQueryHash
  liftIO $ EKG.Gauge.dec $ smActiveSubscriptions serverMetrics
  liftIO $ EKG.Gauge.dec $ smActiveLiveQueries serverMetrics
  where
    lqMap = _ssLiveQueryMap lqState

    getQueryDet subMap = do
      pollerM <- STMMap.lookup handlerId subMap
      fmap join
        $ forM pollerM
        $ \poller -> do
          cohortM <- TMap.lookup cohortId (_pCohorts poller)
          return $ (poller,) <$> cohortM

    cleanHandlerC cohortMap pollerState ioState handlerC parameterizedQueryHash = do
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
      let promMetricGranularLabel = SubscriptionLabel liveQuerySubscriptionLabel (Just $ DynamicSubscriptionLabel (Just parameterizedQueryHash) maybeOperationName)
          promMetricLabel = SubscriptionLabel liveQuerySubscriptionLabel Nothing
      -- when there is no need for handler i.e, this happens to be the last
      -- operation, take the ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId lqMap
          threadRefM <- fmap _pThread <$> STM.tryReadTMVar ioState
          return
            $
            -- deferred IO:
            case threadRefM of
              Just threadRef -> do
                Immortal.stop threadRef
                liftIO $ do
                  pollerLastState <- STM.readTVarIO pollerState
                  when (pollerLastState == PRSError)
                    $ Prometheus.Gauge.dec
                    $ submActiveLiveQueryPollersInError
                    $ pmSubscriptionMetrics prometheusMetrics
                  Prometheus.Gauge.dec $ submActiveLiveQueryPollers $ pmSubscriptionMetrics prometheusMetrics
                  let numSubscriptionMetric = submActiveSubscriptions $ pmSubscriptionMetrics $ prometheusMetrics
                  recordMetricWithLabel
                    granularPrometheusMetricsState
                    True
                    (GaugeVector.dec numSubscriptionMetric promMetricGranularLabel)
                    (GaugeVector.dec numSubscriptionMetric promMetricLabel)
              -- This would seem to imply addLiveQuery broke or a bug
              -- elsewhere. Be paranoid and log:
              Nothing ->
                L.unLogger logger
                  $ L.UnstructuredLog L.LevelError
                  $ fromString
                  $ "In removeLiveQuery no worker thread installed. Please report this as a bug: "
                  <> show lqId
        else do
          let numSubscriptionMetric = submActiveSubscriptions $ pmSubscriptionMetrics $ prometheusMetrics
          return
            $ recordMetricWithLabel
              granularPrometheusMetricsState
              True
              (GaugeVector.dec numSubscriptionMetric promMetricGranularLabel)
              (GaugeVector.dec numSubscriptionMetric promMetricLabel)

removeStreamingQuery ::
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  SubscriptionsState ->
  -- the query and the associated operation
  StreamingSubscriberDetails ->
  IO GranularPrometheusMetricsState ->
  Maybe OperationName ->
  IO ()
removeStreamingQuery logger serverMetrics prometheusMetrics subscriptionState (SubscriberDetails handlerId (cohortId, cursorVariableTV) sinkId) granularPrometheusMetricsState maybeOperationName = mask_ $ do
  join
    $ STM.atomically
    $ do
      detM <- getQueryDet streamQMap
      case detM of
        Nothing -> return (pure ())
        Just (Poller cohorts pollerState ioState parameterizedQueryHash operationNamesMap, currentCohortId, cohort) -> do
          TMap.lookup maybeOperationName operationNamesMap >>= \case
            -- If only one operation name is present in the map, delete it
            Just 1 -> TMap.delete maybeOperationName operationNamesMap
            -- If the count of a operation name is more than 1, then it means there
            -- are more subscriptions with the same name and we should keep emitting
            -- the metrics until the all the subscription with the operaion name are
            -- removed
            Just _ -> TMap.adjust (\v -> v - 1) maybeOperationName operationNamesMap
            Nothing -> return ()
          cleanHandlerC cohorts pollerState ioState (cohort, currentCohortId) parameterizedQueryHash
  liftIO $ do
    EKG.Gauge.dec $ smActiveSubscriptions serverMetrics
    EKG.Gauge.dec $ smActiveStreamingSubscriptions serverMetrics
  where
    streamQMap = _ssStreamQueryMap subscriptionState

    getQueryDet subMap = do
      pollerM <- STMMap.lookup handlerId subMap
      (CursorVariableValues currentCohortCursorVal) <- STM.readTVar cursorVariableTV
      let updatedCohortId = modifyCursorCohortVariables (mkUnsafeValidateVariables currentCohortCursorVal) cohortId
      fmap join
        $ forM pollerM
        $ \poller -> do
          cohortM <- TMap.lookup updatedCohortId (_pCohorts poller)
          return $ (poller,updatedCohortId,) <$> cohortM

    cleanHandlerC cohortMap pollerState ioState (handlerC, currentCohortId) parameterizedQueryHash = do
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
      let promMetricGranularLabel = SubscriptionLabel streamingSubscriptionLabel (Just $ DynamicSubscriptionLabel (Just parameterizedQueryHash) maybeOperationName)
          promMetricLabel = SubscriptionLabel streamingSubscriptionLabel Nothing
      -- when there is no need for handler i.e,
      -- operation, take the ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId streamQMap
          threadRefM <- fmap _pThread <$> STM.tryReadTMVar ioState
          return
            $
            -- deferred IO:
            case threadRefM of
              Just threadRef -> do
                Immortal.stop threadRef
                liftIO $ do
                  pollerLastState <- STM.readTVarIO pollerState
                  when (pollerLastState == PRSError)
                    $ Prometheus.Gauge.dec
                    $ submActiveStreamingPollersInError
                    $ pmSubscriptionMetrics prometheusMetrics
                  Prometheus.Gauge.dec $ submActiveStreamingPollers $ pmSubscriptionMetrics prometheusMetrics
                  let numSubscriptionMetric = submActiveSubscriptions $ pmSubscriptionMetrics $ prometheusMetrics
                  recordMetricWithLabel
                    granularPrometheusMetricsState
                    True
                    (GaugeVector.dec numSubscriptionMetric promMetricGranularLabel)
                    (GaugeVector.dec numSubscriptionMetric promMetricLabel)
              -- This would seem to imply addStreamSubscriptionQuery broke or a bug
              -- elsewhere. Be paranoid and log:
              Nothing ->
                L.unLogger logger
                  $ L.UnstructuredLog L.LevelError
                  $ fromString
                  $ "In removeStreamingQuery no worker thread installed. Please report this as a bug: "
                  <> " poller_id: "
                  <> show handlerId
                  <> ", cohort_id: "
                  <> show cohortId
                  <> ", subscriber_id:"
                  <> show sinkId
        else do
          let numSubscriptionMetric = submActiveSubscriptions $ pmSubscriptionMetrics $ prometheusMetrics
          return
            $ recordMetricWithLabel
              granularPrometheusMetricsState
              True
              (GaugeVector.dec numSubscriptionMetric promMetricGranularLabel)
              (GaugeVector.dec numSubscriptionMetric promMetricLabel)

-- | An async action query whose relationships are referred to table in a source.
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
  = LAAQNoRelationships LiveAsyncActionQueryWithNoRelationships
  | LAAQOnSourceDB LiveAsyncActionQueryOnSource

data AsyncActionQueryLive = AsyncActionQueryLive
  { _aaqlActionIds :: NonEmpty ActionId,
    -- | An IO action to send error message (in case of any exception) to the websocket client
    _aaqlOnException :: (QErr -> IO ()),
    _aaqlLiveExecution :: LiveAsyncActionQuery
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
  STM.atomically
    $ TMap.insert (AsyncActionQueryLive actionIds onException liveQuery) opId queriesState

removeAsyncActionLiveQuery ::
  AsyncActionSubscriptionState -> OperationId -> IO ()
removeAsyncActionLiveQuery queriesState opId =
  STM.atomically $ TMap.delete opId queriesState
