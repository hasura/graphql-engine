-- | Top-level management of live query poller threads. The implementation of the polling itself is
-- in "Hasura.GraphQL.Execute.LiveQuery.Poll". See "Hasura.GraphQL.Execute.LiveQuery" for high-level
-- details.
module Hasura.GraphQL.Execute.LiveQuery.State
  ( LiveQueriesState (..),
    initLiveQueriesState,
    dumpLiveQueriesState,
    LiveQueryId,
    LiveQueryPostPollHook,
    addLiveQuery,
    addStreamSubscriptionQuery,
    removeLiveQuery,
    LiveAsyncActionQueryOnSource (..),
    LiveAsyncActionQueryWithNoRelationships (..),
    LiveAsyncActionQuery (..),
    AsyncActionQueryLive (..),
    AsyncActionSubscriptionState,
    addAsyncActionLiveQuery,
    removeAsyncActionLiveQuery,
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
import Hasura.GraphQL.Execute.LiveQuery.Options
import Hasura.GraphQL.Execute.LiveQuery.Plan
import Hasura.GraphQL.Execute.LiveQuery.Poll
import Hasura.GraphQL.Execute.LiveQuery.TMap qualified as TMap
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol (OperationName)
import Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types (SubscriptionType)
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Common (SourceName, SubscriptionType (..), unNonNegativeDiffTime)
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Types (RequestId)
import StmContainers.Map qualified as STMMap
import System.Metrics.Gauge qualified as EKG.Gauge

-- | The top-level datatype that holds the state for all active live queries.
--
-- NOTE!: This must be kept consistent with a websocket connection's
-- 'OperationMap', in 'onClose' and 'onStart'.
data LiveQueriesState = LiveQueriesState
  { _lqsOptions :: !LiveQueriesOptions,
    _lqsLiveQueryMap :: !(PollerMap ()),
    _lqsStreamQueryMap :: !(PollerMap (STM.TVar CursorVariableValues)),
    -- | A hook function which is run after each fetch cycle
    _lqsPostPollHook :: !LiveQueryPostPollHook,
    _lqsAsyncActions :: !AsyncActionSubscriptionState
  }

initLiveQueriesState ::
  LiveQueriesOptions -> LiveQueryPostPollHook -> IO LiveQueriesState
initLiveQueriesState options pollHook =
  STM.atomically $
    LiveQueriesState options <$> STMMap.new <*> STMMap.new <*> pure pollHook <*> TMap.new

dumpLiveQueriesState :: Bool -> LiveQueriesState -> IO J.Value
dumpLiveQueriesState extended (LiveQueriesState opts lqMap streamMap _ _) = do
  lqMapJ <- dumpPollerMap extended lqMap
  streamMapJ <- dumpPollerMap extended streamMap
  return $
    J.object
      [ "options" J..= opts,
        "live_queries_map" J..= lqMapJ,
        "stream_queries_map" J..= streamMapJ
      ]

data LiveQueryId = LiveQueryId
  { _lqiPoller :: !PollerKey,
    _lqiCohort :: !CohortKey,
    _lqiSubscriber :: !SubscriberId,
    _lqiSubscriptionType :: !SubscriptionType
  }
  deriving (Show)

addLiveQuery ::
  forall b.
  BackendTransport b =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  SubscriberMetadata ->
  LiveQueriesState ->
  SourceName ->
  ParameterizedQueryHash ->
  -- | operation name of the query
  Maybe OperationName ->
  RequestId ->
  LiveQueryPlan b (MultiplexedQuery b) ->
  -- | the action to be executed when result changes
  OnChange ->
  IO LiveQueryId
addLiveQuery
  logger
  serverMetrics
  subscriberMetadata
  lqState
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

    -- a handler is returned only when it is newly created
    handlerM <-
      STM.atomically $
        STMMap.lookup handlerId lqMap >>= \case
          Just handler -> do
            TMap.lookup cohortKey (_pCohorts handler) >>= \case
              Just cohort -> addToCohort subscriber cohort
              Nothing -> addToPoller subscriber cohortId handler
            return Nothing
          Nothing -> do
            !poller <- newPoller
            addToPoller subscriber cohortId poller
            STMMap.insert poller handlerId lqMap
            return $ Just poller

    -- we can then attach a polling thread if it is new the livequery can only be
    -- cancelled after putTMVar
    onJust handlerM $ \handler -> do
      pollerId <- PollerId <$> UUID.nextRandom
      threadRef <- forkImmortal ("pollQuery." <> show pollerId) logger $
        forever $ do
          pollQuery @b pollerId lqOpts (source, sourceConfig) role parameterizedQueryHash query (_pCohorts handler) postPollHook
          sleep $ unNonNegativeDiffTime $ unRefetchInterval refetchInterval
      let !pState = PollerIOState threadRef pollerId
      $assertNFHere pState -- so we don't write thunks to mutable vars
      STM.atomically $ STM.putTMVar (_pIOState handler) pState

    liftIO $ EKG.Gauge.inc $ smActiveSubscriptions serverMetrics

    pure $ LiveQueryId handlerId cohortKey subscriberId STLiveQuery
    where
      LiveQueriesState lqOpts lqMap _ postPollHook _ = lqState
      LiveQueriesOptions _ refetchInterval = lqOpts
      LiveQueryPlan (ParameterizedLiveQueryPlan role query) sourceConfig cohortKey _ _ = plan

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

      newPoller = Poller <$> TMap.new <*> STM.newEmptyTMVar

addStreamSubscriptionQuery ::
  forall b.
  BackendTransport b =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  SubscriberMetadata ->
  LiveQueriesState ->
  SourceName ->
  ParameterizedQueryHash ->
  -- | operation name of the query
  Maybe OperationName ->
  RequestId ->
  LiveQueryPlan b (MultiplexedQuery b) ->
  -- | the action to be executed when result changes
  OnChange ->
  IO LiveQueryId
addStreamSubscriptionQuery
  logger
  serverMetrics
  subscriberMetadata
  lqState
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

    -- a handler is returned only when it is newly created
    handlerM <-
      STM.atomically $
        STMMap.lookup handlerId streamQueryMap >>= \case
          Just handler -> do
            TMap.lookup cohortKey (_pCohorts handler) >>= \case
              Just cohort -> addToCohort subscriber cohort
              Nothing -> addToPoller subscriber cohortId handler
            return Nothing
          Nothing -> do
            !poller <- newPoller
            addToPoller subscriber cohortId poller
            STMMap.insert poller handlerId streamQueryMap
            return $ Just poller

    -- we can then attach a polling thread if it is new the livequery can only be
    -- cancelled after putTMVar
    onJust handlerM $ \handler -> do
      pollerId <- PollerId <$> UUID.nextRandom
      threadRef <- forkImmortal ("pollQuery." <> show pollerId) logger $
        forever $ do
          pollStreamingQuery @b pollerId lqOpts (source, sourceConfig) role parameterizedQueryHash query (_pCohorts handler) postPollHook
          sleep $ unNonNegativeDiffTime $ unRefetchInterval refetchInterval
      let !pState = PollerIOState threadRef pollerId
      $assertNFHere pState -- so we don't write thunks to mutable vars
      STM.atomically $ STM.putTMVar (_pIOState handler) pState

    liftIO $ EKG.Gauge.inc $ smActiveSubscriptions serverMetrics

    pure $ LiveQueryId handlerId cohortKey subscriberId STStreaming
    where
      LiveQueriesState lqOpts _ streamQueryMap postPollHook _ = lqState
      LiveQueriesOptions _ refetchInterval = lqOpts
      LiveQueryPlan (ParameterizedLiveQueryPlan role query) sourceConfig cohortKey _ _ = plan

      handlerId = PollerKey source role $ toTxt query

      addToCohort subscriber handlerC =
        TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers handlerC

      addToPoller subscriber cohortId handler = do
        latestCursorValues <-
          STM.newTVar (CursorVariableValues (_unValidatedVariables (_cvCursorVariables cohortKey)))
        !newCohort <- Cohort cohortId <$> STM.newTVar Nothing <*> TMap.new <*> TMap.new <*> pure latestCursorValues
        addToCohort subscriber newCohort
        TMap.insert newCohort cohortKey $ _pCohorts handler

      newPoller = Poller <$> TMap.new <*> STM.newEmptyTMVar

removeLiveQuery ::
  L.Logger L.Hasura ->
  ServerMetrics ->
  LiveQueriesState ->
  -- the query and the associated operation
  LiveQueryId ->
  IO ()
removeLiveQuery logger serverMetrics lqState lqId@(LiveQueryId handlerId cohortId sinkId subscriptionType) = mask_ $ do
  mbCleanupIO <- STM.atomically $ do
    case subscriptionType of
      STLiveQuery -> do
        detM <- getQueryDet lqMap
        fmap join $
          forM detM $ \(Poller cohorts ioState, cohort) ->
            cleanHandlerC cohorts ioState cohort
      STStreaming -> do
        detM <- getQueryDet streamQMap
        fmap join $
          forM detM $ \(Poller cohorts ioState, cohort) ->
            cleanHandlerC cohorts ioState cohort
  sequence_ mbCleanupIO
  liftIO $ EKG.Gauge.dec $ smActiveSubscriptions serverMetrics
  where
    lqMap = _lqsLiveQueryMap lqState

    streamQMap = _lqsStreamQueryMap lqState

    getQueryDet :: PollerMap a -> STM.STM (Maybe (Poller a, Cohort a))
    getQueryDet subMap = do
      pollerM <- STMMap.lookup handlerId subMap
      fmap join $
        forM pollerM $ \poller -> do
          cohortM <- TMap.lookup cohortId (_pCohorts poller)
          return $ (poller,) <$> cohortM

    cleanHandlerC :: TMap.TMap CohortKey (Cohort a) -> STM.TMVar PollerIOState -> Cohort a -> STM.STM (Maybe (IO ()))
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
      -- when there is no need for handler i.e,
      -- operation, take the ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          case subscriptionType of
            STLiveQuery -> STMMap.delete handlerId lqMap
            STStreaming -> STMMap.delete handlerId streamQMap
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

-- | An async action query whose relationships are refered to table in a source.
-- We need to generate an SQL statement with the action response and execute it
-- in the source database so as to fetch response joined with relationship rows.
-- For more details see Note [Resolving async action query]
data LiveAsyncActionQueryOnSource = LiveAsyncActionQueryOnSource
  { _laaqpCurrentLqId :: !LiveQueryId,
    _laaqpPrevActionLogMap :: !ActionLogResponseMap,
    -- | An IO action to restart the live query poller with updated action log responses fetched from metadata storage
    -- Restarting a live query re-generates the SQL statement with new action log responses to send latest action
    -- response to the client.
    _laaqpRestartLq :: !(LiveQueryId -> ActionLogResponseMap -> IO (Maybe LiveQueryId))
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
