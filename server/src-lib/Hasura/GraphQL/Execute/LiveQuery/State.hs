-- | Top-level management of live query poller threads. The implementation of the polling itself is
-- in "Hasura.GraphQL.Execute.LiveQuery.Poll". See "Hasura.GraphQL.Execute.LiveQuery" for high-level
-- details.
module Hasura.GraphQL.Execute.LiveQuery.State
  ( LiveQueriesState
  , initLiveQueriesState
  , dumpLiveQueriesState

  , LiveQueryId
  , addLiveQuery
  , removeLiveQuery
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async                 as A
import qualified Control.Concurrent.STM                   as STM
import qualified Data.Aeson.Extended                      as J
import qualified StmContainers.Map                        as STMMap

import           Control.Concurrent.Extended              (sleep)

import qualified Hasura.GraphQL.Execute.LiveQuery.TMap    as TMap

import           Hasura.Db
import           Hasura.GraphQL.Execute.LiveQuery.Options
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.LiveQuery.Poll

-- | The top-level datatype that holds the state for all active live queries.
data LiveQueriesState
  = LiveQueriesState
  { _lqsOptions      :: !LiveQueriesOptions
  , _lqsPGExecTx     :: !PGExecCtx
  , _lqsLiveQueryMap :: !PollerMap
  }

initLiveQueriesState :: LiveQueriesOptions -> PGExecCtx -> IO LiveQueriesState
initLiveQueriesState options pgCtx = LiveQueriesState options pgCtx <$> STMMap.newIO

dumpLiveQueriesState :: Bool -> LiveQueriesState -> IO J.Value
dumpLiveQueriesState extended (LiveQueriesState opts _ lqMap) = do
  lqMapJ <- dumpPollerMap extended lqMap
  return $ J.object
    [ "options" J..= opts
    , "live_queries_map" J..= lqMapJ
    ]

data LiveQueryId
  = LiveQueryId
  { _lqiPoller     :: !PollerKey
  , _lqiCohort     :: !CohortKey
  , _lqiSubscriber :: !SubscriberId
  }

addLiveQuery
  :: LiveQueriesState
  -> LiveQueryPlan
  -> OnChange
  -- ^ the action to be executed when result changes
  -> IO LiveQueryId
addLiveQuery lqState plan onResultAction = do
  responseId <- newCohortId
  sinkId <- newSinkId

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $ do
    handlerM <- STMMap.lookup handlerId lqMap
    case handlerM of
      Just handler -> do
        cohortM <- TMap.lookup cohortKey $ _pCohorts handler
        case cohortM of
          Just cohort -> addToCohort sinkId cohort
          Nothing     -> addToPoller sinkId responseId handler
        return Nothing
      Nothing -> do
        poller <- newPoller
        addToPoller sinkId responseId poller
        STMMap.insert poller handlerId lqMap
        return $ Just poller

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \handler -> do
    metrics <- initRefetchMetrics
    threadRef <- A.async $ forever $ do
      pollQuery metrics batchSize pgExecCtx query handler
      sleep $ unRefetchInterval refetchInterval
    STM.atomically $ STM.putTMVar (_pIOState handler) (PollerIOState threadRef metrics)

  pure $ LiveQueryId handlerId cohortKey sinkId
  where
    LiveQueriesState lqOpts pgExecCtx lqMap = lqState
    LiveQueriesOptions batchSize refetchInterval = lqOpts
    LiveQueryPlan (ParameterizedLiveQueryPlan role alias query) cohortKey = plan

    handlerId = PollerKey role query

    addToCohort sinkId handlerC =
      TMap.insert (Subscriber alias onResultAction) sinkId $ _cNewSubscribers handlerC

    addToPoller sinkId responseId handler = do
      newCohort <- Cohort responseId <$> STM.newTVar Nothing <*> TMap.new <*> TMap.new
      addToCohort sinkId newCohort
      TMap.insert newCohort cohortKey $ _pCohorts handler

    newPoller = Poller <$> TMap.new <*> STM.newEmptyTMVar

removeLiveQuery
  :: LiveQueriesState
  -- the query and the associated operation
  -> LiveQueryId
  -> IO ()
removeLiveQuery lqState (LiveQueryId handlerId cohortId sinkId) = do
  threadRef <- STM.atomically $ do
    detM <- getQueryDet
    fmap join $ forM detM $ \(Poller cohorts ioState, cohort) ->
      cleanHandlerC cohorts ioState cohort
  traverse_ A.cancel threadRef
  where
    lqMap = _lqsLiveQueryMap lqState

    getQueryDet = do
      pollerM <- STMMap.lookup handlerId lqMap
      fmap join $ forM pollerM $ \poller -> do
        cohortM <- TMap.lookup cohortId (_pCohorts poller)
        return $ (poller,) <$> cohortM

    cleanHandlerC cohortMap ioState handlerC = do
      let curOps = _cExistingSubscribers handlerC
          newOps = _cNewSubscribers handlerC
      TMap.delete sinkId curOps
      TMap.delete sinkId newOps
      cohortIsEmpty <- (&&)
        <$> TMap.null curOps
        <*> TMap.null newOps
      when cohortIsEmpty $ TMap.delete cohortId cohortMap
      handlerIsEmpty <- TMap.null cohortMap
      -- when there is no need for handler
      -- i.e, this happens to be the last operation, take the
      -- ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId lqMap
          fmap _pThread <$> STM.tryReadTMVar ioState
        else return Nothing
