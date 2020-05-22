-- | Top-level management of live query poller threads. The implementation of the polling itself is
-- in "Hasura.GraphQL.Execute.LiveQuery.Poll". See "Hasura.GraphQL.Execute.LiveQuery" for high-level
-- details.
module Hasura.GraphQL.Execute.LiveQuery.State
  ( LiveQueriesState(..)
  , initLiveQueriesState
  , dumpLiveQueriesState

  , LiveQueryId
  , addLiveQuery
  , removeLiveQuery
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.STM                   as STM
import qualified Control.Immortal                         as Immortal
import qualified Data.Aeson.Extended                      as J
import qualified Data.UUID.V4                             as UUID
import qualified StmContainers.Map                        as STMMap

import           Control.Concurrent.Extended              (forkImmortal, sleep)
import           Control.Exception                        (mask_)
import           Data.String
import           GHC.AssertNF

import qualified Hasura.GraphQL.Execute.LiveQuery.TMap    as TMap
import qualified Hasura.Logging                           as L

import           Hasura.Db
import           Hasura.GraphQL.Execute.LiveQuery.Options
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.LiveQuery.Poll

-- | The top-level datatype that holds the state for all active live queries.
--
-- NOTE!: This must be kept consistent with a websocket connection's 'OperationMap', in 'onClose'
-- and 'onStart'.
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
  } deriving Show

addLiveQuery
  :: L.Logger L.Hasura
  -> UniqueSubscriberId
  -> LiveQueriesState
  -> LiveQueryPlan
  -> OnChange
  -- ^ the action to be executed when result changes
  -> IO LiveQueryId
addLiveQuery logger wsOpId lqState plan onResultAction = do
  -- CAREFUL!: It's absolutely crucial that we can't throw any exceptions here!

  -- disposable UUIDs:
  responseId <- newCohortId
  sinkId <- newSinkId

  $assertNFHere subscriber  -- so we don't write thunks to mutable vars

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
        !poller <- newPoller
        addToPoller sinkId responseId poller
        STMMap.insert poller handlerId lqMap
        return $ Just poller

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \handler -> do
    metrics <- initRefetchMetrics
    pollerId <- PollerId <$> UUID.nextRandom
    threadRef <- forkImmortal ("pollQuery." <> show sinkId) logger $ forever $ do
      pollQuery logger pollerId metrics lqOpts pgExecCtx query handler
      sleep $ unRefetchInterval refetchInterval
    let !pState = PollerIOState threadRef metrics
    $assertNFHere pState  -- so we don't write thunks to mutable vars
    STM.atomically $ STM.putTMVar (_pIOState handler) pState

  pure $ LiveQueryId handlerId cohortKey sinkId
  where
    LiveQueriesState lqOpts pgExecCtx lqMap = lqState
    LiveQueriesOptions _ refetchInterval = lqOpts
    LiveQueryPlan (ParameterizedLiveQueryPlan role query) cohortKey = plan

    handlerId = PollerKey role query

    !subscriber = Subscriber onResultAction wsOpId

    addToCohort sinkId handlerC =
      TMap.insert subscriber sinkId $ _cNewSubscribers handlerC

    addToPoller sinkId responseId handler = do
      !newCohort <- Cohort responseId <$> STM.newTVar Nothing <*> TMap.new <*> TMap.new
      addToCohort sinkId newCohort
      TMap.insert newCohort cohortKey $ _pCohorts handler

    newPoller = Poller <$> TMap.new <*> STM.newEmptyTMVar

removeLiveQuery
  :: L.Logger L.Hasura
  -> LiveQueriesState
  -- the query and the associated operation
  -> LiveQueryId
  -> IO ()
removeLiveQuery logger lqState lqId@(LiveQueryId handlerId cohortId sinkId) = mask_ $ do
  mbCleanupIO <- STM.atomically $ do
    detM <- getQueryDet
    fmap join $ forM detM $ \(Poller cohorts ioState, cohort) ->
      cleanHandlerC cohorts ioState cohort
  sequence_ mbCleanupIO
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
      -- when there is no need for handler i.e, this happens to be the last
      -- operation, take the ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId lqMap
          threadRefM <- fmap _pThread <$> STM.tryReadTMVar ioState
          return $ Just $ -- deferred IO:
            case threadRefM of
              Just threadRef -> Immortal.stop threadRef
              -- This would seem to imply addLiveQuery broke or a bug
              -- elsewhere. Be paranoid and log:
              Nothing -> L.unLogger logger $ L.UnstructuredLog L.LevelError $ fromString $
                "In removeLiveQuery no worker thread installed. Please report this as a bug: "<>
                show lqId
        else return Nothing
