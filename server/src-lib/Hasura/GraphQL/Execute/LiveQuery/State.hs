{-# LANGUAGE CPP #-}
-- | Top-level management of live query poller threads. The implementation of the polling itself is
-- in "Hasura.GraphQL.Execute.LiveQuery.Poll". See "Hasura.GraphQL.Execute.LiveQuery" for high-level
-- details.
module Hasura.GraphQL.Execute.LiveQuery.State
  ( LiveQueriesState(..)
  , initLiveQueriesState
  , dumpLiveQueriesState

  , LiveQueryId
  , LiveQueryPostPollHook
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
#ifndef PROFILING
import           GHC.AssertNF
#endif

import qualified Hasura.GraphQL.Execute.LiveQuery.TMap    as TMap
import qualified Hasura.Logging                           as L

import           Hasura.GraphQL.Execute.LiveQuery.Options
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.LiveQuery.Poll
import           Hasura.RQL.Types.Common                  (unNonNegativeDiffTime)

-- | The top-level datatype that holds the state for all active live queries.
--
-- NOTE!: This must be kept consistent with a websocket connection's
-- 'OperationMap', in 'onClose' and 'onStart'.
data LiveQueriesState
  = LiveQueriesState
  { _lqsOptions      :: !LiveQueriesOptions
  , _lqsLiveQueryMap :: !PollerMap
  , _lqsPostPollHook :: !LiveQueryPostPollHook
  -- ^ A hook function which is run after each fetch cycle
  }

initLiveQueriesState
  :: LiveQueriesOptions -> LiveQueryPostPollHook -> IO LiveQueriesState
initLiveQueriesState options pollHook =
  LiveQueriesState options <$> STMMap.newIO <*> pure pollHook

dumpLiveQueriesState :: Bool -> LiveQueriesState -> IO J.Value
dumpLiveQueriesState extended (LiveQueriesState opts lqMap _) = do
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
  -> SubscriberMetadata
  -> LiveQueriesState
  -> LiveQueryPlan
  -> OnChange
  -- ^ the action to be executed when result changes
  -> IO LiveQueryId
addLiveQuery logger subscriberMetadata lqState plan onResultAction = do
  -- CAREFUL!: It's absolutely crucial that we can't throw any exceptions here!

  -- disposable UUIDs:
  cohortId <- newCohortId
  subscriberId <- newSubscriberId

  let !subscriber = Subscriber subscriberId subscriberMetadata onResultAction

#ifndef PROFILING
  $assertNFHere subscriber  -- so we don't write thunks to mutable vars
#endif

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $
    STMMap.lookup handlerId lqMap >>= \case
      Just handler -> do
        TMap.lookup cohortKey (_pCohorts handler) >>= \case
          Just cohort -> addToCohort subscriber cohort
          Nothing     -> addToPoller subscriber cohortId handler
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
    threadRef <- forkImmortal ("pollQuery." <> show pollerId) logger $ forever $ do
      pollQuery pollerId lqOpts pgExecCtx query (_pCohorts handler) postPollHook
      sleep $ unNonNegativeDiffTime $ unRefetchInterval refetchInterval
    let !pState = PollerIOState threadRef pollerId
#ifndef PROFILING
    $assertNFHere pState  -- so we don't write thunks to mutable vars
#endif
    STM.atomically $ STM.putTMVar (_pIOState handler) pState

  pure $ LiveQueryId handlerId cohortKey subscriberId
  where
    LiveQueriesState lqOpts lqMap postPollHook = lqState
    LiveQueriesOptions _ refetchInterval = lqOpts
    LiveQueryPlan (ParameterizedLiveQueryPlan role query) cohortKey pgExecCtx = plan

    handlerId = PollerKey role query

    addToCohort subscriber handlerC =
      TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers handlerC

    addToPoller subscriber cohortId handler = do
      !newCohort <- Cohort cohortId <$> STM.newTVar Nothing <*> TMap.new <*> TMap.new
      addToCohort subscriber newCohort
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
