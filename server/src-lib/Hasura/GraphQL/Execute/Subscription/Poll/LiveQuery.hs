{-# LANGUAGE TemplateHaskell #-}

-- | Multiplexed subscription poller threads; see "Hasura.GraphQL.Execute.Subscription" for details.
module Hasura.GraphQL.Execute.Subscription.Poll.LiveQuery
  ( -- * Pollers
    pollLiveQuery,
  )
where

import Control.Concurrent.Async qualified as A
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as Map
import Data.List.Split (chunksOf)
import Data.Monoid (Sum (..))
import Data.Text.Extended
import GHC.AssertNF.CPP
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.Poll.Common hiding (Cohort (..), CohortMap, CohortSnapshot (..))
import Hasura.GraphQL.Execute.Subscription.Poll.Common qualified as C
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.GraphQL.Execute.Subscription.Types
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (SourceName)
import Hasura.RQL.Types.Numeric qualified as Numeric
import Hasura.RQL.Types.Subscription (SubscriptionType (..))
import Hasura.Session

pushResultToCohort ::
  GQResult BS.ByteString ->
  Maybe ResponseHash ->
  SubscriptionMetadata ->
  CohortSnapshot 'LiveQuery ->
  -- | subscribers to which data has been pushed, subscribers which already
  -- have this data (this information is exposed by metrics reporting)
  IO ([SubscriberExecutionDetails], [SubscriberExecutionDetails])
pushResultToCohort result !respHashM (SubscriptionMetadata dTime) cohortSnapshot = do
  prevRespHashM <- STM.readTVarIO respRef
  -- write to the current websockets if needed
  (subscribersToPush, subscribersToIgnore) <-
    if isExecError result || respHashM /= prevRespHashM
      then do
        $assertNFHere respHashM -- so we don't write thunks to mutable vars
        STM.atomically $ do
          STM.writeTVar respRef respHashM
        return (newSinks <> curSinks, mempty)
      else return (newSinks, curSinks)
  pushResultToSubscribers subscribersToPush
  pure $
    over
      (each . each)
      ( \Subscriber {..} ->
          SubscriberExecutionDetails _sId _sMetadata
      )
      (subscribersToPush, subscribersToIgnore)
  where
    C.CohortSnapshot _ respRef curSinks newSinks = cohortSnapshot

    response = result <&> (`SubscriptionResponse` dTime)
    pushResultToSubscribers =
      A.mapConcurrently_ $ \Subscriber {..} -> _sOnChangeCallback response

-- | Where the magic happens: the top-level action run periodically by each
-- active 'Poller'. This needs to be async exception safe.
pollLiveQuery ::
  forall b.
  BackendTransport b =>
  PollerId ->
  SubscriptionsOptions ->
  (SourceName, SourceConfig b) ->
  RoleName ->
  ParameterizedQueryHash ->
  MultiplexedQuery b ->
  CohortMap 'LiveQuery ->
  SubscriptionPostPollHook ->
  IO ()
pollLiveQuery pollerId lqOpts (sourceName, sourceConfig) roleName parameterizedQueryHash query cohortMap postPollHook = do
  (totalTime, (snapshotTime, batchesDetails)) <- withElapsedTime $ do
    -- snapshot the current cohorts and split them into batches
    (snapshotTime, cohortBatches) <- withElapsedTime $ do
      -- get a snapshot of all the cohorts
      -- this need not be done in a transaction
      cohorts <- STM.atomically $ TMap.toList cohortMap
      cohortSnapshots <- mapM (STM.atomically . getCohortSnapshot) cohorts
      -- cohorts are broken down into batches specified by the batch size
      let cohortBatches = chunksOf (Numeric.getNonNegativeInt (unBatchSize batchSize)) cohortSnapshots
      -- associating every batch with their BatchId
      pure $ zip (BatchId <$> [1 ..]) cohortBatches

    -- concurrently process each batch
    batchesDetails <- A.forConcurrently cohortBatches $ \(batchId, cohorts) -> do
      (queryExecutionTime, mxRes) <- runDBSubscription @b sourceConfig query $ over (each . _2) C._csVariables cohorts

      let lqMeta = SubscriptionMetadata $ convertDuration queryExecutionTime
          operations = getCohortOperations cohorts mxRes
          -- batch response size is the sum of the response sizes of the cohorts
          batchResponseSize =
            case mxRes of
              Left _ -> Nothing
              Right resp -> Just $ getSum $ foldMap (Sum . BS.length . snd) resp
      (pushTime, cohortsExecutionDetails) <- withElapsedTime $
        A.forConcurrently operations $ \(res, cohortId, respData, snapshot) -> do
          (pushedSubscribers, ignoredSubscribers) <-
            pushResultToCohort res (fst <$> respData) lqMeta snapshot
          pure
            CohortExecutionDetails
              { _cedCohortId = cohortId,
                _cedVariables = C._csVariables snapshot,
                _cedPushedTo = pushedSubscribers,
                _cedIgnored = ignoredSubscribers,
                _cedResponseSize = snd <$> respData,
                _cedBatchId = batchId
              }
      pure $
        BatchExecutionDetails
          queryExecutionTime
          pushTime
          batchId
          cohortsExecutionDetails
          batchResponseSize

    pure (snapshotTime, batchesDetails)

  let pollDetails =
        PollDetails
          { _pdPollerId = pollerId,
            _pdGeneratedSql = toTxt query,
            _pdSnapshotTime = snapshotTime,
            _pdBatches = batchesDetails,
            _pdLiveQueryOptions = lqOpts,
            _pdTotalTime = totalTime,
            _pdSource = sourceName,
            _pdRole = roleName,
            _pdParameterizedQueryHash = parameterizedQueryHash
          }
  postPollHook pollDetails
  where
    SubscriptionsOptions batchSize _ = lqOpts

    getCohortSnapshot (cohortVars, handlerC) = do
      let C.Cohort resId respRef curOpsTV newOpsTV () = handlerC
      curOpsL <- TMap.toList curOpsTV
      newOpsL <- TMap.toList newOpsTV
      forM_ newOpsL $ \(k, action) -> TMap.insert action k curOpsTV
      TMap.reset newOpsTV

      let cohortSnapshot = C.CohortSnapshot cohortVars respRef (map snd curOpsL) (map snd newOpsL)
      return (resId, cohortSnapshot)

    getCohortOperations cohorts = \case
      Left e ->
        -- TODO: this is internal error
        let resp = throwError $ GQExecError [encodeGQLErr False e]
         in [(resp, cohortId, Nothing, snapshot) | (cohortId, snapshot) <- cohorts]
      Right responses -> do
        let cohortSnapshotMap = Map.fromList cohorts
        flip mapMaybe responses $ \(cohortId, respBS) ->
          let respHash = mkRespHash respBS
              respSize = BS.length respBS
           in -- TODO: currently we ignore the cases when the cohortId from
              -- Postgres response is not present in the cohort map of this batch
              -- (this shouldn't happen but if it happens it means a logic error and
              -- we should log it)
              (pure respBS,cohortId,Just (respHash, respSize),)
                <$> Map.lookup cohortId cohortSnapshotMap
