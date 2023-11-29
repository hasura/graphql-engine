{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.Execute.Subscription.Poll.StreamingQuery
  ( -- * Pollers
    pollStreamingQuery,
  )
where

import Control.Concurrent.Async qualified as A
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Data.Aeson.Ordered qualified as JO
import Data.ByteString qualified as BS
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as Set
import Data.List.Split (chunksOf)
import Data.Monoid (Endo (..), Sum (..))
import Data.Text.Extended
import GHC.AssertNF.CPP
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Execute.Subscription.Plan qualified as C
import Hasura.GraphQL.Execute.Subscription.Poll.Common hiding (Cohort (..), CohortMap, CohortSnapshot (..))
import Hasura.GraphQL.Execute.Subscription.Poll.Common qualified as C
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.GraphQL.Execute.Subscription.Types
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging (Hasura, LogLevel (..), Logger (unLogger))
import Hasura.Prelude
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendTag, reify)
import Hasura.RQL.Types.BackendType (BackendType (..), PostgresKind (Vanilla))
import Hasura.RQL.Types.Common (SourceName)
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Subscription (SubscriptionType (..))
import Hasura.SQL.Value (TxtEncodedVal (..))
import Hasura.Server.Logging (ModelInfo (..), ModelInfoLog (..))
import Hasura.Server.Prometheus (PrometheusMetrics (..), SubscriptionMetrics (..), recordSubcriptionMetric, streamingSubscriptionLabel)
import Hasura.Server.Types (GranularPrometheusMetricsState (..), ModelInfoLogState (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Refined (unrefine)
import System.Metrics.Prometheus.Gauge qualified as Prometheus.Gauge
import System.Metrics.Prometheus.HistogramVector qualified as HistogramVector
import Text.Shakespeare.Text (st)

{- Note [Streaming subscriptions rebuilding cohort map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When the multiplexed query is polled, the cohort is snapshotted to get the
existing and newly added subscribers (if any), let's call these subscribers as
current poll subscribers.

Every cohort is associated with a cohort key and the cohort key is basically the
variables the cohort uses. The cohort variables contains multiple types of
variables in it, session variables, query variables, synthetic variables and
cursor variables, out of all these, the cursor variable may change with every
poll. So, we rebuild the cohort map at the end of every poll, see Note
[Streaming subscription polling]. But, rebuilding the cohort map is not straight
forward because there are race conditions that need to be taken care of. Some of
the race conditions which can happen when the current cohorts are processing:

1. A cohort is removed concurrently
2. A subscriber is removed concurrently
3. A new subscriber has started a subscription and it should be placed in the correct cohort

In the current code, the rebuilding of the cohort map goes as the follows:

1. After snapshotting the cohorts, we build a cohort map out of those cohorts,
   in the code these are called as "processedCohorts", it's important to note
   that these are not retrieved from the mutable "CohortMap", these are the
   snapshotted cohorts which were processed in the current poll. The reason we
   maintain the snapshotted cohorts is because it is later used while rebuilding
   the cohort map.

2. We create a processed cohort map which looks like HashMap CohortKey (Cohort
   'Streaming, CohortKey). The key of the map is the CohortKey which was
   associated with the cohort during the poll and the CohortKey in the value
   type is the cohort key after updating the cursor value. Note that both the
   values may or may not be equal.

3. We atomically get the list of the cohorts from the cohort map (mutable
reference), let's call it current cohorts and then traverse over it.

   1. Lookup with the given cohort key into the processed cohort map

      a. If no cohort is found, it means that the cohort with the given cohort
         key has been added while we were polling. So, we keep this as it is.

      b. If a processed cohort is found:

         i. We have to see if any new subscribers have been added to the current
            cohort, this is calculated using the diff of existing subscribers in
            the current cohort and the existing cohort, if there are any then we
            create a new cohort which includes only the newly added subscribers
            and add the new cohort into the cohort map.

         ii. We only retain the subscribers found in the processed cohort which
             exist in the current cohort. We do this because it is possible that
             a subscriber has been stopped their subscription in the time
             between the cohorts were snapshotted for processing and the time
             the cohort map is rebuilt.

         iii. Insert the processed cohort with the updated cohort key into the
         cohort map.
-}

{- Note [Streaming subscription polling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every cohort of a streaming subscription is associated with a mutable latest
cursor value reference, which contains the current value of the cursor.

After a poll, the mutable cursor value gets updated if its value is a non-null
one, null value means that there were no rows to get the min/max value from.

After this, the cohort map associated with the poller is also rebuilt, which
will make sure that the cohort map's key contains the latest cursor values. So,
any new subscriber will get added to an existing cohort if the new subscriber's
cohort key matches with any of the existing cohorts. We *need* to rebuild the
cohort map, because, say if we don't rebuild the cohort map then a new
subscriber may get added to a cohort which has been streaming for a while now,
then the new subscriber will get the responses according to the cursor value
stored in the cohort, instead of the initial value specified by the client. For
example:

Client 1 started streaming a query at t1, say:

```
   subscription {
      log_stream(cursor: {initial_value: {created_at: "2020-01-01"}}, batch_size: 1000) {
         id
         level
         detail
     }
   }
```

Client 2 starts streaming at t2 with the same query (where t2 > t1), if the
cohort map is not rebuilt (to reflect cursor value in the cohort key), then
client 2 and client 1 will both start getting the same responses, which is wrong
because client 2 should start streaming from the initial value it provided.

-}

{- Note [Lifecycle of a streaming subscription poller]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  +-------------------------------------------+           +-----------------------------------------------------------------+
  | Execute multiplexed query in the database | ------->  | Parse the response, every row of response contains three things:|
  +-------------------------------------------+           |  1. Cohort ID                                                   |
             ^                                            |  2. Cohort's Response                                           |
             |                                            |  3. Cohort's latest cursor value                                |
             |                                            +-----------------------------------------------------------------+
             |                                                                            |
             |                                                                            |
             |                                                                            v
             |                                            +------------------------------------------------------------------------+
             |                                            | Processing of the response:                                            |
             |                                            | 1. Send the result to the subscribers                                  |
   +------------------------------------+                 | 2. Update the cursor value in the mutable reference                    |
   | Rebuild the cohort map             |                 |    of the snapshot so that the next poll uses this value               |
   +------------------------------------+        <------  +------------------------------------------------------------------------+

-}

mergeOldAndNewCursorValues :: CursorVariableValues -> CursorVariableValues -> CursorVariableValues
mergeOldAndNewCursorValues (CursorVariableValues prevPollCursorValues) (CursorVariableValues currentPollCursorValues) =
  let combineFn previousVal currentVal =
        case currentVal of
          TENull -> previousVal -- When we get a null value from the DB, we retain the previous value
          TELit t -> TELit t
   in CursorVariableValues $ HashMap.unionWith combineFn prevPollCursorValues currentPollCursorValues

pushResultToCohort ::
  GQResult BS.ByteString ->
  Maybe ResponseHash ->
  SubscriptionMetadata ->
  CursorVariableValues ->
  -- | Root field name
  G.Name ->
  -- | subscribers to which data has been pushed, subscribers which already
  -- have this data (this information is exposed by metrics reporting)
  (CohortSnapshot 'Streaming, Cohort 'Streaming) ->
  IO ([SubscriberExecutionDetails], [SubscriberExecutionDetails])
pushResultToCohort result !respHashM (SubscriptionMetadata dTime) cursorValues rootFieldName (cohortSnapshot, cohort) = do
  prevRespHashM <- STM.readTVarIO respRef
  -- write to the current websockets if needed
  (subscribersToPush, subscribersToIgnore) <-
    -- We send the response to all the subscribers only when the response changes.
    if isExecError result || respHashM /= prevRespHashM
      then do
        $assertNFHere respHashM -- so we don't write thunks to mutable vars
        STM.atomically $ do
          STM.writeTVar respRef respHashM
          STM.writeTVar (C._cStreamCursorVariables cohort) cursorValues
          return (newSinks <> curSinks, mempty)
      else -- when the response is unchanged, the response is only sent to the newly added subscribers
        return (newSinks, curSinks)
  pushResultToSubscribers subscribersToPush
  pure
    $ over
      (each . each)
      ( \Subscriber {..} ->
          SubscriberExecutionDetails _sId _sMetadata
      )
      (subscribersToPush, subscribersToIgnore)
  where
    rootFieldNameText = G.unName rootFieldName
    -- we want to know if the DB response is an empty array and if it is, then we don't send anything
    -- to the client. We could do this by parsing the response and check for an empty list, but
    -- this will have performance impact when large responses are parsed. Instead, we compare
    -- the DB response to a templated empty array response.

    -- We are using templating instead of doing something like
    -- J.encode $ J.object [ rootFieldNameText J..= [] :: [J.Value] ]
    -- is because, unfortunately, the value returned by the above is
    -- {<rootFieldNameText>:[]} (notice the lack of spaces). So, instead
    -- we're templating according to the format postgres sends JSON responses.
    emptyRespBS = Right $ txtToBs [st|{"#{rootFieldNameText}" : []}|]
    -- Same as the above, but cockroach prefixes the responses with @\x1@ and does not
    -- have a space between the key and the colon.
    roachEmptyRespBS = Right $ "\x1" <> txtToBs [st|{"#{rootFieldNameText}": []}|]

    isResponseEmpty =
      result == emptyRespBS || result == roachEmptyRespBS

    C.CohortSnapshot _ respRef curSinks newSinks = cohortSnapshot

    response = result <&> \payload -> SubscriptionResponse payload dTime

    pushResultToSubscribers subscribers =
      unless isResponseEmpty
        $ flip A.mapConcurrently_ subscribers
        $ \Subscriber {..} -> _sOnChangeCallback response

-- | A single iteration of the streaming query polling loop. Invocations on the
-- same mutable objects may race.
pollStreamingQuery ::
  forall b.
  (BackendTransport b) =>
  PollerId ->
  STM.TVar PollerResponseState ->
  SubscriptionsOptions ->
  (SourceName, SourceConfig b) ->
  RoleName ->
  ParameterizedQueryHash ->
  MultiplexedQuery b ->
  CohortMap 'Streaming ->
  G.Name ->
  SubscriptionPostPollHook ->
  Maybe (IO ()) -> -- Optional IO action to make this function (pollStreamingQuery) testable
  PrometheusMetrics ->
  IO GranularPrometheusMetricsState ->
  TMap.TMap (Maybe OperationName) Int ->
  ResolvedConnectionTemplate b ->
  Maybe (Endo JO.Value) ->
  Logger Hasura ->
  [ModelInfoPart] ->
  IO ModelInfoLogState ->
  IO ()
pollStreamingQuery pollerId pollerResponseState streamingQueryOpts (sourceName, sourceConfig) roleName parameterizedQueryHash query cohortMap rootFieldName postPollHook testActionMaybe prometheusMetrics granularPrometheusMetricsState operationNames' resolvedConnectionTemplate modifier logger modelInfoList modelInfoLogStatus = do
  operationNames <- STM.atomically $ TMap.getMap operationNames'
  (totalTime, (snapshotTime, (batchesDetails, processedCohorts, maybeErrors))) <- withElapsedTime $ do
    -- snapshot the current cohorts and split them into batches
    -- This STM transaction is a read only transaction i.e. it doesn't mutate any state
    (snapshotTime, cohortBatches) <- withElapsedTime $ do
      -- get a snapshot of all the cohorts
      -- this need not be done in a transaction
      cohorts <- STM.atomically $ TMap.toList cohortMap
      cohortSnapshots <- mapM (STM.atomically . getCohortSnapshot) cohorts
      -- cohorts are broken down into batches specified by the batch size
      let cohortBatches = chunksOf (unrefine (unBatchSize batchSize)) cohortSnapshots
      -- associating every batch with their BatchId
      pure $ zip (BatchId <$> [1 ..]) cohortBatches

    for_ testActionMaybe id -- IO action intended to run after the cohorts have been snapshotted

    -- concurrently process each batch and also get the processed cohort with the new updated cohort key
    batchesDetailsAndProcessedCohortsWithMaybeError <- A.forConcurrently cohortBatches $ \(batchId, cohorts) -> do
      (queryExecutionTime, mxRes) <-
        runDBStreamingSubscription @b
          sourceConfig
          query
          (over (each . _2) C._csVariables $ fmap (fmap fst) cohorts)
          resolvedConnectionTemplate
      let dbExecTimeMetric = submDBExecTotalTime $ pmSubscriptionMetrics $ prometheusMetrics
      recordSubcriptionMetric
        granularPrometheusMetricsState
        True
        operationNames
        parameterizedQueryHash
        streamingSubscriptionLabel
        (flip (HistogramVector.observe dbExecTimeMetric) (realToFrac queryExecutionTime))

      previousPollerResponseState <- STM.readTVarIO pollerResponseState

      maybeError <- case mxRes of
        Left err -> do
          when (previousPollerResponseState == PRSSuccess) $ do
            Prometheus.Gauge.inc $ submActiveStreamingPollersInError $ pmSubscriptionMetrics prometheusMetrics
            STM.atomically $ STM.writeTVar pollerResponseState PRSError
          let pollDetailsError =
                PollDetailsError
                  { _pdeBatchId = batchId,
                    _pdeErrorDetails = err
                  }
          return $ Just pollDetailsError
        Right _ -> do
          when (previousPollerResponseState == PRSError) $ do
            Prometheus.Gauge.dec $ submActiveStreamingPollersInError $ pmSubscriptionMetrics prometheusMetrics
            STM.atomically $ STM.writeTVar pollerResponseState PRSSuccess
          return Nothing

      let subscriptionMeta = SubscriptionMetadata $ convertDuration queryExecutionTime
          operations = getCohortOperations cohorts mxRes
          -- batch response size is the sum of the response sizes of the cohorts
          batchResponseSize =
            case mxRes of
              Left _ -> Nothing
              Right resp -> Just $ getSum $ foldMap ((\(_, sqlResp, _) -> Sum . BS.length $ sqlResp)) resp
      (pushTime, cohortsExecutionDetails) <- withElapsedTime
        $ A.forConcurrently operations
        $ \(res, cohortId, respData, latestCursorValueMaybe, (snapshot, cohort)) -> do
          let latestCursorValue@(CursorVariableValues updatedCursorVarVal) =
                let prevCursorVariableValue = CursorVariableValues $ C._unValidatedVariables $ C._cvCursorVariables $ C._csVariables snapshot
                 in case latestCursorValueMaybe of
                      Nothing -> prevCursorVariableValue -- Nothing indicates there was an error when the query was polled
                      Just currentPollCursorValue -> mergeOldAndNewCursorValues prevCursorVariableValue currentPollCursorValue
          (pushedSubscribers, ignoredSubscribers) <-
            -- Push the result to the subscribers present in the cohorts
            pushResultToCohort res (fst <$> respData) subscriptionMeta latestCursorValue rootFieldName (snapshot, cohort)
          let currentCohortKey = C._csVariables snapshot
              updatedCohortKey = modifyCursorCohortVariables (mkUnsafeValidateVariables updatedCursorVarVal) $ C._csVariables snapshot
              snapshottedNewSubs = C._csNewSubscribers snapshot
              cohortExecutionDetails =
                CohortExecutionDetails
                  { _cedCohortId = cohortId,
                    _cedVariables = currentCohortKey,
                    _cedPushedTo = pushedSubscribers,
                    _cedIgnored = ignoredSubscribers,
                    _cedResponseSize = snd <$> respData,
                    _cedBatchId = batchId
                  }
          pure (cohortExecutionDetails, (currentCohortKey, (cohort, updatedCohortKey, snapshottedNewSubs)))
      let processedCohortBatch = snd <$> cohortsExecutionDetails

          -- Note: We want to keep the '_bedPgExecutionTime' field for backwards
          -- compatibility reason, which will be 'Nothing' for non-PG backends.
          -- See https://hasurahq.atlassian.net/browse/GS-329
          pgExecutionTime = case reify (backendTag @b) of
            Postgres Vanilla -> Just queryExecutionTime
            _ -> Nothing

          batchExecDetails =
            BatchExecutionDetails
              pgExecutionTime
              queryExecutionTime
              pushTime
              batchId
              (fst <$> cohortsExecutionDetails)
              batchResponseSize
      pure $ (batchExecDetails, processedCohortBatch, maybeError)

    pure (snapshotTime, unzip3 batchesDetailsAndProcessedCohortsWithMaybeError)

  let initPollDetails =
        PollDetails
          { _pdPollerId = pollerId,
            _pdKind = Streaming,
            _pdGeneratedSql = toTxt query,
            _pdSnapshotTime = snapshotTime,
            _pdBatches = batchesDetails,
            _pdLiveQueryOptions = streamingQueryOpts,
            _pdTotalTime = totalTime,
            _pdSource = sourceName,
            _pdRole = roleName,
            _pdParameterizedQueryHash = parameterizedQueryHash,
            _pdLogLevel = LevelInfo,
            _pdErrors = Nothing
          }
      maybePollDetailsErrors = sequenceA maybeErrors
      pollDetails = case maybePollDetailsErrors of
        Nothing -> initPollDetails
        Just pollDetailsError ->
          initPollDetails
            { _pdLogLevel = LevelError,
              _pdErrors = Just pollDetailsError
            }

  STM.atomically $ do
    -- constructing a cohort map for all the cohorts that have been
    -- processed in the current poll

    -- processed cohorts is an array of tuples of the current poll cohort variables and a tuple
    -- of the cohort and the new cohort key
    let processedCohortsMap = HashMap.fromList $ concat processedCohorts

    -- rebuilding the cohorts and the cohort map, see [Streaming subscription polling]
    -- and [Streaming subscriptions rebuilding cohort map]
    currentCohorts <- TMap.toList cohortMap
    updatedCohortsMap <-
      foldM
        ( \accCohortMap (currentCohortKey, currentCohort) -> do
            let processedCohortMaybe = HashMap.lookup currentCohortKey processedCohortsMap
            case processedCohortMaybe of
              -- A new cohort has been added in the cohort map whilst the
              -- current poll was happening, in this case we just return it
              -- as it is
              Nothing -> HashMap.insertWithM mergeCohorts currentCohortKey currentCohort accCohortMap
              Just (processedCohort, updatedCohortKey, snapshottedNewSubs) -> do
                updateCohortSubscribers currentCohort snapshottedNewSubs
                currentCohortExistingSubscribers <- TMap.toList $ C._cExistingSubscribers currentCohort
                newlyAddedSubscribers <- TMap.getMap $ C._cNewSubscribers currentCohort
                -- The newly added subscribers should not be added to the updated cohort, they should be added
                -- to the old cohort because they need to be procesed for the first time with their initial value.
                -- For example: let's take 2 cohorts,
                -- s means a subscriber
                -- C1 - [s1, s2]
                -- C2 -> [s3]
                -- and S4 is added to C1 and S5 is added to C2 during the poll.
                --
                -- Let's say C1 is updated to C2 and C2 to C3, then
                -- the updated cohort map should look like:
                -- C1 -> [s4]
                -- C2 -> [s1, s2, s5]
                -- C3 -> [s3]
                --
                -- Note that s4 and s5 have not been added to the updated cohort and instead
                -- are in the original cohort they were added in.

                -- all the existing subsribers are removed from the current cohort and
                -- the newly added subscribers are added back
                accCohortMapWithCurrentCohort <-
                  if null newlyAddedSubscribers
                    then pure accCohortMap
                    else do
                      -- Creating a new cohort which will only contain the newly added subscribers
                      newCohort <- do
                        existingSubs <- TMap.new
                        newSubs <- TMap.new
                        pure
                          $ C.Cohort
                            (C._cCohortId currentCohort)
                            (C._cPreviousResponse currentCohort)
                            existingSubs
                            newSubs
                            (C._cStreamCursorVariables currentCohort)
                      TMap.replace (C._cNewSubscribers newCohort) newlyAddedSubscribers
                      HashMap.insertWithM mergeCohorts currentCohortKey newCohort accCohortMap
                let allCurrentSubscribers = Set.fromList $ fst <$> (HashMap.toList newlyAddedSubscribers <> currentCohortExistingSubscribers)
                -- retain subscribers only if they still exist in the original cohort's subscriber.
                -- It may happen that a subscriber has stopped their subscription which means it will
                -- no longer exist in the cohort map, so we need to accordingly remove such subscribers
                -- from our processed cohorts.
                TMap.filterWithKey (\k _ -> k `elem` allCurrentSubscribers) $ C._cExistingSubscribers processedCohort
                TMap.filterWithKey (\k _ -> k `elem` allCurrentSubscribers) $ C._cNewSubscribers processedCohort
                HashMap.insertWithM mergeCohorts updatedCohortKey processedCohort accCohortMapWithCurrentCohort
        )
        mempty
        currentCohorts
    TMap.replace cohortMap updatedCohortsMap
  modelInfoLogStatus' <- modelInfoLogStatus
  when (modelInfoLogStatus' == ModelInfoLogOn) $ do
    for_ (modelInfoList) $ \(ModelInfoPart modelName modelType modelSourceName modelSourceType modelQueryType) -> do
      unLogger logger $ ModelInfoLog LevelInfo $ ModelInfo modelName (toTxt modelType) (toTxt <$> modelSourceName) (toTxt <$> modelSourceType) (toTxt modelQueryType) False
  postPollHook pollDetails
  let totalTimeMetric = submTotalTime $ pmSubscriptionMetrics $ prometheusMetrics
  recordSubcriptionMetric
    granularPrometheusMetricsState
    True
    operationNames
    parameterizedQueryHash
    streamingSubscriptionLabel
    (flip (HistogramVector.observe totalTimeMetric) (realToFrac totalTime))
  where
    SubscriptionsOptions batchSize _ = streamingQueryOpts

    updateCohortSubscribers (C.Cohort _id _respRef curOpsTV newOpsTV _) snapshottedNewSubs = do
      allNewOpsL <- TMap.toList newOpsTV
      let snapshottedNewSubsSet = Set.fromList $ _sId <$> snapshottedNewSubs
      forM_ allNewOpsL $ \(subId, subscriber) ->
        when (subId `elem` snapshottedNewSubsSet) do
          -- we only add the snapshotted new subscribers to the current subscribers
          -- because they have been sent the first message of the subscription. The
          -- new subscribers apart from the snapshotted new subscribers are yet to
          -- recieve their first message so we just keep them as new subscribers
          TMap.insert subscriber subId curOpsTV
          TMap.delete subId newOpsTV

    getCohortSnapshot (cohortVars, cohort) = do
      let C.Cohort resId respRef curOpsTV newOpsTV _ = cohort
      curOpsL <- TMap.toList curOpsTV
      newOpsL <- TMap.toList newOpsTV
      let cohortSnapshot = C.CohortSnapshot cohortVars respRef (map snd curOpsL) (map snd newOpsL)
      return (resId, (cohortSnapshot, cohort))

    mergeCohorts :: Cohort 'Streaming -> Cohort 'Streaming -> STM.STM (Cohort 'Streaming)
    mergeCohorts newCohort oldCohort = do
      let newCohortExistingSubscribers = C._cExistingSubscribers newCohort
          oldCohortExistingSubscribers = C._cExistingSubscribers oldCohort
          newCohortNewSubscribers = C._cNewSubscribers newCohort
          oldCohortNewSubscribers = C._cNewSubscribers oldCohort
      mergedExistingSubscribers <- TMap.union newCohortExistingSubscribers oldCohortExistingSubscribers
      mergedNewSubscribers <- TMap.union newCohortNewSubscribers oldCohortNewSubscribers
      pure
        $ newCohort
          { C._cNewSubscribers = mergedNewSubscribers,
            C._cExistingSubscribers = mergedExistingSubscribers
          }

    getCohortOperations cohorts = \case
      Left e ->
        let resp = throwError $ GQExecError [encodeGQLErr False e]
         in [(resp, cohortId, Nothing, Nothing, snapshot) | (cohortId, snapshot) <- cohorts]
      Right responses -> do
        let cohortSnapshotMap = HashMap.fromList cohorts
        -- every row of the response will contain the cohortId, response of the query and the latest value of the cursor for the cohort
        flip mapMaybe responses $ \(cohortId, respBS, respCursorLatestValue) ->
          let respHash = mkRespHash respBS
              respSize = BS.length respBS
           in -- TODO: currently we ignore the cases when the cohortId from
              -- Postgres response is not present in the cohort map of this batch
              -- (this shouldn't happen but if it happens it means a logic error and
              -- we should log it)
              (pure (applyModifier modifier respBS),cohortId,Just (respHash, respSize),Just respCursorLatestValue,)
                <$> HashMap.lookup cohortId cohortSnapshotMap
