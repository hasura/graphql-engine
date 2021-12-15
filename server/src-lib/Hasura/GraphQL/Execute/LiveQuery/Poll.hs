-- | Multiplexed live query poller threads; see "Hasura.GraphQL.Execute.LiveQuery" for details.
module Hasura.GraphQL.Execute.LiveQuery.Poll
  ( -- * Pollers
    Poller (..),
    PollerId (..),
    PollerIOState (..),
    pollQuery,
    pollStreamingQuery,
    PollerKey (..),
    PollerMap,
    dumpPollerMap,
    PollDetails (..),
    BatchExecutionDetails (..),
    CohortExecutionDetails (..),
    LiveQueryPostPollHook,
    defaultLiveQueryPostPollHook,

    -- * Cohorts
    Cohort (..),
    CohortId,
    newCohortId,
    CohortVariables,
    CohortKey,
    CohortMap,

    -- * Subscribers
    Subscriber (..),
    SubscriberId,
    newSubscriberId,
    SubscriberMetadata,
    mkSubscriberMetadata,
    unSubscriberMetadata,
    SubscriberMap,
    OnChange,
    LGQResponse,
    LiveQueryResponse (..),
    LiveQueryMetadata (..),
    SubscriberExecutionDetails (..),

    -- * Batch
    BatchId (..),
  )
where

import Control.Concurrent.Async qualified as A
import Control.Concurrent.STM qualified as STM
import Control.Immortal qualified as Immortal
import Control.Lens
import Crypto.Hash qualified as CH
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.Extended qualified as Map
import Data.List.Split (chunksOf)
import Data.Monoid (Sum (..))
import Data.Text.Extended
import Data.Time.Clock qualified as Clock
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.AssertNF.CPP
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.LiveQuery.Options
import Hasura.GraphQL.Execute.LiveQuery.Plan
import Hasura.GraphQL.Execute.LiveQuery.TMap qualified as TMap
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (SourceName, getNonNegativeInt)
import Hasura.SQL.Value (TxtEncodedVal (..))
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import ListT qualified
import StmContainers.Map qualified as STMMap
import Text.Shakespeare.Text (st)

-- ----------------------------------------------------------------------------------------------
-- Subscribers

newtype SubscriberId = SubscriberId {unSubscriberId :: UUID.UUID}
  deriving (Show, Eq, Generic, Hashable, J.ToJSON)

newSubscriberId :: IO SubscriberId
newSubscriberId =
  SubscriberId <$> UUID.nextRandom

-- | Allows a user of the live query subsystem (currently websocket transport)
-- to attach arbitrary metadata about a subscriber. This information is available
-- as part of Subscriber in CohortExecutionDetails and can be logged by customizing
-- in pollerlog
newtype SubscriberMetadata = SubscriberMetadata {unSubscriberMetadata :: J.Value}
  deriving (Show, Eq, J.ToJSON)

mkSubscriberMetadata :: WS.WSId -> OperationId -> Maybe OperationName -> RequestId -> SubscriberMetadata
mkSubscriberMetadata websocketId operationId operationName reqId =
  SubscriberMetadata $
    J.object
      [ "websocket_id" J..= websocketId,
        "operation_id" J..= operationId,
        "operation_name" J..= operationName,
        "request_id" J..= reqId
      ]

data Subscriber = Subscriber
  { _sId :: !SubscriberId,
    _sMetadata :: !SubscriberMetadata,
    _sRequestId :: !RequestId,
    _sOperationName :: !(Maybe OperationName),
    _sOnChangeCallback :: !OnChange
  }

-- | live query onChange metadata, used for adding more extra analytics data
data LiveQueryMetadata = LiveQueryMetadata
  { _lqmExecutionTime :: !Clock.DiffTime
  }

data LiveQueryResponse = LiveQueryResponse
  { _lqrPayload :: !BS.ByteString,
    _lqrExecutionTime :: !Clock.DiffTime
  }

type LGQResponse = GQResult LiveQueryResponse

type OnChange = LGQResponse -> IO ()

type SubscriberMap = TMap.TMap SubscriberId Subscriber

-- -------------------------------------------------------------------------------------------------
-- Cohorts

-- | A batched group of 'Subscriber's who are not only listening to the same query but also have
-- identical session and query variables. Each result pushed to a 'Cohort' is forwarded along to
-- each of its 'Subscriber's.
--
-- In SQL, each 'Cohort' corresponds to a single row in the laterally-joined @_subs@ table (and
-- therefore a single row in the query result).
--
-- See also 'CohortMap'.
data Cohort streamCursorVars = Cohort
  { -- | a unique identifier used to identify the cohort in the generated query
    _cCohortId :: !CohortId,
    -- | a hash of the previous query result, if any, used to determine if we need to push an updated
    -- result to the subscribers or not
    _cPreviousResponse :: !(STM.TVar (Maybe ResponseHash)),
    -- | the subscribers we’ve already pushed a result to; we push new results to them iff the
    -- response changes
    _cExistingSubscribers :: !SubscriberMap,
    -- | subscribers we haven’t yet pushed any results to; we push results to them regardless if the
    -- result changed, then merge them in the map of existing subscribers
    _cNewSubscribers :: !SubscriberMap,
    -- | a mutable type which holds the latest value of the subscription stream cursor. In case
    --   of live query subscription, this field can be ignored by setting `streamCursorVars` to `()`
    _cStreamVariables :: !streamCursorVars
  }

-- | The @BatchId@ is a number based ID to uniquely identify a batch in a single poll and
--   it's used to identify the batch to which a cohort belongs to.
newtype BatchId = BatchId {_unBatchId :: Int}
  deriving (Show, Eq, J.ToJSON)

{- Note [Blake2b faster than SHA-256]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the time of writing, from https://blake2.net, it is stated,
"BLAKE2 is a cryptographic hash function faster than MD5, SHA-1, SHA-2, and SHA-3,
yet is at least as secure as the latest standard SHA-3".
-}

-- | A hash used to determine if the result changed without having to keep the entire result in
-- memory. Using a cryptographic hash ensures that a hash collision is almost impossible: with 256
-- bits, even if a subscription changes once per second for an entire year, the probability of a
-- hash collision is ~4.294417×10-63. See Note [Blake2b faster than SHA-256].
newtype ResponseHash = ResponseHash {unResponseHash :: CH.Digest CH.Blake2b_256}
  deriving (Show, Eq)

instance J.ToJSON ResponseHash where
  toJSON = J.toJSON . show . unResponseHash

mkRespHash :: BS.ByteString -> ResponseHash
mkRespHash = ResponseHash . CH.hash

-- | A key we use to determine if two 'Subscriber's belong in the same 'Cohort'
-- (assuming they already meet the criteria to be in the same 'Poller'). Note
-- the distinction between this and 'CohortId'; the latter is a completely
-- synthetic key used only to identify the cohort in the generated SQL query.
type CohortKey = CohortVariables

-- | This has the invariant, maintained in 'removeLiveQuery', that it contains
-- no 'Cohort' with zero total (existing + new) subscribers.
type CohortMap a = TMap.TMap CohortKey (Cohort a)

dumpCohortMap :: CohortMap a -> IO J.Value
dumpCohortMap cohortMap = do
  cohorts <- STM.atomically $ TMap.toList cohortMap
  fmap J.toJSON . forM cohorts $ \(variableValues, cohort) -> do
    cohortJ <- dumpCohort cohort
    return $
      J.object
        [ "variables" J..= variableValues,
          "cohort" J..= cohortJ
        ]
  where
    dumpCohort (Cohort respId respTV curOps newOps _) =
      STM.atomically $ do
        prevResHash <- STM.readTVar respTV
        curOpIds <- TMap.toList curOps
        newOpIds <- TMap.toList newOps
        return $
          J.object
            [ "resp_id" J..= respId,
              "current_ops" J..= map fst curOpIds,
              "new_ops" J..= map fst newOpIds,
              "previous_result_hash" J..= prevResHash
            ]

data CohortSnapshot a = CohortSnapshot
  { _csVariables :: !CohortVariables,
    _csPreviousResponse :: !(STM.TVar (Maybe ResponseHash)),
    _csExistingSubscribers :: ![Subscriber],
    _csNewSubscribers :: ![Subscriber],
    _csSubscriptionType :: !a
  }

pushResultToCohort ::
  GQResult BS.ByteString ->
  Maybe ResponseHash ->
  LiveQueryMetadata ->
  CohortSnapshot () ->
  -- | subscribers to which data has been pushed, subscribers which already
  -- have this data (this information is exposed by metrics reporting)
  IO ([SubscriberExecutionDetails], [SubscriberExecutionDetails])
pushResultToCohort result !respHashM (LiveQueryMetadata dTime) cohortSnapshot = do
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
    CohortSnapshot _ respRef curSinks newSinks () = cohortSnapshot

    response = result <&> (`LiveQueryResponse` dTime)
    pushResultToSubscribers =
      A.mapConcurrently_ $ \Subscriber {..} -> _sOnChangeCallback response

pushResultToStreamSubscriptionCohort ::
  GQResult BS.ByteString ->
  Maybe ResponseHash ->
  LiveQueryMetadata ->
  Maybe CursorVariableValues ->
  G.Name ->
  -- | Root field name
  CohortSnapshot (STM.TVar CursorVariableValues) ->
  -- | subscribers to which data has been pushed, subscribers which already
  -- have this data (this information is exposed by metrics reporting)
  IO ([SubscriberExecutionDetails], [SubscriberExecutionDetails])
pushResultToStreamSubscriptionCohort result !respHashM (LiveQueryMetadata dTime) latestCursorValuesDB rootFieldName cohortSnapshot = do
  prevRespHashM <- STM.readTVarIO respRef
  -- write to the current websockets if needed
  (subscribersToPush, subscribersToIgnore) <-
    if isExecError result || respHashM /= prevRespHashM
      then do
        $assertNFHere respHashM -- so we don't write thunks to mutable vars
        STM.atomically $ do
          STM.writeTVar respRef respHashM
          let combineFn previousVal currentVal =
                case currentVal of
                  TENull -> previousVal -- When we get a null value from the DB, we retain the older value
                  TELit t -> TELit t
          case latestCursorValuesDB of
            Nothing -> pure () -- Nothing indicates there was an error when the query was polled
            Just (CursorVariableValues currentPollCursorValues) -> do
              CursorVariableValues prevPollCursorValues <- STM.readTVar latestCursorValueTV
              STM.writeTVar latestCursorValueTV (CursorVariableValues (Map.unionWith combineFn prevPollCursorValues currentPollCursorValues))
          return (newSinks <> curSinks, mempty)
      else return (newSinks, curSinks)
  let rootFieldNameText = G.unName rootFieldName
      -- we want to know if the DB response is an empty array and if it is, then we don't send anything
      -- to the client. We could do this by parsing the response and check for an empty list, but
      -- this will have performance impact when large responses are parsed. Instead, we compare
      -- the DB response to a templated empty array response.

      -- We are using templating instead of doing something like
      -- J.encode $ J.object [ rootFieldNameText J..= [] :: [J.Value] ]
      -- is because, unfortunately, the value returned by the above is
      -- {<rootFieldNameText>:[]} (notice the lack of spaces). So, instead
      -- we're templating according to the format postgres sends JSON responses.
      emptyRespBS = txtToBs $ [st|{"#{rootFieldNameText}" : []}|]
      isResponseEmpty =
        case result of
          Left _ -> False
          Right respBS -> emptyRespBS == respBS
  -- In streaming subscriptions, we don't send anything when there are no rows returned
  -- by the client.
  bool (pushResultToSubscribers subscribersToPush) (pure ()) isResponseEmpty
  pure $
    over
      (each . each)
      ( \Subscriber {..} ->
          SubscriberExecutionDetails _sId _sMetadata
      )
      (subscribersToPush, subscribersToIgnore)
  where
    CohortSnapshot _ respRef curSinks newSinks latestCursorValueTV = cohortSnapshot

    response = result <&> \payload -> LiveQueryResponse payload dTime
    pushResultToSubscribers =
      A.mapConcurrently_ $ \Subscriber {..} -> _sOnChangeCallback response

-- -----------------------------------------------------------------------------
-- Pollers

-- | A unique, multiplexed query. Each 'Poller' has its own polling thread that
-- periodically polls Postgres and pushes results to each of its listening
-- 'Cohort's.
--
-- In SQL, an 'Poller' corresponds to a single, multiplexed query, though in
-- practice, 'Poller's with large numbers of 'Cohort's are batched into
-- multiple concurrent queries for performance reasons.
data Poller streamCursor = Poller
  { _pCohorts :: !(CohortMap streamCursor),
    -- | This is in a separate 'STM.TMVar' because it’s important that we are
    -- able to construct 'Poller' values in 'STM.STM' --- we need the insertion
    -- into the 'PollerMap' to be atomic to ensure that we don’t accidentally
    -- create two for the same query due to a race. However, we can’t spawn the
    -- worker thread or create the metrics store in 'STM.STM', so we insert it
    -- into the 'Poller' only after we’re certain we won’t create any duplicates.
    --
    -- This var is "write once", moving monotonically from empty to full.
    -- TODO this could probably be tightened up to something like
    -- 'STM PollerIOState'
    _pIOState :: !(STM.TMVar PollerIOState)
  }

data PollerIOState = PollerIOState
  { -- | a handle on the poller’s worker thread that can be used to
    -- 'Immortal.stop' it if all its cohorts stop listening
    _pThread :: !Immortal.Thread,
    _pId :: !PollerId
  }

data PollerKey =
  -- we don't need operation name here as a subscription will only have a
  -- single top level field
  PollerKey
  { _lgSource :: !SourceName,
    _lgRole :: !RoleName,
    _lgQuery :: !Text
  }
  deriving (Show, Eq, Generic)

instance Hashable PollerKey

instance J.ToJSON PollerKey where
  toJSON (PollerKey source role query) =
    J.object
      [ "source" J..= source,
        "role" J..= role,
        "query" J..= query
      ]

type PollerMap a = STMMap.Map PollerKey (Poller a)

dumpPollerMap :: Bool -> PollerMap a -> IO J.Value
dumpPollerMap extended lqMap =
  fmap J.toJSON $ do
    entries <- STM.atomically $ ListT.toList $ STMMap.listT lqMap
    forM entries $ \(PollerKey source role query, Poller cohortsMap ioState) -> do
      PollerIOState threadId pollerId <- STM.atomically $ STM.readTMVar ioState
      cohortsJ <-
        if extended
          then Just <$> dumpCohortMap cohortsMap
          else return Nothing
      return $
        J.object
          [ "source" J..= source,
            "role" J..= role,
            "thread_id" J..= show (Immortal.threadId threadId),
            "poller_id" J..= pollerId,
            "multiplexed_query" J..= query,
            "cohorts" J..= cohortsJ
          ]

-- | An ID to track unique 'Poller's, so that we can gather metrics about each
-- poller
newtype PollerId = PollerId {unPollerId :: UUID.UUID}
  deriving (Show, Eq, Generic, J.ToJSON)

data SubscriberExecutionDetails = SubscriberExecutionDetails
  { _sedSubscriberId :: !SubscriberId,
    _sedSubscriberMetadata :: !SubscriberMetadata
  }
  deriving (Show, Eq)

-- | Execution information related to a cohort on a poll cycle
data CohortExecutionDetails = CohortExecutionDetails
  { _cedCohortId :: !CohortId,
    _cedVariables :: !CohortVariables,
    -- | Nothing in case of an error
    _cedResponseSize :: !(Maybe Int),
    -- | The response on this cycle has been pushed to these above subscribers
    -- New subscribers (those which haven't been around during the previous poll
    -- cycle) will always be part of this
    _cedPushedTo :: ![SubscriberExecutionDetails],
    -- | The response on this cycle has *not* been pushed to these above
    -- subscribers. This would when the response hasn't changed from the previous
    -- polled cycle
    _cedIgnored :: ![SubscriberExecutionDetails],
    _cedBatchId :: !BatchId
  }
  deriving (Show, Eq)

-- | Execution information related to a single batched execution
data BatchExecutionDetails = BatchExecutionDetails
  { -- | postgres execution time of each batch
    _bedPgExecutionTime :: !Clock.DiffTime,
    -- | time to taken to push to all cohorts belonging to this batch
    _bedPushTime :: !Clock.DiffTime,
    -- | id of the batch
    _bedBatchId :: !BatchId,
    -- | execution details of the cohorts belonging to this batch
    _bedCohorts :: ![CohortExecutionDetails],
    _bedBatchResponseSizeBytes :: !(Maybe Int)
  }
  deriving (Show, Eq)

-- | see Note [Minimal LiveQuery Poller Log]
batchExecutionDetailMinimal :: BatchExecutionDetails -> J.Value
batchExecutionDetailMinimal BatchExecutionDetails {..} =
  let batchRespSize =
        maybe
          mempty
          (\respSize -> ["batch_response_size_bytes" J..= respSize])
          _bedBatchResponseSizeBytes
   in J.object
        ( [ "pg_execution_time" J..= _bedPgExecutionTime,
            "push_time" J..= _bedPushTime
          ]
            -- log batch resp size only when there are no errors
            <> batchRespSize
        )

data PollDetails = PollDetails
  { -- | the unique ID (basically a thread that run as a 'Poller') for the
    -- 'Poller'
    _pdPollerId :: !PollerId,
    -- | the multiplexed SQL query to be run against the database with all the
    -- variables together
    _pdGeneratedSql :: !Text,
    -- | the time taken to get a snapshot of cohorts from our 'LiveQueriesState'
    -- data structure
    _pdSnapshotTime :: !Clock.DiffTime,
    -- | list of execution batches and their details
    _pdBatches :: ![BatchExecutionDetails],
    -- | total time spent on a poll cycle
    _pdTotalTime :: !Clock.DiffTime,
    _pdLiveQueryOptions :: !LiveQueriesOptions,
    _pdSource :: !SourceName,
    _pdRole :: !RoleName,
    _pdParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show, Eq)

{- Note [Minimal LiveQuery Poller Log]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only want to log the minimal information in the livequery-poller-log as it
could be expensive to log the details of every subscriber (all poller related
information can always be retrieved by dumping the current live queries state)
We capture a lot more details in PollDetails and BatchExecutionDetails than
that is logged currently as other implementations such as pro can use them if
they need to.
-}

-- | see Note [Minimal LiveQuery Poller Log]
pollDetailMinimal :: PollDetails -> J.Value
pollDetailMinimal PollDetails {..} =
  J.object
    [ "poller_id" J..= _pdPollerId,
      "snapshot_time" J..= _pdSnapshotTime,
      "batches" J..= map batchExecutionDetailMinimal _pdBatches,
      "total_time" J..= _pdTotalTime,
      "source" J..= _pdSource,
      "role" J..= _pdRole
    ]

instance L.ToEngineLog PollDetails L.Hasura where
  toEngineLog pl = (L.LevelInfo, L.ELTLivequeryPollerLog, pollDetailMinimal pl)

type LiveQueryPostPollHook = PollDetails -> IO ()

-- the default LiveQueryPostPollHook
defaultLiveQueryPostPollHook :: L.Logger L.Hasura -> LiveQueryPostPollHook
defaultLiveQueryPostPollHook = L.unLogger

-- | Where the magic happens: the top-level action run periodically by each
-- active 'Poller'. This needs to be async exception safe.
pollQuery ::
  forall b.
  BackendTransport b =>
  PollerId ->
  LiveQueriesOptions ->
  (SourceName, SourceConfig b) ->
  RoleName ->
  ParameterizedQueryHash ->
  MultiplexedQuery b ->
  CohortMap () ->
  LiveQueryPostPollHook ->
  IO ()
pollQuery pollerId lqOpts (sourceName, sourceConfig) roleName parameterizedQueryHash query cohortMap postPollHook = do
  (totalTime, (snapshotTime, batchesDetails)) <- withElapsedTime $ do
    -- snapshot the current cohorts and split them into batches
    (snapshotTime, cohortBatches) <- withElapsedTime $ do
      -- get a snapshot of all the cohorts
      -- this need not be done in a transaction
      cohorts <- STM.atomically $ TMap.toList cohortMap
      cohortSnapshots <- mapM (STM.atomically . getCohortSnapshot) cohorts
      -- cohorts are broken down into batches specified by the batch size
      let cohortBatches = chunksOf (getNonNegativeInt (unBatchSize batchSize)) cohortSnapshots
      -- associating every batch with their BatchId
      pure $ zip (BatchId <$> [1 ..]) cohortBatches

    -- concurrently process each batch
    batchesDetails <- A.forConcurrently cohortBatches $ \(batchId, cohorts) -> do
      (queryExecutionTime, mxRes) <- runDBSubscription @b sourceConfig query $ over (each . _2) _csVariables cohorts

      let lqMeta = LiveQueryMetadata $ convertDuration queryExecutionTime
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
                _cedVariables = _csVariables snapshot,
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
    LiveQueriesOptions batchSize _ = lqOpts

    getCohortSnapshot (cohortVars, handlerC) = do
      let Cohort resId respRef curOpsTV newOpsTV () = handlerC
      curOpsL <- TMap.toList curOpsTV
      newOpsL <- TMap.toList newOpsTV
      forM_ newOpsL $ \(k, action) -> TMap.insert action k curOpsTV
      TMap.reset newOpsTV

      let cohortSnapshot = CohortSnapshot cohortVars respRef (map snd curOpsL) (map snd newOpsL) ()
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

-- | Where the magic happens: the top-level action run periodically by each
-- active 'Poller'. This needs to be async exception safe.
pollStreamingQuery ::
  forall b.
  BackendTransport b =>
  PollerId ->
  LiveQueriesOptions ->
  (SourceName, SourceConfig b) ->
  RoleName ->
  ParameterizedQueryHash ->
  MultiplexedQuery b ->
  CohortMap (STM.TVar CursorVariableValues) ->
  G.Name ->
  LiveQueryPostPollHook ->
  IO ()
pollStreamingQuery pollerId lqOpts (sourceName, sourceConfig) roleName parameterizedQueryHash query cohortMap rootFieldName postPollHook = do
  (totalTime, (snapshotTime, batchesDetails)) <- withElapsedTime $ do
    -- snapshot the current cohorts and split them into batches
    (snapshotTime, cohortBatches) <- withElapsedTime $ do
      -- get a snapshot of all the cohorts
      -- this need not be done in a transaction
      cohorts <- STM.atomically $ TMap.toList cohortMap
      cohortSnapshots <- mapM (STM.atomically . getCohortSnapshot) cohorts
      -- cohorts are broken down into batches specified by the batch size
      let cohortBatches = chunksOf (getNonNegativeInt (unBatchSize batchSize)) cohortSnapshots
      -- associating every batch with their BatchId
      pure $ zip (BatchId <$> [1 ..]) cohortBatches

    -- concurrently process each batch
    batchesDetails <- A.forConcurrently cohortBatches $ \(batchId, cohorts) -> do
      (queryExecutionTime, mxRes) <- runDBStreamingSubscription @b sourceConfig query $ over (each . _2) _csVariables cohorts

      let lqMeta = LiveQueryMetadata $ convertDuration queryExecutionTime
          operations = getCohortOperations cohorts mxRes
          -- batch response size is the sum of the response sizes of the cohorts
          batchResponseSize =
            case mxRes of
              Left _ -> Nothing
              Right resp -> Just $ getSum $ foldMap ((\(_, sqlResp, _) -> Sum . BS.length $ sqlResp)) resp
      (pushTime, cohortsExecutionDetails) <- withElapsedTime $
        A.forConcurrently operations $ \(res, cohortId, respData, latestCursorValueMaybe, snapshot) -> do
          (pushedSubscribers, ignoredSubscribers) <-
            pushResultToStreamSubscriptionCohort res (fst <$> respData) lqMeta latestCursorValueMaybe rootFieldName snapshot
          pure
            CohortExecutionDetails
              { _cedCohortId = cohortId,
                _cedVariables = _csVariables snapshot,
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

  -- We rebuild the cohort map here, because after every poll, the latest value of the cursors
  -- change. We rebuild the current poller's cohort map so that the cohort key includes the latest
  -- cursor value in it. This will also help in grouping cohorts together, for example, if client 1
  -- has started a streaming subscription with initial value as 2 on an serial primary key column
  -- and has been running for some time and the latest cursor value in the server is say, 50 now.
  -- Now, if at this exact moment, if there's a new client with the same query and the initial cursor value
  -- as 50, then the client 2 will be added to client 1's cohort.
  cohortsWithUpdatedCohortKeys <- do
    cohortsList <- STM.atomically $ TMap.toList cohortMap
    for cohortsList $ \(cohortKey, cohort) -> do
      CursorVariableValues vals <- STM.readTVarIO $ _cStreamVariables cohort
      pure (modifyCursorCohortVariables (mkUnsafeValidateVariables vals) cohortKey, cohort)
  STM.atomically $ do
    updatedCohortMap <- Map.fromListWithM mergeCohorts cohortsWithUpdatedCohortKeys
    TMap.swap cohortMap updatedCohortMap
  postPollHook pollDetails
  where
    LiveQueriesOptions batchSize _ = lqOpts

    getCohortSnapshot (cohortVars, handlerC) = do
      let Cohort resId respRef curOpsTV newOpsTV latestCursorValsTVar = handlerC
      curOpsL <- TMap.toList curOpsTV
      newOpsL <- TMap.toList newOpsTV
      forM_ newOpsL $ \(k, action) -> TMap.insert action k curOpsTV
      TMap.reset newOpsTV
      let cohortSnapshot = CohortSnapshot cohortVars respRef (map snd curOpsL) (map snd newOpsL) latestCursorValsTVar
      return (resId, cohortSnapshot)

    mergeCohorts :: Cohort (STM.TVar CursorVariableValues) -> Cohort (STM.TVar CursorVariableValues) -> STM.STM (Cohort (STM.TVar CursorVariableValues))
    mergeCohorts newCohort oldCohort = do
      let newCohortExistingSubscribers = _cExistingSubscribers newCohort
          oldCohortExistingSubscribers = _cExistingSubscribers oldCohort
          newCohortNewSubscribers = _cNewSubscribers newCohort
          oldCohortNewSubscribers = _cNewSubscribers oldCohort
      mergedExistingSubscribers <- TMap.union newCohortExistingSubscribers oldCohortExistingSubscribers
      mergedNewSubscribers <- TMap.union newCohortNewSubscribers oldCohortNewSubscribers
      pure $
        newCohort
          { _cNewSubscribers = mergedNewSubscribers,
            _cExistingSubscribers = mergedExistingSubscribers
          }

    getCohortOperations cohorts = \case
      Left e ->
        -- TODO: this is internal error
        let resp = throwError $ GQExecError [encodeGQLErr False e]
         in [(resp, cohortId, Nothing, Nothing, snapshot) | (cohortId, snapshot) <- cohorts]
      Right responses -> do
        let cohortSnapshotMap = Map.fromList cohorts
        flip mapMaybe responses $ \(cohortId, respBS, respCursorLatestValue) ->
          let respHash = mkRespHash respBS
              respSize = BS.length respBS
           in -- TODO: currently we ignore the cases when the cohortId from
              -- Postgres response is not present in the cohort map of this batch
              -- (this shouldn't happen but if it happens it means a logic error and
              -- we should log it)
              (pure respBS,cohortId,Just (respHash, respSize),Just respCursorLatestValue,)
                <$> Map.lookup cohortId cohortSnapshotMap
