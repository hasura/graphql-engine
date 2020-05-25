-- | Multiplexed live query poller threads; see "Hasura.GraphQL.Execute.LiveQuery" for details.
module Hasura.GraphQL.Execute.LiveQuery.Poll (
  -- * Pollers
    Poller(..)
  , PollerId(..)
  , PollerIOState(..)
  , pollQuery

  , PollerKey(..)
  , PollerMap
  , dumpPollerMap

  , RefetchMetrics
  , initRefetchMetrics

  -- * Cohorts
  , Cohort(..)
  , CohortId
  , newCohortId
  , CohortVariables
  , CohortKey
  , CohortMap

  -- * Subscribers
  , Subscriber(..)
  , SubscriberId
  , UniqueSubscriberId(..)
  , newSinkId
  , SubscriberMap
  , OnChange
  , LGQResponse
  , LiveQueryResponse(..)
  , LiveQueryMetadata(..)
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async                    as A
import qualified Control.Concurrent.STM                      as STM
import qualified Control.Immortal                            as Immortal
import qualified Crypto.Hash                                 as CH
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.Extended                         as J
import qualified Data.Aeson.TH                               as J
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.HashMap.Strict                         as Map
import qualified Data.Time.Clock                             as Clock
import qualified Data.UUID                                   as UUID
import qualified Data.UUID.V4                                as UUID
import qualified Database.PG.Query                           as Q
import qualified ListT
import qualified StmContainers.Map                           as STMMap
import qualified System.Metrics.Distribution                 as Metrics

import           Data.List.Split                             (chunksOf)
import           GHC.AssertNF

import qualified Hasura.GraphQL.Execute.LiveQuery.TMap       as TMap
import qualified Hasura.GraphQL.Transport.WebSocket.Protocol as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import qualified Hasura.Logging                              as L

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Options
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types.Error
import           Hasura.Session

-- -------------------------------------------------------------------------------------------------
-- Subscribers

-- | This is to identify each subscriber based on their (websocket id,
-- operation id) - this is useful for collecting metrics and then correlating.
-- The current 'SubscriberId' has a UUID which is internal and not useful for
-- metrics.
data UniqueSubscriberId
  = UniqueSubscriberId
  { _usiWebsocketId :: !WS.WSId
  , _usiOperationId :: !WS.OperationId
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''UniqueSubscriberId)

data Subscriber
  = Subscriber
  { _sOnChangeCallback     :: !OnChange
  , _sWebsocketOperationId :: !UniqueSubscriberId
  }

-- | live query onChange metadata, used for adding more extra analytics data
data LiveQueryMetadata
  = LiveQueryMetadata
  { _lqmExecutionTime :: !Clock.DiffTime
  -- ^ Time spent waiting on the generated query to execute on postgres or the remote.
  }

data LiveQueryResponse
  = LiveQueryResponse
  { _lqrPayload       :: !BL.ByteString
  , _lqrExecutionTime :: !Clock.DiffTime
  }

type LGQResponse = GQResult LiveQueryResponse
type OnChange = LGQResponse -> IO ()

newtype SubscriberId = SubscriberId { _unSinkId :: UUID.UUID }
  deriving (Show, Eq, Hashable, J.ToJSON)

newSinkId :: IO SubscriberId
newSinkId = SubscriberId <$> UUID.nextRandom

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
data Cohort
  = Cohort
  { _cCohortId            :: !CohortId
  -- ^ a unique identifier used to identify the cohort in the generated query
  , _cPreviousResponse    :: !(STM.TVar (Maybe ResponseHash))
  -- ^ a hash of the previous query result, if any, used to determine if we need to push an updated
  -- result to the subscribers or not
  , _cExistingSubscribers :: !SubscriberMap
  -- ^ the subscribers we’ve already pushed a result to; we push new results to them iff the
  -- response changes
  , _cNewSubscribers      :: !SubscriberMap
  -- ^ subscribers we haven’t yet pushed any results to; we push results to them regardless if the
  -- result changed, then merge them in the map of existing subscribers
  }

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
newtype ResponseHash = ResponseHash { unResponseHash :: CH.Digest CH.Blake2b_256 }
  deriving (Show, Eq)

instance J.ToJSON ResponseHash where
  toJSON = J.toJSON . show . unResponseHash

mkRespHash :: BS.ByteString -> ResponseHash
mkRespHash = ResponseHash . CH.hash

-- | A key we use to determine if two 'Subscriber's belong in the same 'Cohort' (assuming they
-- already meet the criteria to be in the same 'Poller'). Note the distinction between this and
-- 'CohortId'; the latter is a completely synthetic key used only to identify the cohort in the
-- generated SQL query.
type CohortKey = CohortVariables
-- | This has the invariant, maintained in 'removeLiveQuery', that it contains no 'Cohort' with
-- zero total (existing + new) subscribers.
type CohortMap = TMap.TMap CohortKey Cohort

dumpCohortMap :: CohortMap -> IO J.Value
dumpCohortMap cohortMap = do
  cohorts <- STM.atomically $ TMap.toList cohortMap
  fmap J.toJSON . forM cohorts $ \(variableValues, cohort) -> do
    cohortJ <- dumpCohort cohort
    return $ J.object
      [ "variables" J..= variableValues
      , "cohort" J..= cohortJ
      ]
  where
    dumpCohort (Cohort respId respTV curOps newOps) =
      STM.atomically $ do
      prevResHash <- STM.readTVar respTV
      curOpIds <- TMap.toList curOps
      newOpIds <- TMap.toList newOps
      return $ J.object
        [ "resp_id" J..= respId
        , "current_ops" J..= map fst curOpIds
        , "new_ops" J..= map fst newOpIds
        , "previous_result_hash" J..= prevResHash
        ]

data CohortSnapshot
  = CohortSnapshot
  { _csVariables           :: !CohortVariables
  , _csPreviousResponse    :: !(STM.TVar (Maybe ResponseHash))
  , _csExistingSubscribers :: ![Subscriber]
  , _csNewSubscribers      :: ![Subscriber]
  }

pushResultToCohort
  :: GQResult EncJSON
  -> Maybe ResponseHash
  -> LiveQueryMetadata
  -> CohortSnapshot
  -> IO ()
pushResultToCohort result !respHashM (LiveQueryMetadata dTime) cohortSnapshot = do
  prevRespHashM <- STM.readTVarIO respRef
  -- write to the current websockets if needed
  sinks <-
    if isExecError result || respHashM /= prevRespHashM
    then do
      $assertNFHere respHashM  -- so we don't write thunks to mutable vars
      STM.atomically $ STM.writeTVar respRef respHashM
      return (newSinks <> curSinks)
    else
      return newSinks
  pushResultToSubscribers sinks
  where
    CohortSnapshot _ respRef curSinks newSinks = cohortSnapshot
    response = result <&> \payload -> LiveQueryResponse (encJToLBS payload) dTime
    pushResultToSubscribers = A.mapConcurrently_ $ \(Subscriber action _) -> action response
-- -------------------------------------------------------------------------------------------------
-- Pollers

-- | A unique, multiplexed query. Each 'Poller' has its own polling thread that periodically polls
-- Postgres and pushes results to each of its listening 'Cohort's.
--
-- In SQL, an 'Poller' corresponds to a single, multiplexed query, though in practice, 'Poller's
-- with large numbers of 'Cohort's are batched into multiple concurrent queries for performance
-- reasons.
data Poller
  = Poller
  { _pCohorts :: !CohortMap
  , _pIOState :: !(STM.TMVar PollerIOState)
  -- ^ This is in a separate 'STM.TMVar' because it’s important that we are able to construct
  -- 'Poller' values in 'STM.STM' --- we need the insertion into the 'PollerMap' to be atomic to
  -- ensure that we don’t accidentally create two for the same query due to a race. However, we
  -- can’t spawn the worker thread or create the metrics store in 'STM.STM', so we insert it into
  -- the 'Poller' only after we’re certain we won’t create any duplicates.
  --
  -- This var is "write once", moving monotonically from empty to full.
  -- TODO this could probably be tightened up to something like :: STM PollerIOState
  }
data PollerIOState
  = PollerIOState
  { _pThread  :: !Immortal.Thread
  -- ^ a handle on the poller’s worker thread that can be used to 'Immortal.stop' it if all its
  -- cohorts stop listening
  , _pMetrics :: !RefetchMetrics
  }

data RefetchMetrics
  = RefetchMetrics
  { _rmSnapshot :: !Metrics.Distribution
  , _rmPush     :: !Metrics.Distribution
  , _rmQuery    :: !Metrics.Distribution
  , _rmTotal    :: !Metrics.Distribution
  }

initRefetchMetrics :: IO RefetchMetrics
initRefetchMetrics = RefetchMetrics <$> Metrics.new <*> Metrics.new <*> Metrics.new <*> Metrics.new

data PollerKey
  -- we don't need operation name here as a subscription will
  -- only have a single top level field
  = PollerKey
  { _lgRole  :: !RoleName
  , _lgQuery :: !MultiplexedQuery
  } deriving (Show, Eq, Generic)

instance Hashable PollerKey

instance J.ToJSON PollerKey where
  toJSON (PollerKey role query) =
    J.object [ "role" J..= role
             , "query" J..= query
             ]

type PollerMap = STMMap.Map PollerKey Poller

dumpPollerMap :: Bool -> PollerMap -> IO J.Value
dumpPollerMap extended lqMap =
  fmap J.toJSON $ do
    entries <- STM.atomically $ ListT.toList $ STMMap.listT lqMap
    forM entries $ \(PollerKey role query, Poller cohortsMap ioState) -> do
      PollerIOState threadId metrics <- STM.atomically $ STM.readTMVar ioState
      metricsJ <- dumpRefetchMetrics metrics
      cohortsJ <-
        if extended
        then Just <$> dumpCohortMap cohortsMap
        else return Nothing
      return $ J.object
        [ "role" J..= role
        , "thread_id" J..= show (Immortal.threadId threadId)
        , "multiplexed_query" J..= query
        , "cohorts" J..= cohortsJ
        , "metrics" J..= metricsJ
        ]
  where
    dumpRefetchMetrics metrics = do
      snapshotS <- Metrics.read $ _rmSnapshot metrics
      queryS <- Metrics.read $ _rmQuery metrics
      pushS <- Metrics.read $ _rmPush metrics
      totalS <- Metrics.read $ _rmTotal metrics
      return $ J.object
        [ "snapshot" J..= dumpStats snapshotS
        , "query" J..= dumpStats queryS
        , "push" J..= dumpStats pushS
        , "total" J..= dumpStats totalS
        ]

    dumpStats stats =
      J.object
      [ "mean" J..= Metrics.mean stats
      , "variance" J..= Metrics.variance stats
      , "count" J..= Metrics.count stats
      , "min" J..= Metrics.min stats
      , "max" J..= Metrics.max stats
      ]


-- | create an ID to track unique 'Poller's, so that we can gather metrics about each poller
newtype PollerId = PollerId { unPollerId :: UUID.UUID }
  deriving (Show, Eq, Generic, J.ToJSON)

data SimpleCohort
  = SimpleCohort
  { _scCohortId          :: !CohortId
  , _scCohortVariables   :: !CohortVariables
  , _scResponseSizeBytes :: !(Maybe Int)
  , _scSubscribers       :: ![UniqueSubscriberId]
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''SimpleCohort)

data ExecutionBatch
  = ExecutionBatch
  { _ebPgExecutionTime :: !Clock.DiffTime
  -- ^ postgres execution time of each batch
  , _ebPushCohortsTime :: !Clock.DiffTime
  -- ^ time to taken to push to all cohorts belonging to this batch
  , _ebBatchSize       :: !Int
  -- ^ the number of cohorts belonging to this batch
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''ExecutionBatch)

data PollerLog
  = PollerLog
  { _plPollerId           :: !PollerId
  -- ^ the unique ID (basically a thread that run as a 'Poller') for the 'Poller'; TODO: is this
  -- practical? do we need to track, potentially, 1000s of poller threads uniquely?
  -- , _plSubscribers        :: ![UniqueSubscriberId]
  -- -- ^ list of (WebsocketConnId, OperationId) to uniquely identify each subscriber
  -- -- ^ REMOVING this because redundant info and extra network bytes
  , _plSubscriberCount    :: !Int
  -- ^ count of the above '_plSubscribers' field; convenient for Lux to perform analytics
  , _plCohortSize         :: !Int
  -- ^ how many cohorts (or no of groups of different variables but same query) are there
  , _plCohorts            :: ![SimpleCohort]
  -- ^ the different query vars which forms the different cohorts
  , _plExecutionBatches   :: ![ExecutionBatch]
  -- ^ list of execution batches and their details
  , _plExecutionBatchSize :: !Int
  -- ^ what is the current batch size that is being run. i.e. in how many batches will this query be
  -- run
  , _plSnapshotTime       :: !Clock.DiffTime
  -- ^ the time taken to get a snapshot of cohorts from our 'LiveQueriesState' data structure
  , _plTotalTime          :: !Clock.DiffTime
  -- ^ total time spent on running the entire 'Poller' thread (which may run more than one thread in
  -- batches for one SQL query) and then push results to each client
  , _plGeneratedSql       :: !Q.Query
  -- ^ the multiplexed SQL query to be run against Postgres with all the variables together
  , _plLiveQueryOptions   :: !LiveQueriesOptions
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''PollerLog)

instance L.ToEngineLog PollerLog L.Hasura where
  toEngineLog pl = (L.LevelInfo, L.ELTInternal L.ILTPollerLog, J.toJSON pl)


-- | Where the magic happens: the top-level action run periodically by each active 'Poller'.
--
-- This needs to be async exception safe.
pollQuery
  :: L.Logger L.Hasura
  -> PollerId
  -> RefetchMetrics
  -> LiveQueriesOptions
  -> PGExecCtx
  -> MultiplexedQuery
  -> Poller
  -> IO ()
pollQuery logger pollerId metrics lqOpts pgExecCtx pgQuery handler = do
  (totalTime, (snapshotTime, cohortSnapshotMap, queryVarsBatches, execRes)) <- timing _rmTotal $ do
    (snapshotTime, (cohortSnapshotMap, queryVarsBatches)) <- timing _rmSnapshot $ do
      -- get a snapshot of all the cohorts
      -- this need not be done in a transaction
      cohorts <- STM.atomically $ TMap.toList cohortMap
      cohortSnapshotMap <- Map.fromList <$> mapM (STM.atomically . getCohortSnapshot) cohorts

      -- queryVarsBatches are queryVars broken down into chunks specified by the batch size
      let queryVarsBatches = chunksOf (unBatchSize batchSize) $ getQueryVars cohortSnapshotMap
      return (cohortSnapshotMap, queryVarsBatches)

    -- run the multiplexed query, in the partitioned batches
    execRes <- A.forConcurrently queryVarsBatches $ \queryVars -> do
      (dt, mxRes) <- timing _rmQuery $
        runExceptT $ runLazyTx' pgExecCtx $ executeMultiplexedQuery pgQuery queryVars
      let lqMeta = LiveQueryMetadata $ convertDuration dt
          operations = getCohortOperations cohortSnapshotMap lqMeta mxRes

      (pushTime, respSizes) <- timing _rmPush $ do
        -- concurrently push each unique result
        A.forConcurrently operations $ \(res, respSize, respHash, action, snapshot) ->
          pushResultToCohort res respHash action snapshot >> return respSize
        -- return various metrics of each opeartion of executing against postgres
      return $ (ExecutionBatch dt pushTime (length queryVars), respSizes)

    return (snapshotTime, cohortSnapshotMap, queryVarsBatches, execRes)

  -- transform data for PollerLog and log it
  let (execBatches, respSizes') = unzip execRes
      respSizes = concat respSizes'
      subscriberWsOpIds = getSubscribers cohortSnapshotMap
      simpleCohorts = zipWith3 (\(cId, vars) (_, size) (_, subs) -> SimpleCohort cId vars size subs)
                      (getQueryVars cohortSnapshotMap) respSizes subscriberWsOpIds
      allSubscribers = concatMap snd $ subscriberWsOpIds
      pollerLog = PollerLog
                { _plPollerId = pollerId
                , _plSubscriberCount = length allSubscribers
                , _plCohortSize = length cohortSnapshotMap
                , _plCohorts = simpleCohorts
                , _plExecutionBatches = execBatches
                , _plExecutionBatchSize = length queryVarsBatches
                , _plSnapshotTime = snapshotTime
                , _plTotalTime = totalTime
                , _plGeneratedSql = unMultiplexedQuery pgQuery
                , _plLiveQueryOptions = lqOpts
                }
  L.unLogger logger pollerLog

  where
    timing :: (RefetchMetrics -> Metrics.Distribution) -> IO b -> IO (DiffTime, b)
    timing f m = do
      (dt, a) <- withElapsedTime m
      Metrics.add (f metrics) $ realToFrac dt
      return (dt, a)

    Poller cohortMap _ = handler
    LiveQueriesOptions batchSize _ = lqOpts

    getCohortSnapshot (cohortVars, handlerC) = do
      let Cohort resId respRef curOpsTV newOpsTV = handlerC
      curOpsL <- TMap.toList curOpsTV
      newOpsL <- TMap.toList newOpsTV
      forM_ newOpsL $ \(k, action) -> TMap.insert action k curOpsTV
      TMap.reset newOpsTV
      let cohortSnapshot = CohortSnapshot cohortVars respRef (map snd curOpsL) (map snd newOpsL)
      return (resId, cohortSnapshot)

    getSubscribers :: Map.HashMap CohortId CohortSnapshot -> [(CohortId, [UniqueSubscriberId])]
    getSubscribers cohortSnapshotMap =
      let getWsOpId v = map _sWebsocketOperationId $ _csExistingSubscribers v <> _csNewSubscribers v
      in Map.toList $ Map.map getWsOpId cohortSnapshotMap

    getQueryVars :: Map.HashMap CohortId CohortSnapshot -> [(CohortId, CohortVariables)]
    getQueryVars cohortSnapshotMap =
      Map.toList $ fmap _csVariables cohortSnapshotMap

    getCohortOperations cohortSnapshotMap actionMeta = \case
      Left e ->
        -- TODO: this is internal error
        let resp = GQExecError [encodeGQLErr False e]
        in [ (resp, (cId, Nothing), Nothing, actionMeta, snapshot)
           | (cId, snapshot) <- Map.toList cohortSnapshotMap
           ]
      Right responses ->
        flip mapMaybe responses $ \(respId, result) ->
          -- TODO: change it to use bytestrings directly
          let -- No reason to use lazy bytestrings here, since (1) we fetch the entire result set
              -- from Postgres strictly and (2) even if we didn’t, hashing will have to force the
              -- whole thing anyway.
              respBS = encJToBS result
              respHash = mkRespHash respBS
              respSize = BS.length respBS
          in (GQSuccess result, (respId, Just $! respSize), Just $! respHash, actionMeta,)
             <$> Map.lookup respId cohortSnapshotMap
