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

  , PollDetails(..)
  , BatchExecutionDetails(..)
  , CohortExecutionDetails(..)

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
  , newSubscriberId
  , SubscriberMetadata
  , mkSubscriberMetadata
  , SubscriberMap
  , OnChange
  , LGQResponse
  , LiveQueryResponse(..)
  , LiveQueryMetadata(..)
  ) where

import           Data.List.Split                          (chunksOf)
import           GHC.AssertNF
import           Hasura.Prelude

import qualified Control.Concurrent.Async                 as A
import qualified Control.Concurrent.STM                   as STM
import qualified Control.Immortal                         as Immortal
import qualified Crypto.Hash                              as CH
import qualified Data.Aeson.Casing                        as J
import qualified Data.Aeson.Extended                      as J
import qualified Data.Aeson.TH                            as J
import qualified Data.ByteString                          as BS
import qualified Data.HashMap.Strict                      as Map
import qualified Data.Time.Clock                          as Clock
import qualified Data.UUID                                as UUID
import qualified Data.UUID.V4                             as UUID
import qualified Database.PG.Query                        as Q
import qualified ListT
import qualified StmContainers.Map                        as STMMap

import           Control.Lens

import qualified Hasura.GraphQL.Execute.LiveQuery.TMap    as TMap
import qualified Hasura.Logging                           as L

import           Hasura.Db
import           Hasura.GraphQL.Execute.LiveQuery.Options
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types.Error
import           Hasura.Session

-- ----------------------------------------------------------------------------------------------
-- Subscribers

newtype SubscriberId
  = SubscriberId { unSubscriberId :: UUID.UUID }
  deriving (Show, Eq, Generic, Hashable, J.ToJSON)

newSubscriberId :: IO SubscriberId
newSubscriberId =
  SubscriberId <$> UUID.nextRandom

-- | Allows a user of the live query subsystem (currently websocket transport)
-- to attach arbitrary metadata about a subscriber. This information is available
-- as part of Subscriber in CohortExecutionDetails and can be logged by customizing
-- in pollerlog
newtype SubscriberMetadata
  = SubscriberMetadata { unSubscriberMetadata :: J.Value }
  deriving (Show, Eq, J.ToJSON)

mkSubscriberMetadata :: J.Value -> SubscriberMetadata
mkSubscriberMetadata = SubscriberMetadata

data Subscriber
  = Subscriber
  { _sId               :: !SubscriberId
  , _sMetadata         :: !SubscriberMetadata
  , _sOnChangeCallback :: !OnChange
  }

-- | live query onChange metadata, used for adding more extra analytics data
data LiveQueryMetadata
  = LiveQueryMetadata
  { _lqmExecutionTime :: !Clock.DiffTime
  }

data LiveQueryResponse
  = LiveQueryResponse
  { _lqrPayload       :: !BS.ByteString
  , _lqrExecutionTime :: !Clock.DiffTime
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

-- | A key we use to determine if two 'Subscriber's belong in the same 'Cohort'
-- (assuming they already meet the criteria to be in the same 'Poller'). Note
-- the distinction between this and 'CohortId'; the latter is a completely
-- synthetic key used only to identify the cohort in the generated SQL query.
type CohortKey = CohortVariables
-- | This has the invariant, maintained in 'removeLiveQuery', that it contains
-- no 'Cohort' with zero total (existing + new) subscribers.
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
  :: GQResult BS.ByteString
  -> Maybe ResponseHash
  -> LiveQueryMetadata
  -> CohortSnapshot
  -> IO ( [(SubscriberId, SubscriberMetadata)], [(SubscriberId, SubscriberMetadata)])
  -- ^ subscribers to which data has been pushed, subscribers which already
  -- have this data (this information is exposed by metrics reporting)
pushResultToCohort result !respHashM (LiveQueryMetadata dTime) cohortSnapshot = do
  prevRespHashM <- STM.readTVarIO respRef
  -- write to the current websockets if needed
  (subscribersToPush, subscribersToIgnore) <-
    if isExecError result || respHashM /= prevRespHashM
    then do
      $assertNFHere respHashM  -- so we don't write thunks to mutable vars
      STM.atomically $ STM.writeTVar respRef respHashM
      return (newSinks <> curSinks, mempty)
    else
      return (newSinks, curSinks)
  pushResultToSubscribers subscribersToPush
  pure $ over (each.each) (\Subscriber{..} -> (_sId, _sMetadata))
         (subscribersToPush, subscribersToIgnore)
  where
    CohortSnapshot _ respRef curSinks newSinks = cohortSnapshot
    response = result <&> \payload -> LiveQueryResponse payload dTime
    pushResultToSubscribers =
      A.mapConcurrently_ $ \(Subscriber _ _ action) -> action response

-- -----------------------------------------------------------------------------
-- Pollers

-- | A unique, multiplexed query. Each 'Poller' has its own polling thread that
-- periodically polls Postgres and pushes results to each of its listening
-- 'Cohort's.
--
-- In SQL, an 'Poller' corresponds to a single, multiplexed query, though in
-- practice, 'Poller's with large numbers of 'Cohort's are batched into
-- multiple concurrent queries for performance reasons.
data Poller
  = Poller
  { _pCohorts :: !CohortMap
  , _pIOState :: !(STM.TMVar PollerIOState)
  -- ^ This is in a separate 'STM.TMVar' because it’s important that we are
  -- able to construct 'Poller' values in 'STM.STM' --- we need the insertion
  -- into the 'PollerMap' to be atomic to ensure that we don’t accidentally
  -- create two for the same query due to a race. However, we can’t spawn the
  -- worker thread or create the metrics store in 'STM.STM', so we insert it
  -- into the 'Poller' only after we’re certain we won’t create any duplicates.
  --
  -- This var is "write once", moving monotonically from empty to full.
  -- TODO this could probably be tightened up to something like
  -- 'STM PollerIOState'
  }

data PollerIOState
  = PollerIOState
  { _pThread :: !Immortal.Thread
  -- ^ a handle on the poller’s worker thread that can be used to
  -- 'Immortal.stop' it if all its cohorts stop listening
  , _pId     :: !PollerId
  }

data PollerKey
  -- we don't need operation name here as a subscription will only have a
  -- single top level field
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
      PollerIOState threadId pollerId <- STM.atomically $ STM.readTMVar ioState
      cohortsJ <-
        if extended
        then Just <$> dumpCohortMap cohortsMap
        else return Nothing
      return $ J.object
        [ "role" J..= role
        , "thread_id" J..= show (Immortal.threadId threadId)
        , "poller_id" J..= pollerId
        , "multiplexed_query" J..= query
        , "cohorts" J..= cohortsJ
        ]

-- | An ID to track unique 'Poller's, so that we can gather metrics about each
-- poller
newtype PollerId = PollerId { unPollerId :: UUID.UUID }
  deriving (Show, Eq, Generic, J.ToJSON)

-- | Execution information related to a cohort on a poll cycle
data CohortExecutionDetails
  = CohortExecutionDetails
  { _cedCohortId     :: !CohortId
  , _cedVariables    :: !CohortVariables
  , _cedResponseSize :: !(Maybe Int)
  -- ^ Nothing in case of an error
  , _cedPushedTo     :: ![(SubscriberId, SubscriberMetadata)]
  -- ^ The response on this cycle has been pushed to these above subscribers
  -- New subscribers (those which haven't been around during the previous poll
  -- cycle) will always be part of this
  , _cedIgnored      :: ![(SubscriberId, SubscriberMetadata)]
  -- ^ The response on this cycle has *not* been pushed to these above
  -- subscribers. This would when the response hasn't changed from the previous
  -- polled cycle
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''CohortExecutionDetails)

-- | Execution information related to a single batched execution
data BatchExecutionDetails
  = BatchExecutionDetails
  { _bedPgExecutionTime :: !Clock.DiffTime
  -- ^ postgres execution time of each batch
  , _bedPushTime        :: !Clock.DiffTime
  -- ^ time to taken to push to all cohorts belonging to this batch
  , _bedCohorts         :: ![CohortExecutionDetails]
  -- ^ execution details of the cohorts belonging to this batch
  } deriving (Show, Eq)

-- | see Note [Minimal LiveQuery Poller Log]
batchExecutionDetailMinimal :: BatchExecutionDetails -> J.Value
batchExecutionDetailMinimal BatchExecutionDetails{..} =
  J.object [ "pg_execution_time" J..= _bedPgExecutionTime
           , "push_time" J..= _bedPushTime
           ]

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''BatchExecutionDetails)

data PollDetails
  = PollDetails
  { _pdPollerId         :: !PollerId
  -- ^ the unique ID (basically a thread that run as a 'Poller') for the
  -- 'Poller'
  , _pdGeneratedSql     :: !Q.Query
  -- ^ the multiplexed SQL query to be run against Postgres with all the
  -- variables together
  , _pdSnapshotTime     :: !Clock.DiffTime
  -- ^ the time taken to get a snapshot of cohorts from our 'LiveQueriesState'
  -- data structure
  , _pdBatches          :: ![BatchExecutionDetails]
  -- ^ list of execution batches and their details
  , _pdTotalTime        :: !Clock.DiffTime
  -- ^ total time spent on a poll cycle
  , _pdLiveQueryOptions :: !LiveQueriesOptions
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''PollDetails)

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
pollDetailMinimal (PollDetails{..}) =
  J.object [ "poller_id" J..= _pdPollerId
           , "snapshot_time" J..= _pdSnapshotTime
           , "batches" J..= (map batchExecutionDetailMinimal _pdBatches)
           , "total_time" J..= _pdTotalTime
           ]

instance L.ToEngineLog PollDetails L.Hasura where
  toEngineLog pl = (L.LevelInfo, L.ELTLivequeryPollerLog, pollDetailMinimal pl)

-- | Where the magic happens: the top-level action run periodically by each
-- active 'Poller'. This needs to be async exception safe.
pollQuery
  :: L.Logger L.Hasura
  -> PollerId
  -> LiveQueriesOptions
  -> PGExecCtx
  -> MultiplexedQuery
  -> CohortMap
  -> IO ()
pollQuery logger pollerId lqOpts pgExecCtx pgQuery cohortMap = do
  (totalTime, (snapshotTime, batchesDetails)) <- withElapsedTime $ do

    -- snapshot the current cohorts and split them into batches
    (snapshotTime, cohortBatches) <- withElapsedTime $ do
      -- get a snapshot of all the cohorts
      -- this need not be done in a transaction
      cohorts <- STM.atomically $ TMap.toList cohortMap
      cohortSnapshots <- mapM (STM.atomically . getCohortSnapshot) cohorts
      -- cohorts are broken down into batches specified by the batch size
      pure $ chunksOf (unBatchSize batchSize) cohortSnapshots

    -- concurrently process each batch
    batchesDetails <- A.forConcurrently cohortBatches $ \cohorts -> do
      (queryExecutionTime, mxRes) <- withElapsedTime $
        runExceptT $ runQueryTx pgExecCtx $
          executeMultiplexedQuery pgQuery $ over (each._2) _csVariables cohorts

      let lqMeta = LiveQueryMetadata $ convertDuration queryExecutionTime
          operations = getCohortOperations cohorts mxRes

      (pushTime, cohortsExecutionDetails) <- withElapsedTime $
        A.forConcurrently operations $ \(res, cohortId, respData, snapshot) -> do
          (pushedSubscribers, ignoredSubscribers) <-
            pushResultToCohort res (fst <$> respData) lqMeta snapshot
          pure CohortExecutionDetails
            { _cedCohortId = cohortId
            , _cedVariables = _csVariables snapshot
            , _cedPushedTo = pushedSubscribers
            , _cedIgnored = ignoredSubscribers
            , _cedResponseSize = snd <$> respData
            }
      pure $ BatchExecutionDetails queryExecutionTime pushTime cohortsExecutionDetails

    pure (snapshotTime, batchesDetails)

  L.unLogger logger $ PollDetails
                      { _pdPollerId = pollerId
                      , _pdGeneratedSql = unMultiplexedQuery pgQuery
                      , _pdSnapshotTime = snapshotTime
                      , _pdBatches = batchesDetails
                      , _pdLiveQueryOptions = lqOpts
                      , _pdTotalTime = totalTime
                      }
  where
    LiveQueriesOptions batchSize _ = lqOpts

    getCohortSnapshot (cohortVars, handlerC) = do
      let Cohort resId respRef curOpsTV newOpsTV = handlerC
      curOpsL <- TMap.toList curOpsTV
      newOpsL <- TMap.toList newOpsTV
      forM_ newOpsL $ \(k, action) -> TMap.insert action k curOpsTV
      TMap.reset newOpsTV
      let cohortSnapshot = CohortSnapshot cohortVars respRef (map snd curOpsL) (map snd newOpsL)
      return (resId, cohortSnapshot)

    getCohortOperations cohorts = \case
      Left e ->
        -- TODO: this is internal error
        let resp = GQExecError [encodeGQLErr False e]
        in [(resp, cohortId, Nothing, snapshot) | (cohortId, snapshot) <- cohorts]
      Right responses -> do
        let cohortSnapshotMap = Map.fromList cohorts
        flip mapMaybe responses $ \(cohortId, respBS) ->
          let respHash = mkRespHash respBS
              respSize = BS.length respBS
          -- TODO: currently we ignore the cases when the cohortId from
          -- Postgres response is not present in the cohort map of this batch
          -- (this shouldn't happen but if it happens it means a logic error and
          -- we should log it)
          in (GQSuccess respBS, cohortId, Just $!(respHash, respSize),) <$>
             Map.lookup cohortId cohortSnapshotMap
