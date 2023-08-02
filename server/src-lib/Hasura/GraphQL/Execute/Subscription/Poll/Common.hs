-- | Multiplexed subscription poller threads; see "Hasura.GraphQL.Execute.Subscription" for details.
module Hasura.GraphQL.Execute.Subscription.Poll.Common
  ( -- * Pollers
    Poller (..),
    PollerId (..),
    PollerIOState (..),
    PollerKey (..),
    BackendPollerKey (..),
    PollerMap,
    dumpPollerMap,
    PollDetailsError (..),
    PollDetails (..),
    BatchExecutionDetails (..),
    CohortExecutionDetails (..),
    SubscriptionPostPollHook,
    defaultSubscriptionPostPollHook,
    PollerResponseState (..),

    -- * Cohorts
    Cohort (..),
    CohortSnapshot (..),
    CursorVariableValues (..),
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
    SubscriptionGQResponse,
    SubscriptionResponse (..),
    SubscriptionMetadata (..),
    SubscriberExecutionDetails (..),

    -- * Batch
    BatchId (..),

    -- * Hash
    ResponseHash (..),
    mkRespHash,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Immortal qualified as Immortal
import Crypto.Hash qualified as CH
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Text (unpack)
import Data.Time.Clock qualified as Clock
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasura.Base.Error (QErr, showQErr)
import Hasura.GraphQL.Execute.Subscription.Options
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (SourceName)
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Subscription (SubscriptionType)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types (RequestId)
import ListT qualified
import StmContainers.Map qualified as STMMap

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
  SubscriberMetadata
    $ J.object
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

-- | Subscription onChange metadata, used for adding more extra analytics data
data SubscriptionMetadata = SubscriptionMetadata
  { _sqmExecutionTime :: !Clock.DiffTime
  }

data SubscriptionResponse = SubscriptionResponse
  { _lqrPayload :: !BS.ByteString,
    _lqrExecutionTime :: !Clock.DiffTime
  }

type SubscriptionGQResponse = GQResult SubscriptionResponse

type OnChange = SubscriptionGQResponse -> IO ()

type SubscriberMap = TMap.TMap SubscriberId Subscriber

data PollerResponseState
  = PRSSuccess
  | PRSError
  deriving (Eq)

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
    _cCohortId :: CohortId,
    -- | Contains a hash of the previous poll's DB query result, if any, used to determine
    --   if we need to push an updated result to the subscribers or not.
    _cPreviousResponse :: STM.TVar (Maybe ResponseHash),
    -- | the subscribers we’ve already pushed a result to; we push new results to them if the
    -- response changes
    _cExistingSubscribers :: SubscriberMap,
    -- | subscribers we haven’t yet pushed any results to; we push results to them regardless if the
    -- result changed, then merge them in the map of existing subscribers
    _cNewSubscribers :: SubscriberMap,
    -- | a mutable type which holds the latest value of the subscription stream cursor. In case
    --   of live query subscription, this field is ignored by setting `streamCursorVars` to `()`
    _cStreamCursorVariables :: streamCursorVars
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
type CohortMap streamCursor = TMap.TMap CohortKey (Cohort streamCursor)

dumpCohortMap :: CohortMap streamCursor -> IO J.Value
dumpCohortMap cohortMap = do
  cohorts <- STM.atomically $ TMap.toList cohortMap
  fmap J.toJSON . forM cohorts $ \(variableValues, cohort) -> do
    cohortJ <- dumpCohort cohort
    return
      $ J.object
        [ "variables" J..= variableValues,
          "cohort" J..= cohortJ
        ]
  where
    dumpCohort (Cohort respId respTV curOps newOps _) =
      STM.atomically $ do
        prevResHash <- STM.readTVar respTV
        curOpIds <- TMap.toList curOps
        newOpIds <- TMap.toList newOps
        return
          $ J.object
            [ "resp_id" J..= respId,
              "current_ops" J..= map fst curOpIds,
              "new_ops" J..= map fst newOpIds,
              "previous_result_hash" J..= prevResHash
            ]

data CohortSnapshot = CohortSnapshot
  { _csVariables :: CohortVariables,
    _csPreviousResponse :: STM.TVar (Maybe ResponseHash),
    _csExistingSubscribers :: [Subscriber],
    _csNewSubscribers :: [Subscriber]
  }

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
  { _pCohorts :: CohortMap streamCursor,
    _pPollerState :: STM.TVar PollerResponseState,
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
    _pIOState :: STM.TMVar PollerIOState,
    _pParameterizedQueryHash :: ParameterizedQueryHash,
    -- The operation names of the subscriptions that are part of this poller. This is
    -- used while emitting subscription metrics
    _pOperationNamesMap :: TMap.TMap (Maybe OperationName) Int
  }

data PollerIOState = PollerIOState
  { -- | a handle on the poller’s worker thread that can be used to
    -- 'Immortal.stop' it if all its cohorts stop listening
    _pThread :: !Immortal.Thread,
    _pId :: !PollerId
  }

data PollerKey b =
  -- we don't need operation name here as a subscription will only have a
  -- single top level field
  PollerKey
  { _lgSource :: SourceName,
    _lgRole :: RoleName,
    _lgQuery :: Text,
    _lgConnectionKey :: (ResolvedConnectionTemplate b),
    _lgParameterizedQueryHash :: ParameterizedQueryHash
  }
  deriving (Generic)

deriving instance (Backend b) => Show (PollerKey b)

deriving instance (Backend b) => Eq (PollerKey b)

instance (Backend b) => Hashable (PollerKey b)

instance J.ToJSON (PollerKey b) where
  toJSON (PollerKey source role query _connectionKey _) =
    J.object
      [ "source" J..= source,
        "role" J..= role,
        "query" J..= query
      ]

newtype BackendPollerKey = BackendPollerKey {unBackendPollerKey :: AB.AnyBackend PollerKey}
  deriving (Eq, Show, Hashable)

type PollerMap streamCursor = STMMap.Map BackendPollerKey (Poller streamCursor)

-- | For dev debugging, output subject to change.
dumpPollerMap :: Bool -> PollerMap streamCursor -> IO J.Value
dumpPollerMap extended pollerMap =
  fmap J.toJSON $ do
    entries <- STM.atomically $ ListT.toList $ STMMap.listT pollerMap
    forM entries $ \(pollerKey, Poller cohortsMap _responseState ioState _paramQueryHash _opNames) ->
      AB.dispatchAnyBackend @Backend (unBackendPollerKey pollerKey) $ \(PollerKey source role query _connectionKey _) -> do
        PollerIOState threadId pollerId <- STM.atomically $ STM.readTMVar ioState
        cohortsJ <-
          if extended
            then Just <$> dumpCohortMap cohortsMap
            else return Nothing
        return
          $ J.object
            [ "source" J..= source,
              "role" J..= role,
              "thread_id" J..= show (Immortal.threadId threadId),
              "poller_id" J..= pollerId,
              "multiplexed_query" J..= query,
              "cohorts" J..= cohortsJ,
              "parameterized_query_hash" J..= _paramQueryHash
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
  { -- | postgres execution time of each batch ('Nothing' in case of non-PG dbs)
    _bedPgExecutionTime :: Maybe Clock.DiffTime,
    -- | database execution time of each batch
    _bedDbExecutionTime :: Clock.DiffTime,
    -- | time to taken to push to all cohorts belonging to this batch
    _bedPushTime :: Clock.DiffTime,
    -- | id of the batch
    _bedBatchId :: BatchId,
    -- | execution details of the cohorts belonging to this batch
    _bedCohorts :: [CohortExecutionDetails],
    _bedBatchResponseSizeBytes :: Maybe Int
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
      pgExecTime =
        maybe
          mempty
          (\execTime -> ["pg_execution_time" J..= execTime])
          _bedPgExecutionTime
   in J.object
        ( [ "db_execution_time" J..= _bedDbExecutionTime,
            "push_time" J..= _bedPushTime,
            "batch_id" J..= _bedBatchId
          ]
            -- log pg exec time only when its not 'Nothing'
            <> pgExecTime
            -- log batch resp size only when there are no errors
            <> batchRespSize
        )

data PollDetailsError = PollDetailsError
  { _pdeBatchId :: BatchId,
    _pdeErrorDetails :: QErr
  }
  deriving (Eq)

instance Show PollDetailsError where
  show pde = "batch_id = " ++ show (_pdeBatchId pde) ++ ", detail = " ++ unpack (showQErr $ _pdeErrorDetails pde)

instance J.ToJSON PollDetailsError where
  toJSON PollDetailsError {..} =
    J.object
      $ [ "batch_id" J..= _pdeBatchId,
          "detail" J..= _pdeErrorDetails
        ]

-- TODO consider refactoring into two types: one that is returned from pollLiveQuery and pollStreamingQuery, and a parent type containing pollerId, sourceName, and so on, which is assembled at the callsites of those two functions. Move postPollHook out of those functions to callsites
data PollDetails = PollDetails
  { -- | the unique ID (basically a thread that run as a 'Poller') for the
    -- 'Poller'
    _pdPollerId :: PollerId,
    -- | distinguish between the subscription type (i.e. live-query or streaming
    -- subscription)
    _pdKind :: SubscriptionType,
    -- | the multiplexed SQL query to be run against the database with all the
    -- variables together
    _pdGeneratedSql :: Text,
    -- | the time taken to get a snapshot of cohorts from our 'SubscriptionsState'
    -- data structure
    _pdSnapshotTime :: Clock.DiffTime,
    -- | list of execution batches and their details
    _pdBatches :: [BatchExecutionDetails],
    -- | total time spent on a poll cycle
    _pdTotalTime :: Clock.DiffTime,
    _pdLiveQueryOptions :: SubscriptionsOptions,
    _pdSource :: SourceName,
    _pdRole :: RoleName,
    _pdParameterizedQueryHash :: ParameterizedQueryHash,
    _pdLogLevel :: L.LogLevel,
    _pdErrors :: Maybe [PollDetailsError]
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
    $ [ "poller_id" J..= _pdPollerId,
        "kind" J..= _pdKind,
        "snapshot_time" J..= _pdSnapshotTime,
        "batches" J..= batches, -- TODO: deprecate this field
        "execution_batches" J..= batches,
        "subscriber_count" J..= sum (map (length . _bedCohorts) _pdBatches),
        "total_time" J..= _pdTotalTime,
        "source" J..= _pdSource,
        "generated_sql" J..= _pdGeneratedSql,
        "role" J..= _pdRole,
        "subscription_options" J..= _pdLiveQueryOptions
      ]
    <> maybe [] (\err -> ["errors" J..= err]) _pdErrors
  where
    batches = map batchExecutionDetailMinimal _pdBatches

instance L.ToEngineLog PollDetails L.Hasura where
  toEngineLog pl = (_pdLogLevel pl, L.ELTLivequeryPollerLog, pollDetailMinimal pl)

type SubscriptionPostPollHook = PollDetails -> IO ()

-- the default SubscriptionPostPollHook
defaultSubscriptionPostPollHook :: L.Logger L.Hasura -> SubscriptionPostPollHook
defaultSubscriptionPostPollHook = \x -> L.unLogger x
