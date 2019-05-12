module Hasura.GraphQL.Execute.LiveQuery.Multiplexed
  ( BatchSize
  , mkBatchSize
  , RefetchInterval
  , refetchIntervalFromMilli
  , MxOpts
  , mkMxOpts

  , LiveQueriesState
  , initLiveQueriesState
  , dumpLiveQueriesState

  , MxOpCtx
  , mkMxOpCtx
  , MxOp

  , LiveQueryId
  , addLiveQuery
  , removeLiveQuery
  ) where

import           Data.List                              (unfoldr)
import           Data.Word                              (Word32)
import qualified ListT

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified Data.Aeson.Extended                    as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Time.Clock                        as Clock
import qualified Data.UUID                              as UUID
import qualified Data.UUID.V4                           as UUID
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified StmContainers.Map                      as STMMap
import qualified System.Metrics.Distribution            as Metrics

import           Control.Concurrent                     (threadDelay)

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Types
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Value

-- remove these when array encoding is merged
import qualified Database.PG.Query.PTI                  as PTI
import qualified PostgreSQL.Binary.Encoding             as PE

newtype RespId
  = RespId { unRespId :: UUID.UUID }
  deriving (Show, Eq, Hashable, Q.FromCol)

newtype RespIdList
  = RespIdList { unRespIdList :: [RespId] }
  deriving (Show, Eq)

instance Q.ToPrepArg RespIdList where
  toPrepVal (RespIdList l) =
    Q.toPrepValHelper PTI.unknown encoder $ map unRespId l
    where
      encoder =
        PE.array 2950 . PE.dimensionArray foldl'
        (PE.encodingArray . PE.uuid)

newRespId :: IO RespId
newRespId = RespId <$> UUID.nextRandom

data LQGroup
  -- we don't need operation name here as a subscription will
  -- only have a single top level field
  = LQGroup
  { _lgRole         :: !RoleName
  , _lgGQLQueryText :: !GQLQueryText
  } deriving (Show, Eq, Generic)

instance Hashable LQGroup

instance J.ToJSON LQGroup where
  toJSON (LQGroup role query) =
    J.object [ "role" J..= role
             , "query" J..= query
             ]

data MxOpts
  = MxOpts
  { _moBatchSize       :: !BatchSize
  , _moRefetchInterval :: !RefetchInterval
  } deriving (Show, Eq)

instance J.ToJSON MxOpts where
  toJSON (MxOpts batchSize refetchInterval) =
    J.object [ "batch_size" J..= batchSize
             , "refetch_delay" J..= refetchInterval
             ]

-- 1 second
defaultRefetchInterval :: RefetchInterval
defaultRefetchInterval =
  refetchIntervalFromMilli 1000

mkMxOpts
  :: Maybe BatchSize
  -> Maybe RefetchInterval
  -> MxOpts
mkMxOpts batchSizeM refetchIntervalM =
  MxOpts
  (fromMaybe defaultBatchSize batchSizeM)
  (fromMaybe defaultRefetchInterval refetchIntervalM)

data LiveQueriesState
  = LiveQueriesState
  { _lqsOptions      :: !MxOpts
  , _lqsLiveQueryMap :: !LiveQueryMap
  }

data RefetchMetrics
  = RefetchMetrics
  { _rmSnapshot :: !Metrics.Distribution
  , _rmPush     :: !Metrics.Distribution
  , _rmQuery    :: !Metrics.Distribution
  , _rmTotal    :: !Metrics.Distribution
  }

initRefetchMetrics :: IO RefetchMetrics
initRefetchMetrics =
  RefetchMetrics
  <$> Metrics.new
  <*> Metrics.new
  <*> Metrics.new
  <*> Metrics.new

data ThreadState
  = ThreadState
  { _tsThread  :: !(A.Async ())
  , _tsMetrics :: !RefetchMetrics
  }

type LiveQueryMap
  = STMMap.Map LQGroup (LQHandler, STM.TMVar ThreadState)

initLiveQueriesState
  :: MxOpts
  -> STM.STM LiveQueriesState
initLiveQueriesState lqOptions =
  LiveQueriesState
  lqOptions
  <$> STMMap.new

dumpLiveQueriesState :: Bool -> LiveQueriesState -> IO J.Value
dumpLiveQueriesState extended (LiveQueriesState opts lqMap) = do
  lqMapJ <- dumpLiveQueryMap extended lqMap
  return $ J.object
    [ "options" J..= opts
    , "live_queries_map" J..= lqMapJ
    ]

dumpLiveQueryMap :: Bool -> LiveQueryMap -> IO J.Value
dumpLiveQueryMap extended lqMap =
  fmap J.toJSON $ do
    entries <- STM.atomically $ ListT.toList $ STMMap.listT lqMap
    forM entries $ \(lq, (lqHandler, threadRef)) -> do
      ThreadState threadId metrics <-
        STM.atomically $ STM.readTMVar threadRef
      metricsJ <- dumpReftechMetrics metrics
      candidatesJ <-
        if extended
        then fmap Just $ dumpCandidates $ _mhCandidates lqHandler
        else return Nothing
      return $ J.object
        [ "key" J..= lq
        , "thread_id" J..= show (A.asyncThreadId threadId)
        , "alias" J..= _mhAlias lqHandler
        , "multiplexed_query" J..= Q.getQueryText (_mhQuery lqHandler)
        , "candidates" J..= candidatesJ
        , "metrics" J..= metricsJ
        ]
  where
    dumpReftechMetrics metrics = do
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
    dumpCandidates candidateMap = do
      candidates <- STM.atomically $ toListTMap candidateMap
      forM candidates $ \((usrVars, varVals), candidate) -> do
        candidateJ <- dumpCandidate candidate
        return $ J.object
          [ "session_vars" J..= usrVars
          , "variable_values" J..= varVals
          , "candidate" J..= candidateJ
          ]
    dumpCandidate (CandidateState respId _ respTV curOps newOps) =
      STM.atomically $ do
      prevResHash <- STM.readTVar respTV
      curOpIds <- toListTMap curOps
      newOpIds <- toListTMap newOps
      return $ J.object
        [ "resp_id" J..= unRespId respId
        , "current_ops" J..= map fst curOpIds
        , "new_ops" J..= map fst newOpIds
        , "previous_result_hash" J..= prevResHash
        ]

type ValidatedVariables = Map.HashMap G.Variable TxtEncodedPGVal

data LQHandler
  = LQHandler
  { _mhAlias      :: !G.Alias
  , _mhQuery      :: !Q.Query
  , _mhCandidates :: !(TMap CandidateId CandidateState)
  }

-- This type represents the state associated with
-- the response of (role, gqlQueryText, userVars, variableValues)
data CandidateState
  = CandidateState
  -- the laterally joined query responds with [(RespId, EncJSON)]
  -- so the resultid is used to determine the websockets
  -- where the data needs to be sent
  { _csRespId        :: !RespId

  -- query variables which are validated and text encoded
  , _csValidatedVars :: !ValidatedVariables

  -- we need to store the previous response
  , _csPrevRes       :: !RespTV

  -- the actions that have been run previously
  -- we run these if the response changes
  , _csCurOps        :: !Sinks

  -- we run these operations regardless
  -- and then merge them with current operations
  , _csNewOps        :: !Sinks
  }

-- the multiplexed query associated with the livequery
-- and the validated, text encoded query variables
data MxOpCtx
  = MxOpCtx
  { _mocGroup     :: !LQGroup
  , _mocAlias     :: !G.Alias
  , _mocQuery     :: !Q.Query
  }

instance J.ToJSON MxOpCtx where
  toJSON (MxOpCtx lqGroup als q) =
    J.object [ "query" J..= Q.getQueryText q
             , "alias" J..= als
             , "group" J..= lqGroup
             ]

type MxOp = (MxOpCtx, UserVars, ValidatedVariables)

mkMxOpCtx
  :: RoleName -> GQLQueryText
  -> G.Alias -> Q.Query
  -> MxOpCtx
mkMxOpCtx role queryTxt als query =
  MxOpCtx lqGroup als $ mkMxQuery query
  where
    lqGroup = LQGroup role queryTxt

mkMxQuery :: Q.Query -> Q.Query
mkMxQuery baseQuery =
  Q.fromText $ mconcat $ map Q.getQueryText $
      [mxQueryPfx, baseQuery, mxQuerySfx]
  where
    mxQueryPfx :: Q.Query
    mxQueryPfx =
      [Q.sql|
        select
          _subs.result_id, _fld_resp.root as result
          from
            unnest(
              $1::uuid[], $2::json[]
            ) _subs (result_id, result_vars)
          left outer join lateral
            (
        |]

    mxQuerySfx :: Q.Query
    mxQuerySfx =
      [Q.sql|
            ) _fld_resp ON ('true')
        |]

data LiveQueryId
  = LiveQueryId
  { _lqiGroup     :: !LQGroup
  , _lqiCandidate :: !CandidateId
  , _lqiSink      :: !SinkId
  }

addLiveQuery
  :: PGExecCtx
  -> LiveQueriesState
  -- the query
  -> MxOp
  -- the action to be executed when result changes
  -> OnChange
  -> IO LiveQueryId
addLiveQuery pgExecCtx lqState (mxOpCtx, usrVars, valQVars) onResultAction = do

  -- generate a new result id
  responseId <- newRespId

  -- generate a new sink id
  sinkId <- newSinkId

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $ do
    handlerM <- STMMap.lookup handlerId lqMap
    case handlerM of
      Just (handler, _) -> do
        candidateM <- lookupTMap candidateId $ _mhCandidates handler
        case candidateM of
          Just candidate -> addToExistingCandidate sinkId candidate
          Nothing        -> addToExistingHandler sinkId responseId handler
        return Nothing
      Nothing -> do
        handler <- newHandler sinkId responseId
        asyncRefTM <- STM.newEmptyTMVar
        STMMap.insert (handler, asyncRefTM) handlerId lqMap
        return $ Just (handler, asyncRefTM)

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \(handler, pollerThreadTM) -> do
    metrics <- initRefetchMetrics
    threadRef <- A.async $ forever $ do
      pollQuery metrics batchSize pgExecCtx handler
      threadDelay $ refetchIntervalToMicro refetchInterval
    let threadState = ThreadState threadRef metrics
    STM.atomically $ STM.putTMVar pollerThreadTM threadState

  return $ LiveQueryId handlerId candidateId sinkId

  where

    MxOpCtx handlerId als mxQuery = mxOpCtx
    LiveQueriesState lqOpts lqMap = lqState
    MxOpts batchSize refetchInterval = lqOpts

    candidateId = (usrVars, valQVars)

    addToExistingCandidate sinkId handlerC =
      insertTMap onResultAction sinkId $ _csNewOps handlerC

    newHandlerC sinkId responseId = do
      handlerC <- CandidateState
                  responseId
                  valQVars
                  <$> STM.newTVar Nothing
                  <*> newTMap
                  <*> newTMap
      insertTMap onResultAction sinkId $ _csNewOps handlerC
      return handlerC

    addToExistingHandler sinkId responseId handler = do
      handlerC <- newHandlerC sinkId responseId
      insertTMap handlerC candidateId $ _mhCandidates handler

    newHandler sinkId responseId = do
      handler <- LQHandler als mxQuery <$> newTMap
      handlerC <- newHandlerC sinkId responseId
      insertTMap handlerC candidateId $ _mhCandidates handler
      return handler

type CandidateId = (UserVars, ValidatedVariables)

removeLiveQuery
  :: LiveQueriesState
  -- the query and the associated operation
  -> LiveQueryId
  -> IO ()
removeLiveQuery lqState (LiveQueryId handlerId candidateId sinkId) = do
  threadRefM <- STM.atomically $ do
   detM <- getQueryDet
   fmap join $ forM detM $
    \(handler, threadRef, candidate) ->
      cleanHandlerC (_mhCandidates handler) threadRef candidate
  onJust threadRefM (A.cancel . _tsThread)

  where
    lqMap = _lqsLiveQueryMap lqState

    getQueryDet = do
      handlerM <- STMMap.lookup handlerId lqMap
      fmap join $ forM handlerM $ \(handler, threadRef) -> do
        let LQHandler _ _ candidateMap = handler
        candidateM <- lookupTMap candidateId candidateMap
        return $ fmap (handler, threadRef,) candidateM

    cleanHandlerC candidateMap threadRef handlerC = do
      let curOps = _csCurOps handlerC
          newOps = _csNewOps handlerC
      deleteTMap sinkId curOps
      deleteTMap sinkId newOps
      candidateIsEmpty <- (&&)
        <$> nullTMap curOps
        <*> nullTMap newOps
      when candidateIsEmpty $ deleteTMap candidateId candidateMap
      handlerIsEmpty <- nullTMap candidateMap
      -- when there is no need for handler
      -- i.e, this happens to be the last operation, take the
      -- ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId lqMap
          Just <$> STM.takeTMVar threadRef
        else return Nothing

data CandidateSnapshot
  = CandidateSnapshot
  { _csRespVars   :: !RespVars
  , _csPrevResRef :: !RespTV
  , _csCurSinks   :: ![OnChange]
  , _csNewSinks   :: ![OnChange]
  }

pushCandidateResult :: GQResp -> Maybe RespHash -> CandidateSnapshot -> IO ()
pushCandidateResult resp respHashM candidateSnapshot = do
  prevRespHashM <- STM.readTVarIO respRef
  -- write to the current websockets if needed
  sinks <-
    if (isExecError resp || respHashM /= prevRespHashM)
    then do
      STM.atomically $ STM.writeTVar respRef respHashM
      return (newSinks <> curSinks)
    else
      return newSinks
  pushResultToSinks sinks
  where
    CandidateSnapshot _ respRef curSinks newSinks = candidateSnapshot
    pushResultToSinks =
      A.mapConcurrently_ (\action -> action resp)

type RespVars = J.Value

newtype RespVarsList
  = RespVarsList { _unRespVarsList :: [RespVars]}

instance Q.ToPrepArg RespVarsList where
  toPrepVal (RespVarsList l) =
    Q.toPrepValHelper PTI.unknown encoder l
    where
      encoder =
        PE.array 114 . PE.dimensionArray foldl'
        (PE.encodingArray . PE.json_ast)

getRespVars :: UserVars -> ValidatedVariables -> RespVars
getRespVars usrVars valVars =
  J.object [ "user" J..= usrVars
           , "variables" J..= valVars
           ]

newtype BatchSize
  = BatchSize { unBatchSize :: Word32 }
  deriving (Show, Eq, J.ToJSON)

mkBatchSize :: Word32 -> BatchSize
mkBatchSize = BatchSize

defaultBatchSize :: BatchSize
defaultBatchSize =
  BatchSize 100

chunks :: Word32 -> [a] -> [[a]]
chunks n =
  takeWhile (not.null) . unfoldr (Just . splitAt (fromIntegral n))

pollQuery
  :: RefetchMetrics
  -> BatchSize
  -> PGExecCtx
  -> LQHandler
  -> IO ()
pollQuery metrics batchSize pgExecCtx handler = do

  procInit <- Clock.getCurrentTime

  -- get a snapshot of all the candidates
  -- this need not be done in a transaction
  candidates <- STM.atomically $ toListTMap candidateMap
  candidateSnapshotMap <-
    fmap Map.fromList $
    mapM (STM.atomically . getCandidateSnapshot) candidates

  let queryVarsBatches = chunks (unBatchSize batchSize) $
                        getQueryVars candidateSnapshotMap

  snapshotFinish <- Clock.getCurrentTime
  Metrics.add (_rmSnapshot metrics) $
    realToFrac $ Clock.diffUTCTime snapshotFinish procInit
  flip A.mapConcurrently_ queryVarsBatches $ \queryVars -> do
    queryInit <- Clock.getCurrentTime
    mxRes <- runExceptT $ runLazyTx' pgExecCtx $
             liftTx $ Q.listQE defaultTxErrorHandler
             pgQuery (mkMxQueryPrepArgs queryVars) True
    queryFinish <- Clock.getCurrentTime
    Metrics.add (_rmQuery metrics) $
      realToFrac $ Clock.diffUTCTime queryFinish queryInit
    let operations = getCandidateOperations candidateSnapshotMap mxRes
    -- concurrently push each unique result
    A.mapConcurrently_ (uncurry3 pushCandidateResult) operations
    pushFinish <- Clock.getCurrentTime
    Metrics.add (_rmPush metrics) $
      realToFrac $ Clock.diffUTCTime pushFinish queryFinish
  procFinish <- Clock.getCurrentTime
  Metrics.add (_rmTotal metrics) $
    realToFrac $ Clock.diffUTCTime procFinish procInit

  where
    LQHandler alias pgQuery candidateMap = handler
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (a, b, c) = f a b c

    getCandidateSnapshot ((usrVars, _), handlerC) = do
      let CandidateState resId valVars respRef curOpsTV newOpsTV = handlerC
      curOpsL <- toListTMap curOpsTV
      newOpsL <- toListTMap newOpsTV
      forM_ newOpsL $ \(k, action) -> insertTMap action k curOpsTV
      resetTMap newOpsTV
      let resultVars = getRespVars usrVars valVars
          candidateSnapshot = CandidateSnapshot resultVars respRef
                              (map snd curOpsL) (map snd newOpsL)
      return (resId, candidateSnapshot)

    getQueryVars candidateSnapshotMap =
      Map.toList $ fmap _csRespVars candidateSnapshotMap

    mkMxQueryPrepArgs l =
      let (respIdL, respVarL) = unzip l
      in (RespIdList respIdL, RespVarsList respVarL)

    getCandidateOperations candidateSnapshotMap = \case
      Left e ->
        -- TODO: this is internal error
        let resp = GQExecError [encodeGQErr False e]
        in [ (resp, Nothing, snapshot)
           | (_, snapshot) <- Map.toList candidateSnapshotMap
           ]
      Right responses ->
        flip mapMaybe responses $ \(respId, respEnc) ->
          -- TODO: change it to use bytestrings directly

          let fldAlsT = G.unName $ G.unAlias alias
              respLbs = encJToLBS $ encJFromAssocList $
                        pure $ (,) fldAlsT respEnc
              resp = GQSuccess respLbs
              respHash = mkRespHash respLbs
          in (resp, Just respHash,) <$> Map.lookup respId candidateSnapshotMap
