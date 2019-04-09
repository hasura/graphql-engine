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

  , MxOp
  , addLiveQuery
  , removeLiveQuery
  , mkMxQuery
  ) where

import           Data.List                              (unfoldr)
import           Data.Word                              (Word32)

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified Data.Aeson.Extended                    as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.UUID                              as UUID
import qualified Data.UUID.V4                           as UUID
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified ListT
import qualified StmContainers.Map                      as STMMap

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

data MLiveQueryId
  -- we don't need operation name here as a subscription will
  -- only have a single top level field
  = MLiveQueryId
  { _mlqRole         :: !RoleName
  , _mlqGQLQueryText :: !GQLQueryText
  } deriving (Show, Eq, Generic)

instance Hashable MLiveQueryId

instance J.ToJSON MLiveQueryId where
  toJSON (MLiveQueryId role query) =
    J.object [ "role" J..= role
             , "query" J..= query
             ]

data MxOpts
  = MxOpts
  { _moBatchSize    :: !BatchSize
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

data LiveQueriesState k
  = LiveQueriesState
  { _lqsOptions      :: !MxOpts
  , _lqsLiveQueryMap :: !(LiveQueryMap k)
  }

type LiveQueryMap k = STMMap.Map MLiveQueryId (MLQHandler k, ThreadTM)

initLiveQueriesState
  :: MxOpts
  -> STM.STM (LiveQueriesState k)
initLiveQueriesState lqOptions =
  LiveQueriesState
  lqOptions
  <$> STMMap.new

dumpLiveQueriesState :: (J.ToJSON k) => LiveQueriesState k -> IO J.Value
dumpLiveQueriesState (LiveQueriesState opts lqMap) = do
  lqMapJ <- dumpLiveQueryMap lqMap
  return $ J.object
    [ "options" J..= opts
    , "live_queries_map" J..= lqMapJ
    ]

dumpLiveQueryMap :: (J.ToJSON k) => LiveQueryMap k -> IO J.Value
dumpLiveQueryMap lqMap =
  fmap J.toJSON $ STM.atomically $ do
    entries <- ListT.toList $ STMMap.listT lqMap
    forM entries $ \(lq, (lqHandler, threadRef)) -> do
      threadId <- A.asyncThreadId <$> STM.readTMVar threadRef
      candidatesJ <- dumpCandidates $ _mhCandidates lqHandler
      return $ J.object
        [ "key" J..= lq
        , "thread_id" J..= show threadId
        , "alias" J..= _mhAlias lqHandler
        , "multiplexed_query" J..= Q.getQueryText (_mhQuery lqHandler)
        , "candidates" J..= candidatesJ
        ]
  where
    dumpCandidates candidateMap = do
      candidates <- ListT.toList $ STMMap.listT candidateMap
      forM candidates $ \((usrVars, varVals), candidate) -> do
        candidateJ <- dumpCandidate candidate
        return $ J.object
          [ "session_vars" J..= usrVars
          , "variable_values" J..= varVals
          , "candidate" J..= candidateJ
          ]
    dumpCandidate (MLQHandlerCandidate respId _ respTV curOps newOps) = do
      prevResHash <- STM.readTVar respTV
      curOpIds <- ListT.toList $ STMMap.listT curOps
      newOpIds <- ListT.toList $ STMMap.listT newOps
      return $ J.object
        [ "resp_id" J..= unRespId respId
        , "current_ops" J..= map fst curOpIds
        , "new_ops" J..= map fst newOpIds
        , "previous_result_hash" J..= prevResHash
        ]

type ValidatedVariables = Map.HashMap G.Variable TxtEncodedPGVal

data MLQHandler k
  = MLQHandler
  { _mhAlias      :: !G.Alias
  , _mhQuery      :: !Q.Query
  , _mhCandidates ::
      !(STMMap.Map
        (UserVars, Maybe VariableValues)
        (MLQHandlerCandidate k)
       )
  }

-- This type represents the state associated with
-- the response of (role, gqlQueryText, userVars, variableValues)
data MLQHandlerCandidate k
  = MLQHandlerCandidate
  -- the laterally joined query responds with [(RespId, EncJSON)]
  -- so the resultid is used to determine the websockets
  -- where the data needs to be sent
  { _mlqhcRespId        :: !RespId

  -- query variables which are validated and text encoded
  , _mlqhcValidatedVars :: !ValidatedVariables

  -- we need to store the previous response
  , _mlqhcPrevRes       :: !RespTV

  -- the actions that have been run previously
  -- we run these if the response changes
  , _mlqhcCurOps        :: !(Sinks k)

  -- we run these operations regardless
  -- and then merge them with current operations
  , _mlqhcNewOps        :: !(Sinks k)
  }

-- the multiplexed query associated with the livequery
-- and the validated, text encoded query variables
type MxOp = (G.Alias, Q.Query, ValidatedVariables)

addLiveQuery
  :: (Eq k, Hashable k)
  => PGExecCtx
  -> LiveQueriesState k
  -- the query
  -> LiveQuery
  -> MxOp
  -- a unique operation id
  -> k
  -- the action to be executed when result changes
  -> OnChange
  -> IO ()
addLiveQuery pgExecCtx lqState liveQ (als, mxQuery, valQVars) k onResultAction = do

  -- generate a new result id
  responseId <- newRespId

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $ do
    handlerM <- STMMap.lookup handlerId lqMap
    case handlerM of
      Just (handler, _) -> do
        candidateM <- STMMap.lookup candidateId $ _mhCandidates handler
        case candidateM of
          Just candidate -> addToExistingCandidate candidate
          Nothing        -> addToExistingHandler responseId handler
        return Nothing
      Nothing -> do
        handler <- newHandler responseId
        asyncRefTM <- STM.newEmptyTMVar
        STMMap.insert (handler, asyncRefTM) handlerId lqMap
        return $ Just (handler, asyncRefTM)

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \(handler, pollerThreadTM) -> do
    threadRef <- A.async $ forever $ do
      pollQuery batchSize pgExecCtx handler
      threadDelay $ refetchIntervalToMicro refetchInterval
    STM.atomically $ STM.putTMVar pollerThreadTM threadRef

  where

    LiveQueriesState lqOpts lqMap = lqState
    MxOpts batchSize refetchInterval = lqOpts

    addToExistingCandidate handlerC =
      STMMap.insert onResultAction k $ _mlqhcNewOps handlerC

    newHandlerC responseId = do
      handlerC <- MLQHandlerCandidate
                  responseId
                  valQVars
                  <$> STM.newTVar Nothing
                  <*> STMMap.new
                  <*> STMMap.new
      STMMap.insert onResultAction k $ _mlqhcNewOps handlerC
      return handlerC

    addToExistingHandler responseId handler = do
      handlerC <- newHandlerC responseId
      STMMap.insert handlerC candidateId $ _mhCandidates handler

    newHandler responseId = do
      handler <- MLQHandler als mxQuery <$> STMMap.new
      handlerC <- newHandlerC responseId
      STMMap.insert handlerC candidateId $ _mhCandidates handler
      return handler

    handlerId   = getHandlerId liveQ
    candidateId = getCandidateId liveQ

type CandidateId = (UserVars, Maybe VariableValues)

getCandidateId :: LiveQuery -> CandidateId
getCandidateId lq =
  (usrVars, queryVars)
  where
    usrVars   = userVars $ _lqUser lq
    queryVars = _grVariables $ _lqRequest lq

getHandlerId :: LiveQuery -> MLiveQueryId
getHandlerId lq =
  MLiveQueryId role queryStr
  where
    role     = userRole $ _lqUser lq
    queryStr = _grQuery $ _lqRequest lq

removeLiveQuery
  :: (Eq k, Hashable k)
  => LiveQueriesState k
  -- the query and the associated operation
  -> LiveQuery
  -> k
  -> IO ()
removeLiveQuery lqState liveQ k = do
  threadRefM <- STM.atomically $ do
   detM <- getQueryDet
   fmap join $ forM detM $
    \(handler, threadRef, candidate) ->
      cleanHandlerC (_mhCandidates handler) threadRef candidate
  onJust threadRefM A.cancel

  where
    lqMap = _lqsLiveQueryMap lqState

    getQueryDet = do
      handlerM <- STMMap.lookup handlerId lqMap
      fmap join $ forM handlerM $ \(handler, threadRef) -> do
        let MLQHandler _ _ candidateMap = handler
        candidateM <- STMMap.lookup candidateId candidateMap
        return $ fmap (handler, threadRef,) candidateM

    cleanHandlerC candidateMap threadRef handlerC = do
      let curOps = _mlqhcCurOps handlerC
          newOps = _mlqhcNewOps handlerC
      STMMap.delete k curOps
      STMMap.delete k newOps
      candidateIsEmpty <- (&&)
        <$> STMMap.null curOps
        <*> STMMap.null newOps
      when candidateIsEmpty $ STMMap.delete candidateId candidateMap
      handlerIsEmpty <- STMMap.null candidateMap
      -- when there is no need for handler
      -- i.e, this happens to be the last operation, take the
      -- ref for the polling thread to cancel it
      if handlerIsEmpty
        then do
          STMMap.delete handlerId lqMap
          Just <$> STM.takeTMVar threadRef
        else return Nothing

    handlerId   = getHandlerId liveQ
    candidateId = getCandidateId liveQ

data CandidateSnapshot
  = CandidateSnapshot
  { _csRespVars   :: !RespVars
  , _csPrevResRef :: !RespTV
  , _csCurSinks   :: ![OnChange]
  , _csNewSinks   :: ![OnChange]
  }

pushCandidateResult :: GQResp -> Maybe RespHash -> CandidateSnapshot -> IO ()
pushCandidateResult resp respHashM candidateSnapshot = do
  pushResultToSinks newSinks
  -- write to the current websockets if needed
  prevRespHashM <- STM.readTVarIO respRef
  when (isExecError resp || respHashM /= prevRespHashM) $ do
    pushResultToSinks curSinks
    STM.atomically $ STM.writeTVar respRef respHashM
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
           , "variables" J..= fmap asJson valVars
           ]
  where
    asJson = \case
      TENull  -> J.Null
      TELit t -> J.String t

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
  :: (Eq k, Hashable k)
  => BatchSize
  -> PGExecCtx
  -> MLQHandler k
  -> IO ()
pollQuery batchSize pgExecCtx (MLQHandler alias pgQuery candidateMap) = do

  -- get a snapshot of all the candidates
  candidateSnapshotMap <- STM.atomically $ do
    candidates <- ListT.toList $ STMMap.listT candidateMap
    candidateSnapshots <- mapM getCandidateSnapshot candidates
    return $ Map.fromList candidateSnapshots

  let queryVarsBatches = chunks (unBatchSize batchSize) $
                        getQueryVars candidateSnapshotMap

  flip A.mapConcurrently_ queryVarsBatches $ \queryVars -> do
    mxRes <- runExceptT $ runLazyTx' pgExecCtx $
             liftTx $ Q.listQE defaultTxErrorHandler
             pgQuery (mkMxQueryPrepArgs queryVars) True

    let operations = getCandidateOperations candidateSnapshotMap mxRes
    -- concurrently push each unique result
    A.mapConcurrently_ (uncurry3 pushCandidateResult) operations

  where
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (a, b, c) = f a b c

    getCandidateSnapshot ((usrVars, _), handlerC) = do
      let MLQHandlerCandidate resId valVars respRef curOpsTV newOpsTV = handlerC
      curOpsL <- ListT.toList $ STMMap.listT curOpsTV
      newOpsL <- ListT.toList $ STMMap.listT newOpsTV
      forM_ newOpsL $ \(k, action) -> STMMap.insert action k curOpsTV
      STMMap.reset newOpsTV
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
