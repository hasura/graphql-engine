module Hasura.GraphQL.Execute.LiveQuery.Multiplexed
  ( addLiveQuery
  , removeLiveQuery
  ) where

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
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import qualified Hasura.SQL.DML                         as S
import qualified Hasura.SQL.Types                       as S
import           Hasura.SQL.Value                       (textToPrepVal)

data LiveQuery
  = LiveQuery
  { _lqUser    :: !UserInfo
  , _lqRequest :: !GQLReqUnparsed
  } deriving (Show, Eq, Generic)

newtype RespId
  = RespId { unRespId :: UUID.UUID }
  deriving (Show, Eq, Hashable, Q.FromCol)

newtype RespIdList
  = RespIdList { unRespIdList :: [RespId] }
  deriving (Show, Eq)

instance Q.ToPrepArg RespIdList where
  toPrepVal (RespIdList l) =
    textToPrepVal $ S.toSQLTxt $
    S.SEArray $ map (S.SELit . UUID.toText . unRespId) l

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

type ThreadTM       = STM.TMVar (A.Async ())
type MLiveQueryMap k = STMMap.Map MLiveQueryId (MLQHandler k, ThreadTM)

type OnChange = GQResp -> IO ()

type ValidatedVariables = Map.HashMap G.Variable Text

data MLQHandler k
  = MLQHandler
  { _mhQuery      :: !Q.Query
  , _mhCandidates ::
      !(STMMap.Map
        (UserVars, Maybe VariableValues)
        (MLQHandlerCandidate k)
       )
  }

type RespRef = STM.TVar (Maybe GQResp)

type MxQResp = [(RespId, EncJSON)]

type TxRunner = LazyTx QErr MxQResp -> IO (Either QErr MxQResp)
-- 'k' uniquely identifies a sink
-- in case of websockets, it is (wsId, opId)
type Sinks k = STMMap.Map k OnChange

-- This type represents the state associated with
-- the response of (role, gqlQueryText, userVars, variableValues)
data MLQHandlerCandidate k
  = MLQHandlerCandidate
  -- the laterally joined query responds with [(RespId, EncJSON)]
  -- so the resultid is used to determine the websockets
  -- where the data needs to be sent
  { _mlqhcRespId    :: !RespId

  -- query variables which are validated and text encoded
  , _mlqhcValidatedVars :: !ValidatedVariables

  -- we need to store the previous response
  , _mlqhcPrevRes       :: !RespRef

  -- the actions that have been run previously
  -- we run these if the response changes
  , _mlqhcCurOps        :: !(Sinks k)

  -- we run these operations regardless
  -- and then merge them with current operations
  , _mlqhcNewOps        :: !(Sinks k)
  }

addLiveQuery
  :: (Eq k, Hashable k)
  => TxRunner
  -> MLiveQueryMap k
  -- the query
  -> LiveQuery
  -- the multiplexed query associated with this query
  -> Q.Query
  -- the validated query variables
  -> ValidatedVariables
  -- a unique operation id
  -> k
  -- the action to be executed when result changes
  -> OnChange
  -> IO ()
addLiveQuery txRunner lqMap liveQ mxQuery valQVars k onResultAction= do

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
      pollQuery txRunner handler
      threadDelay $ 1 * 1000 * 1000
    STM.atomically $ STM.putTMVar pollerThreadTM threadRef

  where

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
      handler <- MLQHandler mxQuery <$> STMMap.new
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
  => MLiveQueryMap k
  -- the query and the associated operation
  -> LiveQuery
  -> k
  -> IO ()
removeLiveQuery lqMap liveQ k = do
  threadRefM <- STM.atomically $ do
   detM <- getQueryDet
   fmap join $ forM detM $
    \(handler, threadRef, candidate) ->
      cleanHandlerC (_mhCandidates handler) threadRef candidate
  onJust threadRefM A.cancel

  where
    getQueryDet = do
      handlerM <- STMMap.lookup handlerId lqMap
      fmap join $ forM handlerM $ \(handler, threadRef) -> do
        let MLQHandler _ candidateMap = handler
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
  { _csRespVars :: !RespVars
  , _csPrevResRef :: !RespRef
  , _csCurSinks   :: ![OnChange]
  , _csNewSinks   :: ![OnChange]
  }

pushCandidateResult :: GQResp -> CandidateSnapshot -> IO ()
pushCandidateResult resp candidateSnapshot = do
  pushResultToSinks newSinks
  -- write to the current websockets if needed
  prevRespM <- STM.readTVarIO respRef
  when (isExecError resp || Just resp /= prevRespM) $ do
    pushResultToSinks curSinks
    STM.atomically $ STM.writeTVar respRef $ Just resp
  where
    CandidateSnapshot _ respRef curSinks newSinks = candidateSnapshot
    pushResultToSinks =
      A.mapConcurrently_ (\action -> action resp)

type RespVars = J.Value

newtype RespVarsList
  = RespVarsList { _unRespVarsList :: [RespVars]}

instance Q.ToPrepArg RespVarsList where
  toPrepVal (RespVarsList l) =
    textToPrepVal $ S.toSQLTxt $
    S.SEArray $ map (S.SELit . J.encodeToStrictText) l

getRespVars :: UserVars -> ValidatedVariables -> RespVars
getRespVars usrVars valVars =
  J.object [ "user" J..= usrVars
           , "variables" J..= valVars
           ]

pollQuery
  :: (Eq k, Hashable k)
  => TxRunner
  -> MLQHandler k
  -> IO ()
pollQuery runTx (MLQHandler pgQuery candidateMap) = do

  -- get a snapshot of all the candidates
  candidateSnapshotMap <- STM.atomically $ do
    candidates <- ListT.toList $ STMMap.listT candidateMap
    candidateSnapshots <- mapM getCandidateSnapshot candidates
    return $ Map.fromList candidateSnapshots

  let (respIds, respVars) = getQueryVars candidateSnapshotMap

  mxRes <- runTx $ liftTx $ Q.listQE defaultTxErrorHandler
           pgQuery (respIds, respVars) True

  let operations = getCandidateOperations candidateSnapshotMap mxRes
  -- concurrently push each unique result
  A.mapConcurrently_ (uncurry pushCandidateResult) operations

  where
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
      let (respIdL, respVarL) =
            unzip $ Map.toList $ fmap _csRespVars candidateSnapshotMap
      in (RespIdList respIdL, RespVarsList respVarL)

    getCandidateOperations candidateSnapshotMap = \case
      Left e ->
        -- TODO: this is internal error
        let resp = GQExecError [encodeGQErr False e]
        in [ (resp, snapshot)
           | (_, snapshot) <- Map.toList candidateSnapshotMap
           ]
      Right responses ->
        flip mapMaybe responses $ \(respId, respEnc) ->
          -- TODO: change it to use bytestrings directly
          let resp = GQSuccess $ encJToLBS respEnc
          in (resp,) <$> Map.lookup respId candidateSnapshotMap
