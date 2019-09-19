module Hasura.GraphQL.Execute.LiveQuery.Fallback
  ( RefetchInterval
  , refetchIntervalFromMilli
  , FallbackOpts
  , mkFallbackOpts

  , LiveQueriesState
  , initLiveQueriesState
  , dumpLiveQueriesState

  , FallbackOp
  , mkFallbackOp
  , LiveQueryId
  , addLiveQuery
  , removeLiveQuery
  ) where

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified Data.Aeson                             as J
import qualified ListT
import qualified StmContainers.Map                      as STMMap

import           Control.Concurrent                     (threadDelay)

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Types
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

data LiveQuery
  = LiveQuery
  { _lqUser    :: !UserInfo
  , _lqRequest :: !GQLReqUnparsed
  } deriving (Show, Eq, Generic)

instance J.ToJSON LiveQuery where
  toJSON (LiveQuery user req) =
    J.object [ "user" J..= userVars user
             , "request" J..= req
             ]

instance Hashable LiveQuery

data LQHandler
  = LQHandler
  -- the tx to be executed
  { _lqhRespTx  :: !LazyRespTx
  -- previous result
  , _lqhPrevRes :: !RespTV
  -- the actions that have been run previously
  -- we run these if the response changes
  , _lqhCurOps  :: !Sinks
  -- we run these operations regardless
  -- and then merge them with current operations
  , _lqhNewOps  :: !Sinks
  }

data FallbackOpts
  = FallbackOpts
  { _foRefetchInterval :: !RefetchInterval
  } deriving (Show, Eq)

instance J.ToJSON FallbackOpts where
  toJSON (FallbackOpts refetchInterval) =
    J.object [ "refetch_delay" J..= refetchInterval
             ]

-- 1 second
defaultRefetchInterval :: RefetchInterval
defaultRefetchInterval =
  refetchIntervalFromMilli 1000

mkFallbackOpts
  :: Maybe RefetchInterval
  -> FallbackOpts
mkFallbackOpts refetchIntervalM =
  FallbackOpts
  (fromMaybe defaultRefetchInterval refetchIntervalM)

data LiveQueriesState
  = LiveQueriesState
  { _lqsOptions      :: !FallbackOpts
  , _lqsLiveQueryMap :: !LiveQueryMap
  }

dumpLiveQueriesState :: LiveQueriesState -> IO J.Value
dumpLiveQueriesState (LiveQueriesState opts lqMap) = do
  lqMapJ <- dumpLiveQueryMap lqMap
  return $ J.object
    [ "options" J..= opts
    , "live_queries_map" J..= lqMapJ
    ]

initLiveQueriesState
  :: FallbackOpts
  -> STM.STM LiveQueriesState
initLiveQueriesState lqOptions =
  LiveQueriesState
  lqOptions
  <$> STMMap.new

data LiveQueryId
  = LiveQueryId
  { _lqiQuery     :: !LiveQuery
  , _lqiSink      :: !SinkId
  }

type LiveQueryMap = STMMap.Map LiveQuery (LQHandler, ThreadTM)

dumpLiveQueryMap :: LiveQueryMap -> IO J.Value
dumpLiveQueryMap lqMap =
  fmap J.toJSON $ STM.atomically $ do
    entries <- ListT.toList $ STMMap.listT lqMap
    forM entries $ \(lq, (lqHandler, threadRef)) -> do
      prevResHash <- STM.readTVar $ _lqhPrevRes lqHandler
      threadId <-  A.asyncThreadId <$> STM.readTMVar threadRef
      curOps <- toListTMap $ _lqhCurOps lqHandler
      newOps <- toListTMap $ _lqhNewOps lqHandler
      return $ J.object
        [ "query" J..= lq
        , "thread_id" J..= show threadId
        , "current_ops" J..= map fst curOps
        , "new_ops" J..= map fst newOps
        , "previous_result_hash" J..= prevResHash
        ]

removeLiveQuery
  :: LiveQueriesState
  -- the query and the associated operation
  -> LiveQueryId
  -> IO ()
removeLiveQuery lqState (LiveQueryId liveQ k) = do

  -- clean the handler's state
  threadRefM <- STM.atomically $ do
    lqHandlerM <- STMMap.lookup liveQ lqMap
    maybe (return Nothing) cleanLQHandler lqHandlerM

  -- cancel the polling thread
  onJust threadRefM A.cancel

  where
    lqMap = _lqsLiveQueryMap lqState
    cleanLQHandler (handler, threadRef) = do
      let curOps = _lqhCurOps handler
          newOps = _lqhNewOps handler
      deleteTMap k curOps
      deleteTMap k newOps
      cancelPollThread <- (&&)
        <$> nullTMap curOps
        <*> nullTMap newOps
      -- if this happens to be the last operation, take the
      -- ref for the polling thread to cancel it
      if cancelPollThread then do
          STMMap.delete liveQ lqMap
          Just <$> STM.takeTMVar threadRef
        else return Nothing

-- the transaction associated with this query
type FallbackOp = (LiveQuery, LazyRespTx)

mkFallbackOp
  :: UserInfo -> GQLReqUnparsed
  -> LazyRespTx -> FallbackOp
mkFallbackOp userInfo req tx =
  (LiveQuery userInfo req, tx)


addLiveQuery
  :: PGExecCtx
  -> LiveQueriesState
  -- the query
  -> FallbackOp
  -- the action to be executed when result changes
  -> OnChange
  -> IO LiveQueryId
addLiveQuery pgExecCtx lqState (liveQ, respTx) onResultAction= do

  sinkId <- newSinkId

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $ do
    lqHandlerM <- STMMap.lookup liveQ lqMap
    maybe (newHandler sinkId) (addToExistingHandler sinkId) lqHandlerM

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \(handler, pollerThreadTM) -> do
    threadRef <- A.async $ forever $ do
      pollQuery pgExecCtx handler
      threadDelay $ refetchIntervalToMicro refetchInterval
    STM.atomically $ STM.putTMVar pollerThreadTM threadRef

  return $ LiveQueryId liveQ sinkId

  where

    LiveQueriesState lqOpts lqMap = lqState
    FallbackOpts refetchInterval = lqOpts

    addToExistingHandler sinkId (handler, _) = do
      insertTMap onResultAction sinkId $ _lqhNewOps handler
      return Nothing

    newHandler sinkId = do
      handler <- LQHandler
                 <$> return respTx
                 <*> STM.newTVar Nothing
                 <*> newTMap
                 <*> newTMap
      insertTMap onResultAction sinkId $ _lqhNewOps handler
      asyncRefTM <- STM.newEmptyTMVar
      STMMap.insert (handler, asyncRefTM) liveQ lqMap
      return $ Just (handler, asyncRefTM)

pollQuery
  :: PGExecCtx
  -> LQHandler
  -> IO ()
pollQuery pgExecCtx (LQHandler respTx respTV curOpsTV newOpsTV) = do

  resOrErr <- runExceptT $ runLazyTx pgExecCtx respTx

  let (resp, respHashM) = case resOrErr of
        Left e    -> (GQExecError [encodeGQErr False e], Nothing)
        Right encJson -> (GQSuccess encJson, Just $ mkRespHash $ encJToLBS encJson)

  -- extract the current and new operations
  (curOps, newOps) <- STM.atomically $ do
    curOpsL <- toListTMap curOpsTV
    newOpsL <- toListTMap newOpsTV
    forM_ newOpsL $ \(k, action) -> insertTMap action k curOpsTV
    resetTMap newOpsTV
    return (curOpsL, newOpsL)

  runOperations resp newOps

  -- write to the current websockets if needed
  prevRespHashM <- STM.readTVarIO respTV
  when (isExecError resp || respHashM /= prevRespHashM) $ do
    runOperations resp curOps
    STM.atomically $ STM.writeTVar respTV respHashM

  where
    runOperation resp action = action resp
    runOperations resp =
      void . A.mapConcurrently (runOperation resp . snd)
