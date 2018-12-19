module Hasura.GraphQL.Resolve.LiveQuery
  ( LiveQuery(..)
  , LiveQueryMap
  , newLiveQueryMap
  , addLiveQuery
  , removeLiveQuery
  ) where

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified Data.ByteString.Lazy                   as BL
import qualified ListT
import qualified STMContainers.Map                      as STMMap

import           Control.Concurrent                     (threadDelay)

import           Hasura.GraphQL.Resolve.Context         (LazyRespTx)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.Types

data LiveQuery
  = LiveQuery
  { _lqUser    :: !UserInfo
  , _lqRequest :: !GraphQLRequest
  } deriving (Show, Eq, Generic)

instance Hashable LiveQuery

type OnChange k = GQResp -> IO ()

data LQHandler k
  = LQHandler
  -- the tx to be executed
  { _lqhRespTx  :: !LazyRespTx
  -- previous result
  , _lqhPrevRes :: !(STM.TVar (Maybe GQResp))
  -- the actions that have been run previously
  -- we run these if the response changes
  , _lqhCurOps  :: !(STMMap.Map k (OnChange k))
  -- we run these operations regardless
  -- and then merge them with current operations
  , _lqhNewOps  :: !(STMMap.Map k (OnChange k))
  }

type ThreadTM       = STM.TMVar (A.Async ())
type LiveQueryMap k = STMMap.Map LiveQuery (LQHandler k, ThreadTM)

newLiveQueryMap :: STM.STM (LiveQueryMap k)
newLiveQueryMap = STMMap.new

type TxRunner = LazyRespTx -> IO (Either QErr BL.ByteString)

removeLiveQuery
  :: (Eq k, Hashable k)
  => LiveQueryMap k
  -- the query and the associated operation
  -> LiveQuery
  -> k
  -> IO ()
removeLiveQuery lqMap liveQ k = do

  -- clean the handler's state
  threadRefM <- STM.atomically $ do
    lqHandlerM <- STMMap.lookup liveQ lqMap
    maybe (return Nothing) cleanLQHandler lqHandlerM

  -- cancel the polling thread
  onJust threadRefM A.cancel

  where
    cleanLQHandler (handler, threadRef) = do
      let curOps = _lqhCurOps handler
          newOps = _lqhNewOps handler
      STMMap.delete k curOps
      STMMap.delete k newOps
      cancelPollThread <- (&&)
        <$> STMMap.null curOps
        <*> STMMap.null newOps
      -- if this happens to be the last operation, take the
      -- ref for the polling thread to cancel it
      if cancelPollThread then do
          STMMap.delete liveQ lqMap
          Just <$> STM.takeTMVar threadRef
        else return Nothing

addLiveQuery
  :: (Eq k, Hashable k)
  => TxRunner
  -> LiveQueryMap k
  -- the query
  -> LiveQuery
  -- the transaction associated with this query
  -> LazyRespTx
  -- a unique operation id
  -> k
  -- the action to be executed when result changes
  -> OnChange k
  -> IO ()
addLiveQuery txRunner lqMap liveQ respTx k onResultAction= do

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $ do
    lqHandlerM <- STMMap.lookup liveQ lqMap
    maybe newHandler addToExistingHandler lqHandlerM

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \(handler, pollerThreadTM) -> do
    threadRef <- A.async $ forever $ do
      pollQuery txRunner handler
      threadDelay $ 1 * 1000 * 1000
    STM.atomically $ STM.putTMVar pollerThreadTM threadRef

  where

    addToExistingHandler (handler, _) = do
      STMMap.insert onResultAction k $ _lqhNewOps handler
      return Nothing

    newHandler = do
      handler <- LQHandler
                 <$> return respTx
                 <*> STM.newTVar Nothing
                 <*> STMMap.new
                 <*> STMMap.new
      STMMap.insert onResultAction k $ _lqhNewOps handler
      asyncRefTM <- STM.newEmptyTMVar
      STMMap.insert (handler, asyncRefTM) liveQ lqMap
      return $ Just (handler, asyncRefTM)

pollQuery
  :: (Eq k, Hashable k)
  => TxRunner
  -> LQHandler k
  -> IO ()
pollQuery runTx (LQHandler respTx respTV curOpsTV newOpsTV) = do

  res <- runTx respTx

  let resp = case res of
        Left e   -> GQExecError [encodeGQErr False e]
        Right bs -> GQSuccess bs

  -- extract the current and new operations
  (curOps, newOps) <- STM.atomically $ do
    curOpsL <- ListT.toList $ STMMap.stream curOpsTV
    newOpsL <- ListT.toList $ STMMap.stream newOpsTV
    forM_ newOpsL $ \(k, action) -> STMMap.insert action k curOpsTV
    STMMap.deleteAll newOpsTV
    return (curOpsL, newOpsL)

  runOperations resp newOps

  -- write to the current websockets if needed
  prevRespM <- STM.readTVarIO respTV
  when (isExecError resp || Just resp /=  prevRespM) $ do
    runOperations resp curOps
    STM.atomically $ STM.writeTVar respTV $ Just resp

  where
    runOperation resp action = action resp
    runOperations resp =
      void . A.mapConcurrently (runOperation resp . snd)
