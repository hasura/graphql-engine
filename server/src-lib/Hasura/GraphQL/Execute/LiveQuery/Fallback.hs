module Hasura.GraphQL.Execute.LiveQuery.Fallback
  ( LiveQuery(..)
  , LiveQueryMap
  , newLiveQueryMap
  , FallbackOp
  , addLiveQuery
  , removeLiveQuery
  ) where

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified ListT
import qualified StmContainers.Map                      as STMMap

import           Control.Concurrent                     (threadDelay)

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Types
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

data LQHandler k
  = LQHandler
  -- the tx to be executed
  { _lqhRespTx  :: !LazyRespTx
  -- previous result
  , _lqhPrevRes :: !RespTV
  -- the actions that have been run previously
  -- we run these if the response changes
  , _lqhCurOps  :: !(Sinks k)
  -- we run these operations regardless
  -- and then merge them with current operations
  , _lqhNewOps  :: !(Sinks k)
  }

type LiveQueryMap k = STMMap.Map LiveQuery (LQHandler k, ThreadTM)

newLiveQueryMap :: STM.STM (LiveQueryMap k)
newLiveQueryMap = STMMap.new

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

-- the transaction associated with this query
type FallbackOp = LazyRespTx

addLiveQuery
  :: (Eq k, Hashable k)
  => PGExecCtx
  -> LiveQueryMap k
  -- the query
  -> LiveQuery
  -> FallbackOp
  -- a unique operation id
  -> k
  -- the action to be executed when result changes
  -> OnChange
  -> IO ()
addLiveQuery pgExecCtx lqMap liveQ respTx k onResultAction= do

  -- a handler is returned only when it is newly created
  handlerM <- STM.atomically $ do
    lqHandlerM <- STMMap.lookup liveQ lqMap
    maybe newHandler addToExistingHandler lqHandlerM

  -- we can then attach a polling thread if it is new
  -- the livequery can only be cancelled after putTMVar
  onJust handlerM $ \(handler, pollerThreadTM) -> do
    threadRef <- A.async $ forever $ do
      pollQuery pgExecCtx handler
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
  => PGExecCtx
  -> LQHandler k
  -> IO ()
pollQuery pgExecCtx (LQHandler respTx respTV curOpsTV newOpsTV) = do

  resOrErr <- runExceptT $ runLazyTx pgExecCtx respTx

  let (resp, respHashM) = case encJToLBS <$> resOrErr of
        Left e    -> (GQExecError [encodeGQErr False e], Nothing)
        Right lbs -> (GQSuccess lbs, Just $ mkRespHash lbs)

  -- extract the current and new operations
  (curOps, newOps) <- STM.atomically $ do
    curOpsL <- ListT.toList $ STMMap.listT curOpsTV
    newOpsL <- ListT.toList $ STMMap.listT newOpsTV
    forM_ newOpsL $ \(k, action) -> STMMap.insert action k curOpsTV
    STMMap.reset newOpsTV
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
