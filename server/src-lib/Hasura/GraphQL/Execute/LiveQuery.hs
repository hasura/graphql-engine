{-# LANGUAGE RankNTypes #-}

module Hasura.GraphQL.Execute.LiveQuery
  ( LiveQueriesState
  , initLiveQueriesState
  , LiveQuery(..)
  , LiveQueryOp(..)
  , addLiveQuery
  , removeLiveQuery

  , TxRunner(..)
  , LQM.mkMxQuery
  ) where

import qualified Control.Concurrent.STM                 as STM

import qualified Hasura.GraphQL.Execute.LiveQuery.Fallback    as LQF
import qualified Hasura.GraphQL.Execute.LiveQuery.Multiplexed as LQM

import           Hasura.GraphQL.Execute.LiveQuery.Types

import           Hasura.Prelude
import           Hasura.RQL.Types

-- this has to be in another module
newtype TxRunner
  = TxRunner { unTxRunner :: forall a. LazyTx QErr a -> IO (Either QErr a) }

data LiveQueriesState k
  = LiveQueriesState
  { _lqsMultiplexed :: LQM.LiveQueryMap k
  , _lqsFallback    :: LQF.LiveQueryMap k
  , _lqsTxRunner    :: TxRunner
  }

initLiveQueriesState
  :: (forall a. LazyTx QErr a -> IO (Either QErr a))
  -> IO (LiveQueriesState k)
initLiveQueriesState txRunner = do
  (mxMap, fallbackMap) <- STM.atomically $
    (,) <$> LQM.newLiveQueryMap <*> LQF.newLiveQueryMap
  return $ LiveQueriesState mxMap fallbackMap $ TxRunner txRunner

data LiveQueryOp
  = LQMultiplexed !LQM.MxOp
  | LQFallback !LQF.FallbackOp

addLiveQuery
  :: (Eq k, Hashable k)
  => LiveQueriesState k
  -- the query
  -> LiveQuery
  -> LiveQueryOp
  -- a unique operation id
  -> k
  -- the action to be executed when result changes
  -> OnChange
  -> IO ()
addLiveQuery lqState liveQ liveQOp k onResultAction =
  case liveQOp of
    LQMultiplexed mxOp ->
      LQM.addLiveQuery
      (unTxRunner txRunner) mxMap liveQ mxOp k onResultAction
    LQFallback fallbackOp ->
      LQF.addLiveQuery
      (unTxRunner txRunner) fallbackMap liveQ fallbackOp k onResultAction
  where
    LiveQueriesState mxMap fallbackMap txRunner = lqState

removeLiveQuery
  :: (Eq k, Hashable k)
  => LiveQueriesState k
  -- the query and the associated operation
  -> LiveQuery
  -> k
  -> IO ()
removeLiveQuery lqState liveQ k = do
  LQM.removeLiveQuery mxMap liveQ k
  LQF.removeLiveQuery fallbackMap liveQ k
  where
    LiveQueriesState mxMap fallbackMap _ = lqState
