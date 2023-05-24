module Control.Monad.TimeLimit
  ( runWithTimeLimit,
    succeedsWithinTimeLimit,
  )
where

import Control.Concurrent (newEmptyMVar, putMVar, tryTakeMVar)
import Control.Concurrent.Async (async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Extended (sleep)
import Hasura.Prelude

-- | Runs an action with a time limit of approximately 0.1s.
-- If the time taken to perform the action exceeds this limit,
-- it returns 'Nothing'.
runWithTimeLimit :: (MonadIO m) => IO a -> m (Maybe a)
runWithTimeLimit action = liftIO do
  var <- newEmptyMVar
  thread <- async do
    value <- action
    putMVar var $! value
  result <-
    foldr1 continueOnFail $ replicate 10 do
      sleep 0.01
      tryTakeMVar var
  Async.cancel thread
  pure result
  where
    continueOnFail step nextStep =
      step >>= \case
        Nothing -> nextStep
        Just res -> pure (Just res)

-- | Runs an action with a time limit of approximately 0.1s.
-- If the time taken to perform the action exceeds this limit,
-- it fails.
succeedsWithinTimeLimit :: (MonadIO m, MonadFail m) => IO a -> m a
succeedsWithinTimeLimit action =
  runWithTimeLimit action
    `onNothingM` fail "failed to compute in reasonable time"
