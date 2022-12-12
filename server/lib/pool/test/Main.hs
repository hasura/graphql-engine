{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sleep" #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Exception (fromException))
import Control.Exception.Base (IOException, SomeException)
import Data.Maybe (isJust)
import Data.Pool
import Debug.Trace (traceShow)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "acquiring resources" $ do
    it "acquire a single available resource" $
      canAcquireSingleItem `shouldReturn` ()
    it "throw when timeout expires and no resource is available" $
      acquiringThrowsWhenTimeoutExpired `shouldThrow` timeoutException
    it "acquires resource after it was released" $
      acquireReleaseAndReAcquire `shouldReturn` ()
    it "acquire resource with a delay shorter than the timeout" $
      acquireWaitAndRelease `shouldReturn` ()
    it "throw when waiting for longer than timeout" $
      acquireAndWaitTooLong `shouldThrow` timeoutException
    it "resource acquisition interacts with resizing" $
      acquireResize `shouldReturn` ()

timeoutException :: Selector TimeoutException
timeoutException = const True

mkPool :: IO (Pool ())
mkPool =
  createPool'
    (pure ())
    (const (pure ()))
    stripes
    idleTime
    maxResources
    timeout
  where
    stripes = 1
    idleTime = 1000
    maxResources = 1
    timeout = Just 1

canAcquireSingleItem :: IO ()
canAcquireSingleItem =
  mkPool >>= fmap fst . takeResource

acquiringThrowsWhenTimeoutExpired :: IO ()
acquiringThrowsWhenTimeoutExpired = do
  pool <- mkPool
  _ <- takeResource pool
  fst <$> takeResource pool

acquireReleaseAndReAcquire :: IO ()
acquireReleaseAndReAcquire = do
  pool <- mkPool
  (res, lp) <- takeResource pool
  putResource lp res
  fst <$> takeResource pool

acquireWaitAndRelease :: IO ()
acquireWaitAndRelease = do
  pool <- mkPool
  (res, lp) <- takeResource pool
  _ <- forkIO do
    threadDelay 300_000
    putResource lp res
  fst <$> takeResource pool

acquireAndWaitTooLong :: IO ()
acquireAndWaitTooLong = do
  pool <- mkPool
  (res, lp) <- takeResource pool
  _ <- forkIO do
    threadDelay 2_000_000
    putResource lp res
  fst <$> takeResource pool

acquireResize :: IO ()
acquireResize = do
  pool <- mkPool
  (res, lp) <- takeResource pool
  Nothing <- tryTakeResource pool
  resizePool pool 2
  (res', lp') <- takeResource pool
  2 <- getInUseResourceCount pool
  resizePool pool 1
  -- this resource should get destroyed
  releaseResource pool lp res
  1 <- getInUseResourceCount pool
  -- this resource should get returned to the pool
  releaseResource pool lp' res'
  1 <- getInUseResourceCount pool
  fst <$> takeResource pool
