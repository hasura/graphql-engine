{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (MVar, forkIO, modifyMVar_, readMVar, threadDelay, yield)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newMVar)
import Control.Exception (Exception (fromException), throwIO)
import Control.Exception.Base (IOException, SomeException)
import Control.Monad (replicateM, replicateM_, void)
import Data.Maybe (isJust)
import Data.Pool
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Debug.Trace (traceShow)
import Test.Hspec

{-# ANN module ("HLint: ignore avoid Control.Concurrent.forkIO" :: String) #-}

{-# ANN module ("HLint: ignore avoid Control.Concurrent.threadDelay" :: String) #-}

main :: IO ()
main = hspec do
  describe "a resource pool" do
    describe "when acquiring resources" do
      it "acquires a single available resource" do
        pool <- mkPool defaultConfiguration
        value <- fst <$> takeResource pool
        value `shouldBe` ()

      it "acquires a resource after it was released" do
        pool <- mkPool defaultConfiguration
        (res, lp) <- takeResource pool
        putResource lp res
        value <- fst <$> takeResource pool
        value `shouldBe` ()

    describe "upon exception" do
      it "automatically destroys a resource after an exception is thrown" do
        (pool, logVar) <- mkPoolWithLog defaultConfiguration {confMaxResources = 3}

        flip shouldThrow anIOError $ withResource pool $ const do
          append Acquired logVar
          throwIO (userError "Oh no!")
          append Released logVar
        flip shouldThrow anIOError $ withResource pool $ const do
          append Acquired logVar
          throwIO (userError "Oh no!")
          append Released logVar
        void $ withResource pool $ const do
          append Acquired logVar
          yield
          append Released logVar

        logContents <- readLog logVar
        logContents
          `shouldBe` [ Created,
                       Acquired,
                       Destroyed,
                       Created,
                       Acquired,
                       Destroyed,
                       Created,
                       Acquired,
                       Released
                     ]

    describe "with regards to sizing" do
      it "respects the size of the pool" do
        let maxResources = 10
        (pool, logVar) <- mkPoolWithLog defaultConfiguration {confMaxResources = maxResources}

        Async.replicateConcurrently_ 100 $ withResource pool $ const do
          append Acquired logVar
          threadDelay 100_000
          append Released logVar

        logContents <- readLog logVar
        let createdEntries = filter (== Created) logContents
            acquiredEntries = filter (== Acquired) logContents
        length createdEntries `shouldBe` maxResources
        length acquiredEntries `shouldSatisfy` (> maxResources)

      it "adjusts resource acquisition to respect resizing" do
        pool <- mkPool defaultConfiguration
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
        value <- fst <$> takeResource pool
        value `shouldBe` ()

    describe "when resources reach their idle limit" do
      it "destroys old resources" do
        let idleTime = 1
        let replications = 3
        (pool, logVar) <- mkPoolWithLog defaultConfiguration {confMaxResources = 10, confIdleTime = idleTime}

        Async.replicateConcurrently_ replications $ withResource pool $ const do
          append Acquired logVar
          sleep 0.1
          append Released logVar
        sleep (idleTime * 2)
        Async.replicateConcurrently_ replications $ withResource pool $ const do
          append Acquired logVar
          sleep 0.1
          append Released logVar

        logContents <- readLog logVar
        let createdEntries = filter (== Created) logContents
            destroyedEntries = filter (== Destroyed) logContents
            acquiredEntries = filter (== Acquired) logContents
        length createdEntries `shouldBe` (replications * 2)
        length destroyedEntries `shouldBe` (replications)
        length acquiredEntries `shouldBe` (replications * 2)

      it "does not hand out resources in the process of being destroyed" do
        let idleTime = 1
        (pool, logVar) <- mkPoolWithLog defaultConfiguration {confMaxResources = 10, confIdleTime = idleTime, confDestroyDelay = idleTime * 3}

        withResource pool $ const do
          append Acquired logVar
          sleep 0.1
          append Released logVar
        sleep (idleTime * 2)
        withResource pool $ const do
          append Acquired logVar
          sleep 0.1
          append Released logVar

        logContents <- readLog logVar
        let createdEntries = filter (== Created) logContents
            destroyedEntries = filter (== Destroyed) logContents
            acquiredEntries = filter (== Acquired) logContents
        length createdEntries `shouldBe` 2
        length destroyedEntries `shouldBe` 0 -- not yet destroyed
        length acquiredEntries `shouldBe` 2

    describe "with regards to timing out" do
      it "acquires a resource with a delay shorter than the timeout" do
        pool <- mkPool defaultConfiguration
        (res, lp) <- takeResource pool
        _ <- forkIO do
          sleep 0.3
          putResource lp res
        value <- fst <$> takeResource pool
        value `shouldBe` ()

      it "throws when the timeout expires and no resource is available" do
        pool <- mkPool defaultConfiguration
        _ <- takeResource pool
        (fst <$> takeResource pool) `shouldThrow` timeoutException

      it "throws when waiting for longer than the timeout" do
        pool <- mkPool defaultConfiguration
        (res, lp) <- takeResource pool
        _ <- forkIO do
          sleep 2
          putResource lp res
        (fst <$> takeResource pool) `shouldThrow` timeoutException

anIOError :: Selector IOError
anIOError = const True

timeoutException :: Selector TimeoutException
timeoutException = const True

mkPool :: PoolConfiguration -> IO (Pool ())
mkPool PoolConfiguration {..} =
  createPool'
    (pure ())
    (const (pure ()))
    confStripes
    confIdleTime
    confMaxResources
    confTimeout

mkPoolWithLog :: PoolConfiguration -> IO (Pool (), MVar Log)
mkPoolWithLog PoolConfiguration {..} = do
  logVar <- newMVar (Log [])
  pool <-
    createPool'
      (append Created logVar)
      ( const do
          sleep confDestroyDelay
          append Destroyed logVar
      )
      confStripes
      confIdleTime
      confMaxResources
      confTimeout
  pure (pool, logVar)

data PoolConfiguration = PoolConfiguration
  { confStripes :: Int,
    confIdleTime :: NominalDiffTime,
    confMaxResources :: Int,
    confTimeout :: Maybe NominalDiffTime,
    confDestroyDelay :: NominalDiffTime
  }

defaultConfiguration =
  PoolConfiguration
    { confStripes = 1,
      confIdleTime = 1000,
      confMaxResources = 1,
      confTimeout = Just 1,
      confDestroyDelay = 0
    }

newtype Log = Log {unLog :: [LogEntry]}

data LogEntry = Created | Destroyed | Acquired | Released
  deriving (Eq, Show)

append :: LogEntry -> MVar Log -> IO ()
append entry logVar = modifyMVar_ logVar (pure . appendPure entry)
  where
    appendPure :: LogEntry -> Log -> Log
    appendPure entry (Log log) = Log (entry : log)

readLog :: MVar Log -> IO [LogEntry]
readLog logVar = reverse . unLog <$> readMVar logVar

sleep :: NominalDiffTime -> IO ()
sleep diffTime = threadDelay (truncate (diffTime * 1_000_000))
