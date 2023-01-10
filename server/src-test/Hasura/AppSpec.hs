{-# LANGUAGE NumericUnderscores #-}

module Hasura.AppSpec (spec) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Extended (sleep)
import Control.Exception (throwIO)
import Hasura.Prelude
import Hasura.ShutdownLatch (newShutdownLatch, shutdownGracefully, shuttingDown, waitForShutdown)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "ShutdownLatch" shutdownLatchSpec

shutdownLatchSpec :: Spec
shutdownLatchSpec = do
  it "waitForShutdown blocks before shutdown, not after" $ do
    latch <- newShutdownLatch
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Nothing
    timeout 10_000 (shutdownGracefully latch)
      `shouldReturn` Just ()
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Just ()

  it "allows multiple calls to shutdownGracefully" $ do
    latch <- newShutdownLatch
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Nothing
    timeout 10_000 (shutdownGracefully latch)
      `shouldReturn` Just ()
    timeout 10_000 (shutdownGracefully latch)
      `shouldReturn` Just ()
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Just ()

  it "allows shutting down a thread" $ do
    latch <- newShutdownLatch
    Async.withAsync (waitForShutdown latch >> return ("shut down" :: String)) $ \async -> do
      sleep 0.01

      pollThrow async
        `shouldReturn` Nothing
      shutdownGracefully latch
      timeout 1_000_000 (Async.wait async)
        `shouldReturn` Just "shut down"

  it "allows multiple threads to wait for shutdown" $ do
    latch <- newShutdownLatch
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Nothing
    shutdownGracefully latch
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Just ()
    timeout 10_000 (waitForShutdown latch)
      `shouldReturn` Just ()

  it "checks if a latch is ready for shutdown" $ do
    latch <- newShutdownLatch
    shuttingDown latch `shouldReturn` False
    shutdownGracefully latch
    shuttingDown latch `shouldReturn` True

pollThrow :: Async.Async a -> IO (Maybe a)
pollThrow async = do
  res <- Async.poll async
  case res of
    Just (Left e) -> throwIO e
    Just (Right x) -> pure $ Just x
    Nothing -> pure Nothing
