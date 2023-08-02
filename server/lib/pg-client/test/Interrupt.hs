{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interrupt (specInterrupt) where

-------------------------------------------------------------------------------

import Control.Concurrent
  ( MVar,
    newEmptyMVar,
    putMVar,
    threadDelay,
    tryReadMVar,
  )
import Control.Concurrent.Interrupt (interruptOnAsyncException)
import Control.Exception.Safe (Exception, onException, throwIO, uninterruptibleMask_)
import Control.Monad (liftM2, unless)
import Data.Bifunctor (first)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)
import Prelude hiding (log)

-------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore avoid Control.Concurrent.threadDelay" :: String) #-}

specInterrupt :: Spec
specInterrupt = do
  describe "without interrupt" $ do
    it "logging etc works" $ do
      events <- withLogger $ \log -> do
        let action = trace log "sleep" $ sleep (1000 * ms)
        res <- timeout (500 * ms) action
        log "done"
        res `shouldBe` Nothing
      roundLog (100 * ms) events
        `shouldBe` [ (0, "sleep start"),
                     (500 * ms, "sleep exception"),
                     (500 * ms, "done")
                   ]
    it "cancellable sleep is like sleep without cancelling" $ do
      events <- withLogger $ \log -> do
        let action = trace log "sleep" $ cancellableSleep (1000 * ms) (pure False)
        res <- timeout (500 * ms) action
        log "done"
        res `shouldBe` Nothing
      roundLog (100 * ms) events
        `shouldBe` [ (0, "sleep start"),
                     (500 * ms, "sleep exception"),
                     (500 * ms, "done")
                   ]
    it "uninterruptible sleep doesn't time out" $ do
      events <- withLogger $ \log -> do
        let action = trace log "outer" $ do
              uninterruptibleMask_ $ trace log "sleep" $ cancellableSleep (1000 * ms) (pure False)
              -- add an extra action so the timeout is delivered reliably
              sleep (500 * ms)
        res <- timeout (500 * ms) action
        log "done"
        res `shouldBe` Nothing
      roundLog (100 * ms) events
        `shouldBe` [ (0, "outer start"),
                     (0, "sleep start"),
                     (1000 * ms, "sleep end"),
                     (1000 * ms, "outer exception"),
                     (1000 * ms, "done")
                   ]

  describe "interruptOnAsyncException" $ do
    it "behaves like baseline without cancelling" $ do
      events <- withLogger $ \log -> do
        let action = interruptOnAsyncException (pure ()) $ trace log "sleep" $ sleep (1000 * ms)
        res <- timeout (500 * ms) action
        log "done"
        res `shouldBe` Nothing
      roundLog (100 * ms) events
        `shouldBe` [ (0, "sleep start"),
                     (500 * ms, "sleep exception"),
                     (500 * ms, "done")
                   ]
    it "allows interrupting a blocking action" $ do
      (cancel, cancelled) <- getCancel
      events <- withLogger $ \log -> do
        let action = trace log "outer" $ do
              interruptOnAsyncException cancel $ uninterruptibleMask_ $ trace log "sleep" $ cancellableSleep (1000 * ms) cancelled
        res <- timeout (500 * ms) action
        log "done"
        res `shouldBe` Nothing
      roundLog (100 * ms) events
        `shouldBe` [ (0, "outer start"),
                     (0, "sleep start"),
                     (500 * ms, "sleep end"),
                     (500 * ms, "outer exception"),
                     (500 * ms, "done")
                   ]
    it "waits for the thread and bubbles the exception if cancel only throws" $ do
      (_cancel, cancelled) <- getCancel
      let cancel = throwIO CancelException
      events <- withLogger $ \log -> do
        let action = trace log "outer" $ do
              interruptOnAsyncException cancel $ uninterruptibleMask_ $ trace log "sleep" $ cancellableSleep (1000 * ms) cancelled
        timeout (500 * ms) action `shouldThrow` (== CancelException)
        log "done"
      -- the important property is that we always get "sleep"'s end/exception before "outer"'s end/exception
      roundLog (100 * ms) events
        `shouldBe` [ (0, "outer start"),
                     (0, "sleep start"),
                     (1000 * ms, "sleep end"),
                     (1000 * ms, "outer exception"),
                     (1000 * ms, "done")
                   ]
    it "bubbles an exception that occurs before cancelling" $ do
      (cancel, cancelled) <- getCancel
      events <- withLogger $ \log -> do
        let action = trace log "outer" $ do
              interruptOnAsyncException cancel $
                uninterruptibleMask_ $
                  trace log "sleep" $ do
                    sleep (200 * ms)
                    throwIO ActionException :: IO ()
                    cancellableSleep (800 * ms) cancelled
        timeout (500 * ms) action `shouldThrow` (== ActionException)
        log "done"
      roundLog (100 * ms) events
        `shouldBe` [ (0, "outer start"),
                     (0, "sleep start"),
                     (200 * ms, "sleep exception"),
                     (200 * ms, "outer exception"),
                     (200 * ms, "done")
                   ]
    it "bubbles an exception that occurs after cancelling" $ do
      (cancel, cancelled) <- getCancel
      events <- withLogger $ \log -> do
        let action = trace log "outer" $ do
              interruptOnAsyncException cancel $
                uninterruptibleMask_ $
                  trace log "sleep" $ do
                    cancellableSleep (1000 * ms) cancelled
                    throwIO ActionException
        timeout (500 * ms) action `shouldThrow` (== ActionException)
        log "done"
      roundLog (100 * ms) events
        `shouldBe` [ (0, "outer start"),
                     (0, "sleep start"),
                     (500 * ms, "sleep exception"),
                     (500 * ms, "outer exception"),
                     (500 * ms, "done")
                   ]

-- millisecond in microseconds
ms :: Int
ms = 1000

-- second in microseconds
s :: Int
s = 1000000

sleep :: Int -> IO ()
sleep = threadDelay

cancellableSleep :: Int -> IO Bool -> IO ()
cancellableSleep t cancelled = do
  t0 <- getCurrentTime
  let done = do
        t1 <- getCurrentTime
        return $ diffUTCTime t1 t0 * fromIntegral s >= fromIntegral t
  spinUntil (liftM2 (||) done cancelled)
  where
    spinUntil cond = do
      stop <- cond
      unless stop $ do
        threadDelay (1 * ms)
        spinUntil cond

getCancel :: IO (IO (), IO Bool)
getCancel = do
  c :: MVar () <- newEmptyMVar
  let cancel = putMVar c ()
      cancelled = isJust <$> tryReadMVar c
  return (cancel, cancelled)

data CancelException = CancelException
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ActionException = ActionException
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

type Log = [(NominalDiffTime, String)]

roundTo :: Int -> NominalDiffTime -> Int
roundTo interval t = round (t * fromIntegral s / fromIntegral interval) * interval

roundLog :: Int -> Log -> [(Int, String)]
roundLog interval = map (first (roundTo interval))

withLogger :: ((String -> IO ()) -> IO ()) -> IO Log
withLogger f = do
  ref :: IORef Log <- newIORef []
  t0 <- getCurrentTime
  let log event = do
        t <- getCurrentTime
        atomicModifyIORef' ref (\events -> ((diffUTCTime t t0, event) : events, ()))
  f log
  reverse <$> readIORef ref

trace :: (String -> IO ()) -> String -> IO () -> IO ()
trace log label action = do
  log $ label <> " start"
  action `onException` log (label <> " exception")
  log $ label <> " end"
