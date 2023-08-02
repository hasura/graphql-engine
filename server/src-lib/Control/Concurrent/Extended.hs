{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use withAsync" #-}

module Control.Concurrent.Extended
  ( module Control.Concurrent,
    sleep,
    ForkableMonadIO,

    -- * Robust forking
    forkImmortal,
    forkManagedT,
    forkManagedTWithGracefulShutdown,

    -- * Concurrency in MonadError
    forConcurrentlyEIO,
    concurrentlyEIO,

    -- * Deprecated
    ImmortalThreadLog (..),
    ThreadState (..),
    ThreadShutdown (..),
    Forever (..),
  )
where

import Control.Concurrent hiding (threadDelay)
import Control.Concurrent qualified as Base
import Control.Concurrent.Async as A
import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.STM qualified as STM
import Control.Exception
import Control.Immortal qualified as Immortal
import Control.Monad.Except
import Control.Monad.Loops (iterateM_)
import Control.Monad.Trans.Control qualified as MC
import Control.Monad.Trans.Managed (ManagedT (..), allocate)
import Data.Aeson
import Data.List.Split
import Data.Traversable
import Data.Void
-- For forkImmortal. We could also have it take a cumbersome continuation if we
-- want to break this dependency. Probably best to move Hasura.Logging into a
-- separate lib with this if we do the override thing.
import Hasura.Logging
import Hasura.Prelude

{-# HLINT ignore sleep #-}

-- | Like 'Base.threadDelay', but takes a 'DiffTime' instead of an 'Int' microseconds.
--
-- NOTE: you cannot simply replace e.g. @threadDelay 1000@ with @sleep 1000@ since those literals
-- have different meanings!
sleep :: DiffTime -> IO ()
sleep = Base.threadDelay . round . Microseconds

-- | Note: Please consider using 'forkManagedT' instead to ensure reliable
-- resource cleanup.
forkImmortal ::
  (ForkableMonadIO m) =>
  -- | A label describing this thread's function (see 'labelThread').
  String ->
  Logger Hasura ->
  -- | An IO action we expect never to return normally. This will have the type
  -- signature ':: m a' (see e.g. the type of 'forever').
  m Void ->
  -- | A handle for the forked thread. See "Control.Immortal".
  m Immortal.Thread
forkImmortal label logger m =
  Immortal.createWithLabel label $ \this -> do
    -- Log that the thread has started
    liftIO $ unLogger logger (ImmortalThreadRestarted label)
    -- In this case, we are handling unexpected exceptions.
    -- i.e This does not catch the asynchronous exception which stops the thread.
    Immortal.onUnexpectedFinish this logAndPause (void m)
  where
    logAndPause = \case
      Right _void -> pure () -- absurd _void (i.e. unreachable)
      Left e -> liftIO $ do
        liftIO $ unLogger logger (ImmortalThreadUnexpectedException label e)
        -- pause before restarting some arbitrary amount of time. The idea is not to flood
        -- logs or cause other cascading failures.
        sleep (seconds 1)

data ThreadState = ThreadForked | ThreadBlocking | ThreadShutdownInitiated
  deriving (Show, Eq)

-- | @ThreadShutdown@ is a newtype wrapper over an action which is intended
--   to execute when a thread's shutdown is initiated before killing the thread
newtype ThreadShutdown m = ThreadShutdown {tsThreadShutdown :: m ()}

-- | This function pairs a call to 'forkImmortal' with a finalizer which stops
-- the immortal thread.

-- Note, the thread object can leave its scope if this function is incorrectly
-- used. Generally, the result should only be used later in the same ManagedT
-- scope.
forkManagedT ::
  (ForkableMonadIO m) =>
  String ->
  Logger Hasura ->
  m Void ->
  ManagedT m Immortal.Thread
forkManagedT label logger m =
  allocate
    (forkImmortal label logger m)
    ( \thread -> do
        unLogger logger (ImmortalThreadStopping label)
        liftIO $ Immortal.stop thread
    )

-- | The @Forever@ type defines an infinite looping monadic action (like @m void@), but allows the
-- caller to control the recursion or insert code before each iteration. The @a@ is the initial argument,
-- and subsequent iterations will be fed the argument returned by the previous one.  See
-- @forkManagedTWithGracefulShutdown@ to see how it's used
data Forever m = forall a. Forever a (a -> m a)

-- | @forkManagedTWithGracefulShutdown@ is an extension of the @forkManagedT@
--   function this function also attempts to gracefully shutdown the thread. This function
--   accepts a `m (Forever m)` argument. The @Forever@ type contains a function and an argument
--   to the function. The function supplied will be run repeatedly until shutdown is initiated. The
--   response of the function will be the argument to the next iteration.
--
--   For reference, this function is used to run the async actions processor. Check
--   `asyncActionsProcessor`
forkManagedTWithGracefulShutdown ::
  (ForkableMonadIO m) =>
  String ->
  Logger Hasura ->
  ThreadShutdown m ->
  m (Forever m) ->
  ManagedT m Immortal.Thread
forkManagedTWithGracefulShutdown label logger (ThreadShutdown threadShutdownHandler) loopIteration = do
  threadStateTVar <- liftIO $ STM.newTVarIO ThreadForked
  allocate
    ( Immortal.createWithLabel label $ \this -> do
        -- Log that the thread has started
        liftIO $ unLogger logger (ImmortalThreadRestarted label)
        -- In this case, we are handling unexpected exceptions.
        -- i.e This does not catch the asynchronous exception which stops the thread.
        Immortal.onUnexpectedFinish this logAndPause
          $ ( do
                let mLoop (Forever loopFunctionInitArg loopFunction) =
                      flip iterateM_ loopFunctionInitArg $ \args -> do
                        liftIO
                          $ STM.atomically
                          $ do
                            STM.readTVar threadStateTVar >>= \case
                              ThreadShutdownInitiated -> do
                                -- signal to the finalizer that we are now blocking
                                -- and blocking forever since this
                                -- var moves monotonically from forked -> shutdown -> blocking
                                STM.writeTVar threadStateTVar ThreadBlocking
                              ThreadBlocking -> STM.retry
                              ThreadForked -> pure ()
                        loopFunction args
                t <- LA.async $ mLoop =<< loopIteration
                LA.link t
                void $ LA.wait t
            )
    )
    ( \thread -> do
        liftIO
          $ STM.atomically
          $ STM.modifyTVar' threadStateTVar (const ThreadShutdownInitiated)
        -- the threadShutdownHandler here will wait for any in-flight events
        -- to finish processing
        {-
            There is a conundrum here about whether the @threadShutdownHandler@
            should be before or after the @ThreadBlocking@ check call, this is because
            there are problems with both the cases:

            1. @threadShutdownHandler@ before the @ThreadBlocking@ check
            ------------------------------------------------------------

            Let's say we're just about to start processing a new iteration of the
            loop function and before the processing actually starts the shutdown is
            initiated, there will be no in-flight events (because the batch hasn't started processing yet) so
            @threadShutdownHandler@ will return immediately and the new batch will start processing
            which were fetched earlier. This is a race condition and may kill the thread with some
            of the events still processing.

            2. @threadShutdownHandler@ after the @ThreadBlocking@ check
            -----------------------------------------------------------

            This will solve the above race condition but will cause a new problem. The
            graphql-engine accepts a config called `--graceful-shutdown-timeout` which is a timeout
            for any in-flight processing events that are running in the graphql-engine to complete
            processing within this time.

            Let's say we are going to start iterating over the next iteration of `processEventQueue`
            and without loss of generality let's say this batch takes 100 seconds to finish processing
            and the graceful shutdown timeout is 10 seconds and shutdown is initiated in the midst of processing
            this batch, this will have no effect and the thread will be shutdown after the batch completes (after
            100 seconds) which is wrong because it doesn't respect the graceful shutdown timeout

            TODO: figure out a way which solves both the problems

            At the time of writing this PR, we decided to go with 1 because the worst thing
            that will happen is that some events might get processed more than once but this
            is a better solution than what we had earlier where we were shutting down all the in-flight
            processing events without the graceful shutdown timeout.
        -}
        threadShutdownHandler
        liftIO
          $ STM.atomically
          $ do
            STM.readTVar threadStateTVar >>= STM.check . (== ThreadBlocking)
        unLogger logger (ImmortalThreadStopping label)
        liftIO $ Immortal.stop thread
    )
  where
    logAndPause = \case
      Right () -> pure ()
      Left e -> liftIO $ do
        liftIO $ unLogger logger (ImmortalThreadUnexpectedException label e)
        -- pause before restarting some arbitrary amount of time. The idea is not to flood
        -- logs or cause other cascading failures.
        sleep (seconds 1)

data ImmortalThreadLog
  = -- | Synchronous Exception
    ImmortalThreadUnexpectedException String SomeException
  | -- | Asynchronous Exception about to be sent
    ImmortalThreadStopping String
  | ImmortalThreadRestarted String

instance ToEngineLog ImmortalThreadLog Hasura where
  toEngineLog (ImmortalThreadStopping label) =
    (LevelInfo, ELTInternal ILTUnstructured, toJSON msg)
    where
      msg = "Stopping immortal " <> label <> " thread"
  toEngineLog (ImmortalThreadUnexpectedException label e) =
    (LevelError, ELTInternal ILTUnstructured, toJSON msg)
    where
      msg =
        "Unexpected exception in immortal thread "
          <> label
          <> " (it will be restarted):\n"
          <> show e
  toEngineLog (ImmortalThreadRestarted label) =
    (LevelInfo, ELTInternal ILTUnstructured, toJSON msg)
    where
      msg = "Thread " <> label <> " (re)started"

-- TODO
--   - maybe use this everywhere, but also:
--     - consider unifying with: src-lib/Control/Monad/Stateless.hs  ?
--   - nice TypeError:  https://kodimensional.dev/type-errors
--

-- | Like 'MonadIO' but constrained to stacks in which forking a new thread is reasonable/safe.
-- In particular 'StateT' causes problems.
--
-- This is the constraint you can use for functions that call 'LA.async', or 'immortal'.
type ForkableMonadIO m = (MonadIO m, MC.MonadBaseControl IO m, LA.Forall (LA.Pure m))

-- TODO consider deprecating async.
--        export something with polymorphic return type, which makes "fork and forget" difficult
--        this could automatically link in one variant
--        another variant might return ThreadId that self destructs w/ finalizer (mkWeakThreadId)
--          and note: "Holding a normal ThreadId reference will prevent the delivery of BlockedIndefinitely exceptions because the reference could be used as the target of throwTo at any time,  "

-- | A somewhat wonky function for parallelizing @for xs f@ where @f@ is
-- @(MonadIO m, MonadError e m)@. This is equivalent to @for xs f@ modulo the
-- IO effects (i.e. when the IO has no real side effects we care about).
--
-- This also takes a @chunkSize@ argument so you can manipulate the amount of
-- work given to each thread.
forConcurrentlyEIO :: (MonadIO m, MonadError e m) => Int -> [a] -> (a -> ExceptT e IO b) -> m [b]
forConcurrentlyEIO chunkSize xs f = do
  let fIO a = runExceptT (f a) >>= evaluate
  xs' <- liftIO $ fmap concat $ A.forConcurrently (chunksOf chunkSize xs) $ traverse fIO
  for xs' (either throwError pure)

concurrentlyEIO :: (MonadIO m, MonadError e m) => ExceptT e IO a -> ExceptT e IO b -> m (a, b)
concurrentlyEIO left right = do
  (leftE, rightE) <- liftIO $ A.concurrently (runExceptT left >>= evaluate) (runExceptT right >>= evaluate)
  x <- leftE `onLeft` throwError
  y <- rightE `onLeft` throwError
  pure (x, y)
