module Control.Concurrent.Extended
  ( module Control.Concurrent
  , sleep
  , ForkableMonadIO
  -- * Robust forking
  , forkImmortal
  , forkManagedT
  -- * Deprecated
  , threadDelay
  , forkIO
  ) where

import           Prelude
import           Control.Exception
import           Control.Monad.Trans.Managed          (ManagedT(..), allocate)
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
import           Data.Void

import qualified Control.Concurrent                   as Base
import qualified Control.Concurrent.Async.Lifted.Safe as LA
import qualified Control.Immortal                     as Immortal
import qualified Control.Monad.Trans.Control          as MC

import           Control.Concurrent                   hiding (threadDelay, forkIO)
import           Data.Time.Clock.Units                (seconds, Microseconds (..), DiffTime)


-- For forkImmortal. We could also have it take a cumbersome continuation if we
-- want to break this dependency. Probably best to move Hasura.Logging into a
-- separate lib with this if we do the override thing.
import           Hasura.Logging

-- | Like 'Base.threadDelay', but takes a 'DiffTime' instead of an 'Int' microseconds.
--
-- NOTE: you cannot simply replace e.g. @threadDelay 1000@ with @sleep 1000@ since those literals
-- have different meanings!
sleep :: DiffTime -> IO ()
sleep = Base.threadDelay . round . Microseconds

{-# DEPRECATED threadDelay "Please use `sleep` instead (and read the docs!)" #-}
threadDelay :: Int -> IO ()
threadDelay = Base.threadDelay

{-# DEPRECATED forkIO 
   "Please use 'Control.Control.Concurrent.Async.Lifted.Safe.withAsync'\
  \ or our 'forkImmortal' instead formore robust threading." #-}
forkIO :: IO () -> IO ThreadId 
forkIO = Base.forkIO

-- | Note: Please consider using 'forkManagedT' instead to ensure reliable
-- resource cleanup.
forkImmortal 
  :: ForkableMonadIO m
  => String 
  -- ^ A label describing this thread's function (see 'labelThread').
  -> Logger Hasura
  -> m Void
  -- ^ An IO action we expect never to return normally. This will have the type
  -- signature ':: m a' (see e.g. the type of 'forever').
  -> m Immortal.Thread
  -- ^ A handle for the forked thread. See "Control.Immortal".
forkImmortal label logger m =
  Immortal.createWithLabel label $ \this -> do
    -- Log that the thread has started
    liftIO $ unLogger logger (ImmortalThreadRestarted label )
    -- In this case, we are handling unexpected exceptions.
    -- i.e This does not catch the asynchronous exception which stops the thread.
    Immortal.onUnexpectedFinish this logAndPause (void m)
    where logAndPause = \case
            Right _void -> pure () -- absurd _void (i.e. unreachable)
            Left e  -> liftIO $ do
              liftIO $ unLogger logger (ImmortalThreadUnexpectedException label e)
              -- pause before restarting some arbitrary amount of time. The idea is not to flood 
              -- logs or cause other cascading failures.
              sleep (seconds 1)

-- | This function pairs a call to 'forkImmortal' with a finalizer which stops
-- the immortal thread.
--
-- Note, the thread object can leave its scope if this function is incorrectly
-- used. Generally, the result should only be used later in the same ManagedT
-- scope.
forkManagedT 
  :: ForkableMonadIO m
  => String
  -> Logger Hasura
  -> m Void
  -> ManagedT m Immortal.Thread
forkManagedT label logger m = allocate
  (forkImmortal label logger m)
  (\thread -> do
    unLogger logger (ImmortalThreadStopping label)
    liftIO $ Immortal.stop thread)
    
data ImmortalThreadLog 
  = ImmortalThreadUnexpectedException String SomeException
  -- ^ Synchronous Exception 
  | ImmortalThreadStopping String
  -- ^ Asynchronous Exception about to be sent
  | ImmortalThreadRestarted String

instance ToEngineLog ImmortalThreadLog Hasura where
  toEngineLog (ImmortalThreadStopping label) = 
    (LevelInfo, ELTInternal ILTUnstructured, toJSON msg)
      where msg = "Stopping immortal " <> label <> " thread"
  toEngineLog (ImmortalThreadUnexpectedException label e) =
    (LevelError, ELTInternal ILTUnstructured, toJSON msg)
      where msg = "Unexpected exception in immortal thread " <> label <> " (it will be restarted):\n"
               <> show e
  toEngineLog (ImmortalThreadRestarted label) = 
    (LevelInfo, ELTInternal ILTUnstructured, toJSON msg)
      where msg = "Thread " <> label <> " (re)started"

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
