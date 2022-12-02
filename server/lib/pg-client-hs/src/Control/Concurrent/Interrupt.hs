{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Interrupt
  ( interruptOnAsyncException,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent.Async (async, asyncThreadId, wait, waitCatch)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    mask,
    throwIO,
    throwTo,
    try,
  )
import Prelude

-------------------------------------------------------------------------------

-- | interruptOnAsyncexception runs the given action in in a separate thread,
-- running the given cancel action before passing on any asynchronous
-- exceptions to that thread. The intent is that
--   'interruptOnAsyncException (pure ()) == id'
-- in all respects (including exception handling), assuming the wrapped
-- action behaves somewhat reasonably (i.e., doesn't swallow asynchronous
-- exceptions). Particularly, we guarantee that the separate thread terminates
-- before we return. (It's not entirely transparent: for instance, 'myThreadId'
-- returns a different value.)
--
-- The point of this is to allow breaking out of blocking actions if they
-- provide some cancelling escape hatch.
interruptOnAsyncException :: IO () -> IO a -> IO a
interruptOnAsyncException interrupt action = mask $ \restore -> do
  x <- async action

  -- By using 'try' with 'waitCatch', we can distinguish between asynchronous
  -- exceptions received from the outside, and those thrown by the wrapped action.
  -- (The latter shouldn't occur, but we also want to avoid throwing an exception
  -- back at the thread below.)
  res :: Either SomeAsyncException (Either SomeException a) <-
    try $ restore (waitCatch x)
  case res of
    -- Due to the use of 'waitCatch' above, the only exceptions that 'tryAsync'
    -- might catch are asynchronous exceptions received from the "outside".
    -- Thus, the 'Left' case is the only one where the async action has not
    -- necessarily terminated.
    Left e -> do
      -- Cancelling might throw an exception; we save that and re-raise it,
      -- but not before doing or job of passing the asynchronous exception on
      -- to our child and waiting for it to terminate.
      interruptRes :: Either SomeException () <- try interrupt
      throwTo (asyncThreadId x) e
      waitRes :: Either SomeException a <- try $ wait x
      case (interruptRes, waitRes) of
        (Left cancelEx, _) -> throwIO cancelEx
        -- waitEx could be an exception thrown by the action, or our async
        -- exception bubbling back up
        (Right _, Left waitEx) -> throwIO waitEx
        -- in case the async exits cleanly before receiving the exception, we
        -- re-raise it manually so as to not swallow it, since the action
        -- /was/ interrupted
        (Right _, Right _) -> throwIO e

    -- In the non-interrupted case, we "undo" the 'try', collapsing things
    -- effectively to 'restore (wait x)'.
    Right (Left e) ->
      throwIO e
    Right (Right r) ->
      pure r
