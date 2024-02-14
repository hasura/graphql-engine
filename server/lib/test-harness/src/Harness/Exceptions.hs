{-# LANGUAGE DeriveAnyClass #-}

-- | A replacement for "Control.Exception" with a few extra helpers.
module Harness.Exceptions
  ( catchRethrow,
    tryInOrder,
    forFinally_,
    rethrowAll,
    module GHC.Stack,
    module Control.Exception.Safe,
    Exceptions (..),
  )
where

import Control.Exception.Safe
import Data.List.NonEmpty qualified as NE
import Data.String
import GHC.Stack
import Hasura.Prelude hiding (first)

-- | Runs an action with a handler.
--
--   Will run the action. If the action fails and throws an exception,
--   it will run the cleanup and will throw the original exception after it is done.
--   If the cleanup fails as well, it will throw both raised exceptions.
catchRethrow :: (HasCallStack) => IO a -> IO () -> IO a
catchRethrow action cleanup =
  catch
    -- attempt action
    action
    ( \actionEx -> do
        -- try to cleanup the action
        -- if clean also fails, throw both errors
        _ <- catch cleanup (throwIO . Exceptions . (actionEx NE.:|) . (: []))
        -- if clean succeeds, throw the original error
        throwIO actionEx
    )

-- | Try actions in order. If one succeeds, it succeeds. If both fail, throw both exceptions.
tryInOrder :: (HasCallStack) => IO a -> IO a -> IO a
tryInOrder action1 action2 =
  catch
    action1
    ( \action1Ex ->
        catch
          action2
          (throwIO . Exceptions . (\action2Ex -> action1Ex NE.:| [action2Ex]))
    )

-- | Like 'for_', but uses 'finally' instead of '*>' to make sure all actions run even when
--   an exception occurs. Will throw the first error it runs into.
forFinally_ :: [a] -> (a -> IO ()) -> IO ()
forFinally_ list f =
  case list of
    [] -> pure ()
    x : xs -> f x `finally` forFinally_ xs f

-- | Run a list of IO actions, collecting and rethrowing all exceptions that are
-- raised as a single 'Exceptions' exception. If 'Exceptions' thrown in the
-- 'actions', these are collapsed into a single top-level 'Exceptions'
-- exception.
rethrowAll :: (HasCallStack) => [IO ()] -> IO ()
rethrowAll actions = do
  exns <-
    concat
      <$> mapM
        (\action -> handle (return . collectExns) ([] <$ action))
        actions
  case NE.nonEmpty exns of
    Nothing -> return ()
    Just (ex NE.:| []) -> throwIO ex
    Just exnsNE -> throwIO (Exceptions exnsNE)
  where
    collectExns :: SomeException -> [SomeException]
    collectExns exn | Just (Exceptions exns) <- fromException exn = concatMap collectExns exns
    collectExns exn = [exn]

-- | Two exceptions, bundled as one.
data Exceptions
  = Exceptions (NE.NonEmpty SomeException)
  deriving anyclass (Exception)

instance Show Exceptions where
  show (Exceptions exns) =
    unlines
      [ show i ++ "." ++ exnString
        | (i, exnString) <- zip [1 :: Int ..] (map indentShow (NE.toList exns))
      ]
    where
      indentShow e =
        case lines (show e) of
          [] -> ""
          first : rest ->
            unlines $ first : map ("   " <>) rest
