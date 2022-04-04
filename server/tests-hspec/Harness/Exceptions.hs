{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | A replacement for "Control.Exception" with a few extra helpers.
module Harness.Exceptions
  ( catchRethrow,
    tryInOrder,
    forFinally_,
    module GHC.Stack,
    module Control.Exception.Safe,
    Exceptions (..),
  )
where

import Control.Exception.Safe
import Data.String
import GHC.Stack
import Hasura.Prelude hiding (first)

-- | Runs an action with a handler.
--
--   Will run the action. If the action fails and throws an exception,
--   it will run the cleanup and will throw the original exception after it is done.
--   If the cleanup fails as well, it will throw both raised exceptions.
catchRethrow :: HasCallStack => IO a -> IO a -> IO a
catchRethrow action cleanup =
  catch
    -- attempt action
    action
    ( \actionEx -> do
        -- try to cleanup the action
        -- if clean also fails, throw both errors
        _ <- catch cleanup (throwIO . Exceptions actionEx)
        -- if clean succeeds, throw the original error
        throwIO actionEx
    )

-- | Try actions in order. If one succeeds, it succeeds. If both fail, throw both exceptions.
tryInOrder :: HasCallStack => IO a -> IO a -> IO a
tryInOrder action1 action2 =
  catch
    action1
    ( \action1Ex ->
        catch
          action2
          (throwIO . Exceptions action1Ex)
    )

-- | Like 'for_', but uses 'finally' instead of '*>' to make sure all actions run even when
--   an exception occurs. Will throw the first error it runs into.
forFinally_ :: [a] -> (a -> IO ()) -> IO ()
forFinally_ list f =
  case list of
    [] -> pure ()
    x : xs -> f x `finally` forFinally_ xs f

-- | Two exceptions, bundled as one.
data Exceptions
  = Exceptions SomeException SomeException
  deriving anyclass (Exception)

instance Show Exceptions where
  show (Exceptions e1 e2) =
    unlines
      [ "1. " <> indentShow e1,
        "2. " <> indentShow e2
      ]
    where
      indentShow e =
        case lines (show e) of
          [] -> ""
          first : rest ->
            unlines $ first : map ("   " <>) rest
