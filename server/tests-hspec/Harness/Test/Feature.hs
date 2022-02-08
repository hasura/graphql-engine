-- | Helper functions for easily testing features.
module Harness.Test.Feature
  ( feature,
    Feature (..),
    Backend (..),
  )
where

import Control.Exception
import Data.Foldable
import Harness.State (State)
import Test.Hspec
import Prelude

-- | Use this record to put together a test against a set of backends.
data Feature = Feature
  { backends :: [Backend],
    tests :: SpecWith State
  }

-- | A backend specification.
data Backend = Backend
  { -- | Can be any name you want (e.g. "PostgreSQL" or "MySQL v1.2")
    -- or whatnot.
    name :: String,
    -- | To setup the test suite: Run SQL commands, run metadata track
    -- tables calls.
    setup :: State -> IO (),
    -- | Clean up any resources you created in 'setup'.
    teardown :: State -> IO ()
  }

-- | Test the feature, running the setup before any tests are run, and
-- and ensuring teardown happens after all tests are run.
feature :: Feature -> SpecWith State
feature Feature {backends, tests} =
  for_
    backends
    ( \Backend {name, setup, teardown} ->
        describe
          name
          ( aroundAllWith
              ( \actionWith state -> do
                  finallyRethrow
                    ( do
                        setup state
                        actionWith state
                    )
                    (teardown state)
              )
              tests
          )
    )

-- | A custom 'finally' which re-throws exceptions from both the main action and the sequel action.
--
--   The standard 'finally' only re-throws the @sequel@ exception.
finallyRethrow :: IO a -> IO b -> IO a
finallyRethrow a sequel =
  mask $ \restore -> do
    r <-
      catch
        (restore a)
        ( \restoreEx -> do
            _ <- sequel `catch` (throwIO . Exceptions restoreEx)
            (throwIO restoreEx)
        )
    _ <- sequel
    pure r

-- | Two exceptions
data Exceptions
  = Exceptions SomeException SomeException

instance Show Exceptions where
  show (Exceptions e1 e2) =
    unlines
      [ "1. " <> show e1,
        "",
        "2. " <> show e2
      ]

instance Exception Exceptions
