-- | Helper functions for easily testing features.
module Harness.Feature
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
              ( \actionWith server -> do
                  setup server
                  finally (actionWith server) (teardown server)
              )
              tests
          )
    )
