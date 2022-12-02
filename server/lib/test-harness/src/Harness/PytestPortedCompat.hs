-- This module collects setup actions specific to tests ported mechanically
-- from the pytest suite.
module Harness.PytestPortedCompat (compatSetup) where

import Harness.Backend.Citus qualified as Citus
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Test.Fixture
import Harness.TestEnvironment
import Hasura.Prelude

compatSetup :: TestEnvironment -> BackendType -> IO ()
compatSetup testEnvironment backend = do
  defaultSourceMetadata <- case backend of
    Citus -> do
      Citus.run_ testEnvironment "DROP SCHEMA IF EXISTS hasura"
      return (Citus.defaultSourceMetadata testEnvironment)
    _ -> error $ "'compatSetup' not yet defined for backend " ++ show backend

  GraphqlEngine.setSource testEnvironment defaultSourceMetadata Nothing
