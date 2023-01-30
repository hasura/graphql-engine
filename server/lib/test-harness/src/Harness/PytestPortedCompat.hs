{-# LANGUAGE QuasiQuotes #-}

-- This module collects setup actions specific to tests ported mechanically
-- from the pytest suite.
module Harness.PytestPortedCompat (compatSetup) where

import Data.Aeson
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Fixture
import Harness.TestEnvironment
import Hasura.Prelude

compatSetup :: TestEnvironment -> BackendType -> IO ()
compatSetup testEnvironment backend = do
  defaultSourceMetadata <- case backend of
    Citus -> do
      Citus.run_ testEnvironment "DROP SCHEMA IF EXISTS hasura"
      return (Citus.defaultSourceMetadata testEnvironment)
    Postgres -> do
      Postgres.run_ testEnvironment "DROP SCHEMA IF EXISTS hasura"
      return (postgresDefaultSourceMetadata testEnvironment)
    _ -> error $ "'compatSetup' not yet defined for backend " ++ show backend

  GraphqlEngine.setSource testEnvironment defaultSourceMetadata Nothing

-- | The pytests assume that the postgres source is called 'default', and often
-- omit it entirely.
postgresDefaultSourceMetadata :: TestEnvironment -> Value
postgresDefaultSourceMetadata testEnv =
  [interpolateYaml|
    name: default
    kind: pg
    tables: []
    configuration: #{ Postgres.defaultSourceConfiguration testEnv }
  |]
