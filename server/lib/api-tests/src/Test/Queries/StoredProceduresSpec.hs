{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- Stored Procedures is a pro-only feature now, this ensures that this continues
-- to be the case
module Test.Queries.StoredProceduresSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv []
    $ Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Sqlserver.setupTablesAction schema testEnvironment
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

schema :: [Schema.Table]
schema = []

tests :: SpecWith TestEnvironment
tests = do
  let databasesLogicalModel :: Schema.LogicalModel
      databasesLogicalModel =
        (Schema.logicalModel "databases")
          { Schema.logicalModelColumns =
              [ (Schema.logicalModelScalar "database_name" Schema.TStr)
                  { Schema.logicalModelColumnDescription = Just "db name"
                  },
                Schema.logicalModelScalar "database_size" Schema.TInt,
                Schema.logicalModelScalar "REMARKS" Schema.TStr
              ]
          }

      databasesStoredProcedure :: Schema.StoredProcedure
      databasesStoredProcedure = Schema.storedProcedure "sp_databases" "databases"

  describe "Testing Stored Procedures" $ do
    it "We cannot even set up a stored procedure in OSS" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      GraphqlEngine.postMetadata_
        testEnvironment
        (Schema.trackLogicalModelCommand source backendTypeMetadata databasesLogicalModel)

      -- we expect this to fail
      void
        $ GraphqlEngine.postMetadataWithStatus
          400
          testEnvironment
          (Schema.trackStoredProcedureCommand source backendTypeMetadata databasesStoredProcedure)
