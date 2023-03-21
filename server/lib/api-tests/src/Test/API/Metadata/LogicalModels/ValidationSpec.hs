module Test.API.Metadata.LogicalModels.ValidationSpec where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] do
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Postgres.setupTablesAction schema testEnv
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Citus.setupTablesAction schema testEnv
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

schema :: [Schema.Table]
schema =
  [ (Schema.table "stuff")
      { Schema.tableColumns =
          [ Schema.column "thing" Schema.TInt,
            Schema.column "date" Schema.TUTCTime
          ]
      }
  ]

-- ** Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  describe "Validation fails on untrack a logical model" do
    it "when a logical model does not exist" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            nonExistentLogicalModel :: Schema.LogicalModel
            nonExistentLogicalModel = Schema.logicalModel "some_logical_model" ""

            expectedError = "Logical model \"some_logical_model\" not found in source \"" <> sourceName <> "\"."

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnvironment
              (Schema.untrackLogicalModelCommand sourceName backendTypeMetadata nonExistentLogicalModel)
          )
          [yaml|
          code: not-found
          error: *expectedError
          path: "$.args"
        |]

  describe "Validation fails on track a logical model" do
    it "when the query has a syntax error" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        let spicyQuery :: Text
            spicyQuery = "query bad"

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                  path: "$.args"
              |]

            syntaxErrorLogicalModel :: Schema.LogicalModel
            syntaxErrorLogicalModel =
              (Schema.logicalModel "divided_stuff" spicyQuery)
                { Schema.logicalModelColumns =
                    [ (Schema.logicalModelColumn "divided" Schema.TInt)
                        { Schema.logicalModelColumnDescription = Just "A divided thing"
                        }
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" Schema.TInt,
                      Schema.logicalModelColumn "target_date" Schema.TUTCTime
                    ]
                }

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackLogicalModelCommand sourceName backendTypeMetadata syntaxErrorLogicalModel)

        actual `shouldAtLeastBe` expected

    it "when the query refers to non existing table" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        let spicyQuery :: Text
            spicyQuery = "SELECT thing / {{denominator}} AS divided FROM does_not_exist WHERE date = {{target_date}}"

            brokenLogicalModel :: Schema.LogicalModel
            brokenLogicalModel =
              (Schema.logicalModel "divided_stuff" spicyQuery)
                { Schema.logicalModelColumns =
                    [ (Schema.logicalModelColumn "divided" Schema.TInt)
                        { Schema.logicalModelColumnDescription = Just "A divided thing"
                        }
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" Schema.TInt,
                      Schema.logicalModelColumn "target_date" Schema.TUTCTime
                    ]
                }

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                  internal:
                    error:
                      message: "relation \"does_not_exist\" does not exist"
                      status_code: "42P01"
              |]

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackLogicalModelCommand sourceName backendTypeMetadata brokenLogicalModel)

        actual `shouldAtLeastBe` expected

    it "when the logical model has the same name as an already tracked table" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "select * from stuff"

            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
            sourceName = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnv

            conflictingLogicalModel :: Schema.LogicalModel
            conflictingLogicalModel =
              (Schema.logicalModel (Schema.unSchemaName schemaName <> "_stuff") spicyQuery)
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelColumn "thing" Schema.TInt,
                      Schema.logicalModelColumn "date" Schema.TUTCTime
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" Schema.TInt,
                      Schema.logicalModelColumn "target_date" Schema.TUTCTime
                    ]
                }

            expectedError = "Encountered conflicting definitions in the selection set for 'subscription_root' for field 'hasura_stuff' defined in [table hasura.stuff in source " <> sourceName <> ", logical_model hasura_stuff in source " <> sourceName <> "]. Fields must not be defined more than once across all sources."

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              500
              testEnv
              (Schema.trackLogicalModelCommand sourceName backendTypeMetadata conflictingLogicalModel)
          )
          [yaml|
              code: unexpected
              error: *expectedError 
              path: $.args
          |]

    it "when the logical model has the same name as an already tracked logical model" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "select * from stuff"
            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv

            source = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnv

            conflictingLogicalModel :: Schema.LogicalModel
            conflictingLogicalModel =
              (Schema.logicalModel (Schema.unSchemaName schemaName <> "_stuff_exist") spicyQuery)
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelColumn "thing" Schema.TInt,
                      Schema.logicalModelColumn "date" Schema.TUTCTime
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" Schema.TInt,
                      Schema.logicalModelColumn "target_date" Schema.TUTCTime
                    ]
                }

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadata
              testEnv
              (Schema.trackLogicalModelCommand source backendTypeMetadata conflictingLogicalModel)
          )
          [yaml|
              message: success
          |]

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              (Schema.trackLogicalModelCommand source backendTypeMetadata conflictingLogicalModel)
          )
          [yaml|
              code: already-tracked
              error: Logical model 'hasura_stuff_exist' is already tracked.
              path: $.args
          |]

    it "where arguments do not typecheck" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            query = "SELECT 10 / {{denominator}} AS divided"

            brokenTypesLogicalModel :: Schema.LogicalModel
            brokenTypesLogicalModel =
              (Schema.logicalModel "divided_falling" query)
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelColumn "divided" Schema.TInt
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" Schema.TStr
                    ]
                }

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                |]

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackLogicalModelCommand sourceName backendTypeMetadata brokenTypesLogicalModel)

        actual `shouldAtLeastBe` expected

    it "where the column names specified are not returned from the query" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                  internal:
                    error:
                      message: column "text" does not exist
                |]

            query = "SELECT {{text}} AS not_text"

            brokenColumnsLogicalModel :: Schema.LogicalModel
            brokenColumnsLogicalModel =
              (Schema.logicalModel "text_failing" query)
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelColumn "text" Schema.TStr
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "text" Schema.TStr
                    ]
                }

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackLogicalModelCommand sourceName backendTypeMetadata brokenColumnsLogicalModel)

        actual `shouldAtLeastBe` expected

    it "that uses undeclared arguments" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            query = "SELECT 10 / {{denominator}} AS divided"

            missingArgsLogicalModel :: Schema.LogicalModel
            missingArgsLogicalModel =
              (Schema.logicalModel "divided_falling" query)
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelColumn "divided" Schema.TInt
                    ],
                  Schema.logicalModelArguments =
                    []
                }

        shouldReturnYaml
          opts
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnvironment
              (Schema.trackLogicalModelCommand sourceName backendTypeMetadata missingArgsLogicalModel)
          )
          [yaml|
             code: validation-failed
             error: 'Undeclared arguments: "denominator"'
             path: $.args
          |]

  describe "Validation succeeds" do
    it "when tracking then untracking then re-tracking a logical model" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            dividedStuffLogicalModel :: Schema.LogicalModel
            dividedStuffLogicalModel =
              (Schema.logicalModel "divided_stuff2" simpleQuery)
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelColumn "divided" Schema.TInt
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" Schema.TInt,
                      Schema.logicalModelColumn "target_date" Schema.TUTCTime
                    ]
                }

        Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

        Schema.untrackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

        Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment
