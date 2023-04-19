module Test.API.Metadata.NativeQueries.ValidationSpec where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] do
    Fixture.runClean
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
              },
            (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Cockroach.setupTablesAction schema testEnv
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

tests :: SpecWith TestEnvironment
tests = do
  let simpleQuery :: Text
      simpleQuery = "SELECT (thing / 2)::integer AS divided FROM stuff"

      conflictingLogicalModel :: Schema.LogicalModel
      conflictingLogicalModel =
        (Schema.logicalModel "conflicting")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "thing" Schema.TInt,
                Schema.logicalModelScalar "date" Schema.TUTCTime
              ]
          }

      dividedLogicalModel :: Schema.LogicalModel
      dividedLogicalModel =
        (Schema.logicalModel "divided_stuff")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "divided" Schema.TInt
              ]
          }

  describe "Validation fails on untrack a native query" do
    it "when a native query does not exist" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            nonExistentNativeQuery :: Schema.NativeQuery
            nonExistentNativeQuery = Schema.nativeQuery "some_native_query" "" ""

            expectedError = "Native query \"some_native_query\" not found in source \"" <> sourceName <> "\"."

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnvironment
              (Schema.untrackNativeQueryCommand sourceName backendTypeMetadata nonExistentNativeQuery)
          )
          [yaml|
          code: not-found
          error: *expectedError
          path: "$.args"
        |]

  describe "Validation fails on track a native query" do
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

            syntaxErrorNativeQuery :: Schema.NativeQuery
            syntaxErrorNativeQuery =
              (Schema.nativeQuery "divided_stuff" spicyQuery "divided_stuff")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                    ]
                }

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackNativeQueryCommand sourceName backendTypeMetadata syntaxErrorNativeQuery)

        actual `shouldAtLeastBe` expected

    it "when the query refers to non existing table" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        let spicyQuery :: Text
            spicyQuery = "SELECT thing / {{denominator}} AS divided FROM does_not_exist WHERE date = {{target_date}}"

            brokenNativeQuery :: Schema.NativeQuery
            brokenNativeQuery =
              (Schema.nativeQuery "divided_stuff" spicyQuery "divided_stuff")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
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

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackNativeQueryCommand sourceName backendTypeMetadata brokenNativeQuery)

        actual `shouldAtLeastBe` expected

    it "when the native query has the same name as an already tracked table" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "select * from stuff"

            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
            sourceName = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnv

            conflictingNativeQuery :: Schema.NativeQuery
            conflictingNativeQuery =
              (Schema.nativeQuery (Schema.unSchemaName schemaName <> "_stuff") spicyQuery "conflicting")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                    ]
                }

            expectedError = "Encountered conflicting definitions in the selection set for 'subscription_root' for field 'hasura_stuff' defined in [table hasura.stuff in source " <> sourceName <> ", native_query hasura_stuff in source " <> sourceName <> "]. Fields must not be defined more than once across all sources."

        Schema.trackLogicalModel sourceName conflictingLogicalModel testEnv

        shouldReturnYaml
          testEnv
          ( GraphqlEngine.postMetadataWithStatus
              500
              testEnv
              (Schema.trackNativeQueryCommand sourceName backendTypeMetadata conflictingNativeQuery)
          )
          [yaml|
              code: unexpected
              error: *expectedError
              path: $.args
          |]

    it "when the native query has the same name as an already tracked native query" $
      \testEnv -> do
        let spicyQuery :: Text
            spicyQuery = "select * from stuff"
            backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv

            source = BackendType.backendSourceName backendTypeMetadata
            schemaName = Schema.getSchemaName testEnv

            conflictingNativeQuery :: Schema.NativeQuery
            conflictingNativeQuery =
              (Schema.nativeQuery (Schema.unSchemaName schemaName <> "_stuff_exist") spicyQuery "conflicting")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                    ]
                }

        Schema.trackLogicalModel source conflictingLogicalModel testEnv

        shouldReturnYaml
          testEnv
          ( GraphqlEngine.postMetadata
              testEnv
              (Schema.trackNativeQueryCommand source backendTypeMetadata conflictingNativeQuery)
          )
          [yaml|
              message: success
          |]

        shouldReturnYaml
          testEnv
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnv
              (Schema.trackNativeQueryCommand source backendTypeMetadata conflictingNativeQuery)
          )
          [yaml|
              code: already-tracked
              error: Native query 'hasura_stuff_exist' is already tracked.
              path: $.args
          |]

    it "where arguments do not typecheck" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            query = "SELECT 10 / {{denominator}} AS divided"

            brokenTypesNativeQuery :: Schema.NativeQuery
            brokenTypesNativeQuery =
              (Schema.nativeQuery "divided_falling" query "divided_stuff")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TStr
                    ]
                }

        let expected =
              [yaml|
                  code: validation-failed
                  error: Failed to validate query
                |]

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackNativeQueryCommand sourceName backendTypeMetadata brokenTypesNativeQuery)

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

            brokenColumnsReturn :: Schema.LogicalModel
            brokenColumnsReturn =
              (Schema.logicalModel "failing")
                { Schema.logicalModelColumns =
                    [ Schema.logicalModelScalar "text" Schema.TStr
                    ]
                }

            brokenColumnsNativeQuery :: Schema.NativeQuery
            brokenColumnsNativeQuery =
              (Schema.nativeQuery "text_failing" query "failing")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "text" Schema.TStr
                    ]
                }

        Schema.trackLogicalModel sourceName brokenColumnsReturn testEnvironment

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackNativeQueryCommand sourceName backendTypeMetadata brokenColumnsNativeQuery)

        actual `shouldAtLeastBe` expected

    it "that uses undeclared arguments" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            query = "SELECT 10 / {{denominator}} AS divided"

            missingArgsNativeQuery :: Schema.NativeQuery
            missingArgsNativeQuery =
              (Schema.nativeQuery "divided_falling" query "divided_stuff")

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatus
              400
              testEnvironment
              (Schema.trackNativeQueryCommand sourceName backendTypeMetadata missingArgsNativeQuery)
          )
          [yaml|
             code: validation-failed
             error: 'Undeclared arguments: "denominator"'
             path: $.args
          |]

    it "when the native query name has a non-standard character" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            dividedStuffNativeQuery :: Schema.NativeQuery
            dividedStuffNativeQuery =
              (Schema.nativeQuery "Divided-Stuff" simpleQuery "divided_stuff")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                    ]
                }

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        actual <-
          GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackNativeQueryCommand sourceName backendTypeMetadata dividedStuffNativeQuery)
        let expected =
              [yaml|
                 code: parse-failed
                 path: $.args.root_field_name
              |]
        actual `shouldAtLeastBe` expected

  describe "Validation succeeds" do
    it "when tracking then untracking then re-tracking a native query" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            dividedStuffNativeQuery :: Schema.NativeQuery
            dividedStuffNativeQuery =
              (Schema.nativeQuery "divided_stuff2" simpleQuery "divided_stuff")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                    ]
                }

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

        Schema.untrackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

        Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

    it "when the native query name has an uppercase letter" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

            dividedStuffNativeQuery :: Schema.NativeQuery
            dividedStuffNativeQuery =
              (Schema.nativeQuery "DividedStuff" simpleQuery "divided_stuff")
                { Schema.nativeQueryArguments =
                    [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                      Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                    ]
                }

        Schema.trackLogicalModel sourceName dividedLogicalModel testEnvironment

        Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment
