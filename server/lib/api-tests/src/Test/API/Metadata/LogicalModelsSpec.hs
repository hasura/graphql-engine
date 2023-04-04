{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Logical Models feature.
module Test.API.Metadata.LogicalModelsSpec (spec) where

import Control.Lens
import Data.Aeson qualified as A
import Data.Aeson.Lens
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml
import Harness.Schema qualified as Schema
import Harness.Services.GraphqlEngine
import Harness.Services.Metadata
import Harness.Services.PostgresSource
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig, scalarTypeToText)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

-- We currently don't need the table to exist in order to set up a logical model
-- stanza.

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec = do
  let fixtures =
        NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Postgres.setupTablesAction schema testEnv
                  ]
              },
            (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ BigQuery.setupTablesAction schema testEnv
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
              },
            (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Sqlserver.setupTablesAction schema testEnv
                  ]
              }
          ]

  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] do
    traverse_
      (Fixture.runClean fixtures)
      [ testAdminAccess,
        testImplementation
      ]

  metadataHandlingWhenDisabledSpec

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

dividedStuffReturnType :: Schema.CustomType
dividedStuffReturnType =
  (Schema.customType "divided_stuff")
    { Schema.customTypeColumns =
        [ (Schema.logicalModelColumn "divided" Schema.TInt)
            { Schema.logicalModelColumnDescription = Just "a divided thing"
            }
        ]
    }

testAdminAccess :: SpecWith TestEnvironment
testAdminAccess = do
  let query :: Text
      query = "SELECT (thing / {{denominator}})::integer AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Admin access" do
    let dividedStuffLogicalModel :: Schema.LogicalModel
        dividedStuffLogicalModel =
          (Schema.logicalModel "divided_stuff" query "divided_stuff")
            { Schema.logicalModelArguments =
                [ Schema.logicalModelColumn "denominator" Schema.TInt,
                  Schema.logicalModelColumn "target_date" Schema.TUTCTime
                ]
            }

    it "Fails to track a Logical Model without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnvironment
              [ ("X-Hasura-Role", "not-admin")
              ]
              (Schema.trackLogicalModelCommand sourceName backendTypeMetadata dividedStuffLogicalModel)
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

    it "Fails to untrack a Logical Model without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnvironment
              [ ("X-Hasura-Role", "not-admin")
              ]
              (Schema.untrackLogicalModelCommand sourceName backendTypeMetadata dividedStuffLogicalModel)
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

    it "Fails to list a Logical Model without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata
            backendType = BackendType.backendTypeString backendTypeMetadata
            getRequestType = backendType <> "_get_logical_model"

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnvironment
              [ ("X-Hasura-Role", "not-admin")
              ]
              [yaml|
                type: *getRequestType
                args:
                  source: *sourceName
              |]
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

-------------------------
-- Test implementation --
-------------------------

testImplementation :: SpecWith TestEnvironment
testImplementation = do
  let simpleQuery :: Text
      simpleQuery = "SELECT (thing / 2)::integer AS divided FROM stuff"

  let query :: Text
      query = "SELECT (thing / {{denominator}})::integer AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Implementation" $ do
    it "Adds a simple logical model of a function with no arguments and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" simpleQuery "divided_stuff")
              { Schema.logicalModelArguments =
                  [Schema.logicalModelColumn "unused" Schema.TInt]
              }

      Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment
      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

    it "Adding a logical model of a function with broken SQL returns a 400" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          brokenQuery = "SELECT * FROM dogs WHERE name = {{name"

          brokenQueryLogicalModel :: Schema.LogicalModel
          brokenQueryLogicalModel =
            (Schema.logicalModel "divided_stuff" brokenQuery "divided_stuff")
              { Schema.logicalModelArguments =
                  [Schema.logicalModelColumn "unused" Schema.TInt]
              }

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackLogicalModelCommand sourceName backendTypeMetadata brokenQueryLogicalModel)
        )
        [yaml|
          code: parse-failed
          error: "Found '{{' without a matching closing '}}'"
          path: "$.args"
        |]

    it "Checks for the logical model of a function" $ \testEnvironment -> do
      let rootfield :: Text
          rootfield = "divided_stuff2"

          backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_logical_model"

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel rootfield query "divided_stuff")
              { Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" Schema.TInt,
                    Schema.logicalModelColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment
      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType
              args:
                source: *sourceName
            |]
        )
        [interpolateYaml|
          - root_field_name: #{rootfield}
            code: #{query}
            arguments:
              denominator:
                type: #{scalarTypeToText testEnvironment Schema.TInt}
                nullable: false
              target_date:
                type: #{scalarTypeToText testEnvironment Schema.TUTCTime}
                nullable: false
            returns: divided_stuff
        |]

    it "Drops a logical model of a function and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" query "divided_stuff")
              { Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" Schema.TInt,
                    Schema.logicalModelColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment
      Schema.untrackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

    it "Checks the logical model of a function can be deleted" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_logical_model"

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" query "divided_stuff")
              { Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" Schema.TInt,
                    Schema.logicalModelColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment
      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      Schema.untrackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType
              args:
                source: *sourceName
            |]
        )
        [yaml|
          []
        |]

    it "Fails to add a logical model with a non-existent return type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" query "bad_return_type")
              { Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" Schema.TInt,
                    Schema.logicalModelColumn "target_date" Schema.TUTCTime
                  ]
              }

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus 400 testEnvironment $
            Schema.trackLogicalModelCommand sourceName backendTypeMetadata dividedStuffLogicalModel
        )
        [yaml|
          code: not-found
          error: Custom return type "bad_return_type" not found.
          path: $.args
        |]

    it "Adds two logical models with the same return type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          logicalModelOne :: Schema.LogicalModel
          logicalModelOne =
            (Schema.logicalModel "first" query "divided_stuff")
              { Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" Schema.TInt,
                    Schema.logicalModelColumn "target_date" Schema.TUTCTime
                  ]
              }

          logicalModelTwo :: Schema.LogicalModel
          logicalModelTwo =
            (Schema.logicalModel "second" query "divided_stuff")
              { Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" Schema.TInt,
                    Schema.logicalModelColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackCustomType sourceName dividedStuffReturnType testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata testEnvironment $
            Schema.trackLogicalModelCommand sourceName backendTypeMetadata logicalModelOne
        )
        [yaml| message: success |]

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata testEnvironment $
            Schema.trackLogicalModelCommand sourceName backendTypeMetadata logicalModelTwo
        )
        [yaml| message: success |]

metadataHandlingWhenDisabledSpec :: SpecWith GlobalTestEnvironment
metadataHandlingWhenDisabledSpec = do
  describe "When logical models are enabled" do
    withHge
      ( emptyHgeConfig
          { hgeConfigEnvironmentVars =
              [ (featureFlagForLogicalModels, "True")
              ]
          }
      )
      $ withPostgresSource "default"
      $ do
        it "`replace_metadata` does not report any inconsistent objects" $ \env -> do
          let command = Schema.trackCustomTypeCommand "default" Postgres.backendTypeMetadata dividedStuffReturnType
          _ <- hgePost env 200 "/v1/metadata" [] command

          currentMetadata <- export_metadata env
          actual <- replace_metadata env (metadataWithLogicalModel currentMetadata)

          actual
            `shouldBeYaml` [yaml|
              inconsistent_objects: []
              is_consistent: true
              |]

        it "They do appear in the schema" $ \env -> do
          let command = Schema.trackCustomTypeCommand "default" Postgres.backendTypeMetadata dividedStuffReturnType
          _ <- hgePost env 200 "/v1/metadata" [] command

          currentMetadata <- export_metadata env
          _res <- replace_metadata env (metadataWithLogicalModel currentMetadata)

          let expected =
                [yaml|
                data:
                  __type:
                    name: divided_stuff
                |]

          actual <- hgePostGraphql env queryTypesIntrospection
          actual `shouldBeYaml` expected

  describe "When logical models are disabled" do
    withHge emptyHgeConfig $ do
      withPostgresSource "default" $ do
        it "They do not appear in the schema" $ \env -> do
          currentMetadata <- export_metadata env
          _res <- replace_metadata env (metadataWithLogicalModel currentMetadata)

          let expected =
                [yaml|
                data:
                  __type: null
                |]

          actual <- hgePostGraphql env queryTypesIntrospection
          actual `shouldBeYaml` expected
  where
    logicalModelsMetadata =
      [yaml|
          arguments:
            divided:
              nullable: false
              type: int
          code: SELECT {{divided}} as divided
          returns: "divided_stuff"
          root_field_name: divided_stuff
      |]

    metadataWithLogicalModel :: A.Value -> A.Value
    metadataWithLogicalModel currentMetadata =
      currentMetadata
        & key "sources"
          . nth 0
          . atKey "logical_models"
          .~ Just [yaml| - *logicalModelsMetadata |]

    queryTypesIntrospection :: A.Value
    queryTypesIntrospection =
      [graphql|
          query {
            __type(name: "divided_stuff") {
              name
            }
          }
        |]
