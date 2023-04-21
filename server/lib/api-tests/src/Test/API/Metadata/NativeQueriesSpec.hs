{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Tests of the Native Queries feature.
module Test.API.Metadata.NativeQueriesSpec (spec) where

import Control.Lens
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
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
import Harness.Services.Source.Postgres
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig, scalarTypeToText)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

-- We currently don't need the table to exist in order to set up a native query
-- stanza.

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

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

  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] do
    traverse_
      (Fixture.runClean fixtures)
      [ testAdminAccess,
        testRelationships,
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
      },
    (Schema.table "article")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "author_id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr
          ]
      }
  ]

dividedStuffLogicalModel :: Schema.LogicalModel
dividedStuffLogicalModel =
  (Schema.logicalModel "divided_stuff")
    { Schema.logicalModelColumns =
        [ (Schema.logicalModelScalar "divided" Schema.TInt)
            { Schema.logicalModelColumnDescription = Just "a divided thing"
            }
        ]
    }

testAdminAccess :: SpecWith TestEnvironment
testAdminAccess = do
  let query :: Text
      query = "SELECT (thing / {{denominator}})::integer AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Admin access" do
    let dividedStuffNativeQuery :: Schema.NativeQuery
        dividedStuffNativeQuery =
          (Schema.nativeQuery "divided_stuff" query "divided_stuff")
            { Schema.nativeQueryArguments =
                [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                  Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                ]
            }

    it "Fails to track a Native Query without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnvironment
              [ ("X-Hasura-Role", "not-admin")
              ]
              (Schema.trackNativeQueryCommand sourceName backendTypeMetadata dividedStuffNativeQuery)
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

    it "Fails to untrack a Native Query without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadataWithStatusAndHeaders
              400
              testEnvironment
              [ ("X-Hasura-Role", "not-admin")
              ]
              (Schema.untrackNativeQueryCommand sourceName backendTypeMetadata dividedStuffNativeQuery)
          )
          [yaml|
            code: access-denied
            error: "restricted access : admin only"
            path: "$.args"
          |]

    it "Fails to list a Native Query without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata
            backendType = BackendType.backendTypeString backendTypeMetadata
            getRequestType = backendType <> "_get_native_query"

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
-- Test relationships --
-------------------------

testRelationships :: SpecWith TestEnvironment
testRelationships = do
  let query :: Text
      query = "SELECT * FROM (VALUES (1, 'Marenghi'), (2, 'other')) as t(\"id\", \"name\")"

      articleLogicalModel :: Schema.LogicalModel
      articleLogicalModel =
        (Schema.logicalModel "article")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "id" Schema.TInt,
                Schema.logicalModelScalar "author_id" Schema.TInt,
                Schema.logicalModelScalar "title" Schema.TStr,
                Schema.logicalModelScalar "content" Schema.TStr
              ]
          }

      -- we'll need to add the `articles` relationship row later
      relationshipLogicalModel :: Schema.LogicalModel
      relationshipLogicalModel =
        (Schema.logicalModel "author")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "id" Schema.TInt,
                Schema.logicalModelScalar "name" Schema.TStr,
                Schema.logicalModelReference "articles" "article"
              ]
          }

      -- broadly, a 'SELECT * FROM authors' type query
      relationshipNativeQuery :: Schema.NativeQuery
      relationshipNativeQuery =
        Schema.nativeQuery "relationship_test" query "author"

  describe "Relationships" $ do
    it "Adding a native query with a valid array relationship returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

          schemaKeyword :: String
          schemaKeyword = Key.toString $ Fixture.backendSchemaKeyword backendTypeMetadata

          arrayRel =
            [interpolateYaml|
                name: articles
                using:
                  column_mapping:
                    id: author_id
                  insertion_order: null
                  remote_table:
                    name: article
                    #{schemaKeyword}: #{schemaName}
            |]

          nativeQueryWithRelationship =
            relationshipNativeQuery
              { Schema.nativeQueryArrayRelationships = [arrayRel]
              }

      Schema.trackLogicalModel sourceName articleLogicalModel testEnvironment
      Schema.trackLogicalModel sourceName relationshipLogicalModel testEnvironment
      let nativeQueryMetadata = Schema.trackNativeQueryCommand sourceName backendTypeMetadata nativeQueryWithRelationship

      GraphqlEngine.postMetadata_
        testEnvironment
        nativeQueryMetadata

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
    it "Adds a simple native query of a function with no arguments and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffNativeQuery :: Schema.NativeQuery
          dividedStuffNativeQuery =
            (Schema.nativeQuery "divided_stuff" simpleQuery "divided_stuff")
              { Schema.nativeQueryArguments =
                  [Schema.nativeQueryColumn "unused" Schema.TInt]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

    it "Adding a native query of a function with broken SQL returns a 400" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          brokenQuery = "SELECT * FROM dogs WHERE name = {{name"

          brokenQueryNativeQuery :: Schema.NativeQuery
          brokenQueryNativeQuery =
            (Schema.nativeQuery "divided_stuff" brokenQuery "divided_stuff")
              { Schema.nativeQueryArguments =
                  [Schema.nativeQueryColumn "unused" Schema.TInt]
              }

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            (Schema.trackNativeQueryCommand sourceName backendTypeMetadata brokenQueryNativeQuery)
        )
        [yaml|
          code: parse-failed
          error: "Found '{{' without a matching closing '}}'"
          path: "$.args"
        |]

    it "Checks for the native query of a function" $ \testEnvironment -> do
      let rootfield :: Text
          rootfield = "divided_stuff2"

          backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_native_query"

          dividedStuffNativeQuery :: Schema.NativeQuery
          dividedStuffNativeQuery =
            (Schema.nativeQuery rootfield query "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                    Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

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

    it "Drops a native query of a function and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffNativeQuery :: Schema.NativeQuery
          dividedStuffNativeQuery =
            (Schema.nativeQuery "divided_stuff" query "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                    Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment
      Schema.untrackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

    it "Checks the native query of a function can be deleted" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_native_query"

          dividedStuffNativeQuery :: Schema.NativeQuery
          dividedStuffNativeQuery =
            (Schema.nativeQuery "divided_stuff" query "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                    Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

      Schema.untrackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

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

    it "Fails to add a native query with a non-existent return type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffNativeQuery :: Schema.NativeQuery
          dividedStuffNativeQuery =
            (Schema.nativeQuery "divided_stuff" query "bad_return_type")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                    Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                  ]
              }

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus 400 testEnvironment $
            Schema.trackNativeQueryCommand sourceName backendTypeMetadata dividedStuffNativeQuery
        )
        [yaml|
          code: not-found
          error: Logical model "bad_return_type" not found.
          path: $.args
        |]

    it "Adds two native queries with the same return type" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          nativeQueryOne :: Schema.NativeQuery
          nativeQueryOne =
            (Schema.nativeQuery "first" query "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                    Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                  ]
              }

          nativeQueryTwo :: Schema.NativeQuery
          nativeQueryTwo =
            (Schema.nativeQuery "second" query "divided_stuff")
              { Schema.nativeQueryArguments =
                  [ Schema.nativeQueryColumn "denominator" Schema.TInt,
                    Schema.nativeQueryColumn "target_date" Schema.TUTCTime
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata testEnvironment $
            Schema.trackNativeQueryCommand sourceName backendTypeMetadata nativeQueryOne
        )
        [yaml| message: success |]

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadata testEnvironment $
            Schema.trackNativeQueryCommand sourceName backendTypeMetadata nativeQueryTwo
        )
        [yaml| message: success |]

    it "Causes a metadata inconsistency when the return type is deleted" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffNativeQuery :: Schema.NativeQuery
          dividedStuffNativeQuery =
            (Schema.nativeQuery "divided_stuff" simpleQuery "divided_stuff")
              { Schema.nativeQueryArguments =
                  [Schema.nativeQueryColumn "unused" Schema.TInt]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment
      Schema.trackNativeQuery sourceName dividedStuffNativeQuery testEnvironment

      metadata <-
        GraphqlEngine.postMetadata
          testEnvironment
          [yaml|
            type: export_metadata
            args: {}
          |]

      let inconsistent :: A.Value
          inconsistent =
            metadata
              & key "sources" . values . key "logical_models"
                .~ A.Array mempty

          integer :: Text
          integer = Fixture.backendScalarType backendTypeMetadata Schema.TInt

      shouldReturnYaml
        testEnvironment
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [yaml|
              type: replace_metadata
              args: *inconsistent
            |]
        )
        [interpolateYaml|
          code: unexpected
          error: cannot continue due to inconsistent metadata
          internal:
            - definition:
                arguments:
                  unused:
                    nullable: false
                    type: #{integer}
                code: SELECT (thing / 2)::integer AS divided FROM stuff
                returns: divided_stuff
                root_field_name: divided_stuff
              name: native_query divided_stuff in source #{sourceName}
              reason: "Inconsistent object: The logical model divided_stuff could not be found"
              type: native_query
          path: $.args
        |]

metadataHandlingWhenDisabledSpec :: SpecWith GlobalTestEnvironment
metadataHandlingWhenDisabledSpec = do
  describe "When native queries are enabled" do
    withHge
      ( emptyHgeConfig
          { hgeConfigEnvironmentVars =
              [ (featureFlagForNativeQueries, "True")
              ]
          }
      )
      $ withPostgresSource "default"
      $ do
        it "`replace_metadata` does not report any inconsistent objects" $ \env -> do
          let command = Schema.trackLogicalModelCommand "default" Postgres.backendTypeMetadata dividedStuffLogicalModel
          _ <- hgePost env 200 "/v1/metadata" [] command

          currentMetadata <- export_metadata env
          actual <- replace_metadata env (metadataWithNativeQuery currentMetadata)

          actual
            `shouldBeYaml` [yaml|
              inconsistent_objects: []
              is_consistent: true
              |]

        it "They do appear in the schema" $ \env -> do
          let command = Schema.trackLogicalModelCommand "default" Postgres.backendTypeMetadata dividedStuffLogicalModel
          _ <- hgePost env 200 "/v1/metadata" [] command

          currentMetadata <- export_metadata env
          _res <- replace_metadata env (metadataWithNativeQuery currentMetadata)

          let expected =
                [yaml|
                data:
                  __type:
                    name: divided_stuff
                |]

          actual <- hgePostGraphql env queryTypesIntrospection
          actual `shouldBeYaml` expected

  describe "When native queries are disabled" do
    withHge emptyHgeConfig $ do
      withPostgresSource "default" $ do
        it "They do not appear in the schema" $ \env -> do
          currentMetadata <- export_metadata env
          _res <- replace_metadata env (metadataWithNativeQuery currentMetadata)

          let expected =
                [yaml|
                data:
                  __type: null
                |]

          actual <- hgePostGraphql env queryTypesIntrospection
          actual `shouldBeYaml` expected
  where
    nativeQueriesMetadata =
      [yaml|
          arguments:
            divided:
              nullable: false
              type: int
          code: SELECT {{divided}} as divided
          returns: "divided_stuff"
          root_field_name: divided_stuff
      |]

    metadataWithNativeQuery :: A.Value -> A.Value
    metadataWithNativeQuery currentMetadata =
      currentMetadata
        & key "sources"
          . nth 0
          . atKey "native_queries"
          .~ Just [yaml| - *nativeQueriesMetadata |]

    queryTypesIntrospection :: A.Value
    queryTypesIntrospection =
      [graphql|
          query {
            __type(name: "divided_stuff") {
              name
            }
          }
        |]
