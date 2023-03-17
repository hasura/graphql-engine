{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Logical Models feature.
module Test.API.Metadata.LogicalModelsSpec (spec) where

import Data.Aeson qualified as A
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldBeYaml, shouldReturnYaml)
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
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Citus.setupTablesAction schema testEnv
                  ]
              }
          ]

  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] do
    -- do not need to run isolated
    traverse_
      (Fixture.run fixtures)
      [testAdminAccess, testValidation, testPermissionFailures]
    -- need to run isolated
    traverse_
      (Fixture.runClean fixtures)
      [testImplementation, testPermissions]

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

testAdminAccess :: Fixture.Options -> SpecWith TestEnvironment
testAdminAccess opts = do
  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Admin access" do
    let dividedStuffLogicalModel :: Schema.LogicalModel
        dividedStuffLogicalModel =
          (Schema.logicalModel "divided_stuff" query)
            { Schema.logicalModelColumns =
                [ (Schema.logicalModelColumn "divided" "integer")
                    { Schema.logicalModelColumnDescription = Just "a divided thing"
                    }
                ],
              Schema.logicalModelArguments =
                [ Schema.logicalModelColumn "denominator" "integer",
                  Schema.logicalModelColumn "target_date" "date"
                ]
            }

    it "Fails to track a Logical Model without admin access" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        shouldReturnYaml
          opts
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

        shouldReturnYaml
          opts
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
          opts
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

testImplementation :: Fixture.Options -> SpecWith TestEnvironment
testImplementation opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  let query :: Text
      query = "SELECT thing / {{denominator}} AS divided FROM stuff WHERE date = {{target_date}}"

  describe "Implementation" $ do
    it "Adds a simple logical model of a function with no arguments and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" simpleQuery)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" "integer")
                      { Schema.logicalModelColumnDescription = Just "a divided thing"
                      }
                  ],
                Schema.logicalModelArguments =
                  [Schema.logicalModelColumn "unused" "integer"]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

    it "Adding a logical model of a function with broken SQL returns a 400" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          brokenQuery = "SELECT * FROM dogs WHERE name = {{name"

          brokenQueryLogicalModel :: Schema.LogicalModel
          brokenQueryLogicalModel =
            (Schema.logicalModel "divided_stuff" brokenQuery)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" "integer")
                      { Schema.logicalModelColumnDescription = Just "a divided thing"
                      }
                  ],
                Schema.logicalModelArguments =
                  [Schema.logicalModelColumn "unused" "integer"]
              }

      shouldReturnYaml
        opts
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
            (Schema.logicalModel rootfield query)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" "integer")
                      { Schema.logicalModelColumnDescription = Just "a divided thing"
                      }
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" "integer",
                    Schema.logicalModelColumn "target_date" "date"
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType 
              args:
                source: *sourceName
            |]
        )
        [yaml|
          - root_field_name: *rootfield
            code: *query
            arguments:
              denominator:
                type: integer
                nullable: false
              target_date:
                type: date
                nullable: false
            returns:
              columns:
                    divided:
                      type: integer
                      nullable: false
                      description: "a divided thing"
        |]

    it "Drops a logical model of a function and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" query)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" "integer")
                      { Schema.logicalModelColumnDescription = Just "a divided thing"
                      }
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" "integer",
                    Schema.logicalModelColumn "target_date" "date"
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      Schema.untrackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

    it "Checks the logical model of a function can be deleted" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          getRequestType = backendType <> "_get_logical_model"

          dividedStuffLogicalModel :: Schema.LogicalModel
          dividedStuffLogicalModel =
            (Schema.logicalModel "divided_stuff" query)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" "integer")
                      { Schema.logicalModelColumnDescription = Just "a divided thing"
                      }
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "denominator" "integer",
                    Schema.logicalModelColumn "target_date" "date"
                  ]
              }

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      Schema.untrackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        opts
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

    it "Descriptions and nullability appear in the schema" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

          nullableQuery = "SELECT thing / 2 AS divided, null as something_nullable FROM stuff"

          descriptionsAndNullableLogicalModel :: Schema.LogicalModel
          descriptionsAndNullableLogicalModel =
            (Schema.logicalModel "divided_stuff" nullableQuery)
              { Schema.logicalModelColumns =
                  [ (Schema.logicalModelColumn "divided" "integer")
                      { Schema.logicalModelColumnDescription = Just "A divided thing"
                      },
                    (Schema.logicalModelColumn "something_nullable" "integer")
                      { Schema.logicalModelColumnDescription = Just "Something nullable",
                        Schema.logicalModelColumnNullable = True
                      }
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "unused" "integer"
                  ],
                Schema.logicalModelReturnTypeDescription = Just "Return type description"
              }

      Schema.trackLogicalModel sourceName descriptionsAndNullableLogicalModel testEnvironment

      let queryTypesIntrospection :: A.Value
          queryTypesIntrospection =
            [graphql|
                query {
                  __type(name: "divided_stuff") {
                    name
                    description
                    fields {
                      name
                      description
                      type {
                        name
                        kind
                        ofType {
                          name
                        }
                      }
                    }
                  }
                }
              |]

          expected =
            [interpolateYaml|
                {
                  "data": {
                    "__type": {
                      "description": "Return type description",
                      "fields": [
                      {
                        "description": "A divided thing",
                        "name": "divided",
                        "type": {
                          "kind": "NON_NULL",
                          "name": null,
                          "ofType": {
                            "name": "Int"
                          }
                        }
                      },
                      {
                        "description": "Something nullable",
                        "name": "something_nullable",
                        "type": {
                          "kind": "SCALAR",
                          "name": "Int",
                          "ofType": null
                        }
                      }
                      ],
                      "name": "divided_stuff"
                    }
                  }
                }
              |]

      actual <- GraphqlEngine.postGraphql testEnvironment queryTypesIntrospection

      actual `shouldBeYaml` expected

---------------------
-- Test validation --
---------------------

testValidation :: Fixture.Options -> SpecWith TestEnvironment
testValidation opts = do
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
                    [ (Schema.logicalModelColumn "divided" "integer")
                        { Schema.logicalModelColumnDescription = Just "A divided thing"
                        }
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" "integer",
                      Schema.logicalModelColumn "target_date" "date"
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
                    [ (Schema.logicalModelColumn "divided" "integer")
                        { Schema.logicalModelColumnDescription = Just "A divided thing"
                        }
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" "integer",
                      Schema.logicalModelColumn "target_date" "date"
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
                    [ Schema.logicalModelColumn "thing" "integer",
                      Schema.logicalModelColumn "date" "date"
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" "integer",
                      Schema.logicalModelColumn "target_date" "date"
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
                    [ Schema.logicalModelColumn "thing" "integer",
                      Schema.logicalModelColumn "date" "date"
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" "integer",
                      Schema.logicalModelColumn "target_date" "date"
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
                    [ Schema.logicalModelColumn "divided" "integer"
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" "varchar"
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
                    [ Schema.logicalModelColumn "text" "text"
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "text" "text"
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
                    [ Schema.logicalModelColumn "divided" "integer"
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
                    [ Schema.logicalModelColumn "divided" "integer"
                    ],
                  Schema.logicalModelArguments =
                    [ Schema.logicalModelColumn "denominator" "integer",
                      Schema.logicalModelColumn "target_date" "date"
                    ]
                }

        Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

        Schema.untrackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

        Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

----------------------
-- Test permissions --
----------------------

testPermissions :: Fixture.Options -> SpecWith TestEnvironment
testPermissions opts = do
  let simpleQuery :: Text
      simpleQuery = "SELECT thing / 2 AS divided FROM stuff"

  describe "Permissions" do
    let dividedStuffLogicalModel :: Schema.LogicalModel
        dividedStuffLogicalModel =
          (Schema.logicalModel "divided_stuff" simpleQuery)
            { Schema.logicalModelColumns =
                [ (Schema.logicalModelColumn "divided" "integer")
                    { Schema.logicalModelColumnDescription = Just "a divided thing"
                    }
                ],
              Schema.logicalModelArguments =
                [ Schema.logicalModelColumn "unused" "integer"
                ]
            }

    it "Adds a simple logical model function with no arguments a select permission and returns a 200" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"
          getRequestType = backendType <> "_get_logical_model"

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *sourceName
                    root_field_name: divided_stuff
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          - message: success
        |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType 
              args:
                source: *sourceName
            |]
        )
        [yaml|
          - root_field_name: divided_stuff
            code: *simpleQuery
            arguments:
              unused:
                type: integer
                nullable: false
            select_permissions:
              - role: "test"
                permission:
                  columns:
                    - divided
                  filter: {}
            returns:
              columns:
                divided:
                  description: a divided thing
                  nullable: false
                  type: integer
        |]

    it "Adds a logical model, removes it, and returns 200" $ \testEnvironment -> do
      let rootfield :: Text
          rootfield = Schema.logicalModelName dividedStuffLogicalModel

          backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"
          dropPermRequestType = backendType <> "_drop_logical_model_select_permission"
          getRequestType = backendType <> "_get_logical_model"

      Schema.trackLogicalModel sourceName dividedStuffLogicalModel testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType 
                  args:
                    source: *sourceName
                    root_field_name: *rootfield
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
                - type: *dropPermRequestType 
                  args:
                    source: *sourceName
                    root_field_name: *rootfield
                    role: "test"
            |]
        )
        [yaml|
          - message: success
          - message: success
        |]

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: *getRequestType 
              args:
                source: *sourceName
            |]
        )
        [yaml|
          - root_field_name: *rootfield
            code: *simpleQuery
            arguments:
              unused:
                type: integer
                nullable: false
            returns:
              columns:
                divided:
                  description: a divided thing
                  nullable: false
                  type: integer
        |]

testPermissionFailures :: Fixture.Options -> SpecWith TestEnvironment
testPermissionFailures opts = do
  describe "Permission failures" do
    it "Fails to adds a select permission to a nonexisting source" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType 
                  args:
                    source: made_up_source
                    root_field_name: made_up_logical_model
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          code: not-found
          error: "Source \"made_up_source\" not found."
          path: "$.args[0].args"
        |]

    it "Fails to adds a select permission to a nonexisting logical model" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          expectedError = "Logical model \"made_up_logical_model\" not found in source \"" <> sourceName <> "\"."

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType 
                  args:
                    source: *sourceName
                    root_field_name: made_up_logical_model
                    role: "test"
                    permission:
                      columns:
                        - divided
                      filter: {}
            |]
        )
        [yaml|
          code: "not-found"
          error: *expectedError
          path: "$.args[0].args"
        |]

    it "Fails to drop a select permission on a nonexisting source" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendType = BackendType.backendTypeString backendTypeMetadata
          dropPermRequestType = backendType <> "_drop_logical_model_select_permission"

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [yaml|
              type: *dropPermRequestType 
              args:
                source: made_up_source
                root_field_name: made_up_logical_model
                role: "test"
                permission:
                  columns:
                    - divided
                  filter: {}
            |]
        )
        [yaml|
          code: not-found
          error: "Source \"made_up_source\" not found."
          path: "$.args"
        |]

    it "Fails to drop a select permission from a nonexisting logical model" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          dropPermRequestType = backendType <> "_drop_logical_model_select_permission"
          expectedError = "Logical model \"made_up_logical_model\" not found in source \"" <> sourceName <> "\"."

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadataWithStatus
            400
            testEnvironment
            [yaml|
              type: *dropPermRequestType 
              args:
                source: *sourceName
                root_field_name: made_up_logical_model
                role: "test"
            |]
        )
        [yaml|
          code: "not-found"
          error: *expectedError 
          path: "$.args"
        |]
