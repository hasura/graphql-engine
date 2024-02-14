{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests that we can decode Arrays values correctly
module Test.Databases.Postgres.ArraySpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postGraphqlWithVariables)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ postgresFixture,
          citusFixture,
          cockroachFixture
        ]
    )
    singleArrayTests

  -- run these tests with Arrays switched off
  Fixture.hgeWithEnv [("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "disable_postgres_arrays")]
    $ Fixture.run
      ( NE.fromList
          [postgresFixture, citusFixture]
      )
    $ do
      singleArrayTests -- we check these still work with the flag on
      introspectionWithDisabledFeatureTests

  -- CockroachDB introspection looks different, let's not do it
  Fixture.run
    ( NE.fromList
        [postgresFixture, citusFixture]
    )
    introspectionTests

  -- CockroachDB does not support nested arrays
  -- https://www.cockroachlabs.com/docs/stable/array.html
  Fixture.run
    ( NE.fromList
        [postgresFixture, citusFixture]
    )
    nestedArrayTests

  -- CockroachDB does not support json arrays
  Fixture.run
    ( NE.fromList
        [postgresFixture, citusFixture]
    )
    jsonArrayTests

postgresFixture :: Fixture.Fixture ()
postgresFixture =
  (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
    { Fixture.setupTeardown = \(testEnvironment, _) ->
        [ Fixture.SetupAction
            { Fixture.setupAction =
                Postgres.run_ testEnvironment setup,
              Fixture.teardownAction = \_ ->
                pure ()
            },
          Postgres.setupTablesAction schema testEnvironment
        ]
    }

citusFixture :: Fixture.Fixture ()
citusFixture =
  (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
    { Fixture.setupTeardown = \(testEnvironment, _) ->
        [ Fixture.SetupAction
            { Fixture.setupAction =
                Citus.run_ testEnvironment setup,
              Fixture.teardownAction = \_ ->
                pure ()
            },
          Citus.setupTablesAction schema testEnvironment
        ]
    }

cockroachFixture :: Fixture.Fixture ()
cockroachFixture =
  (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
    { Fixture.setupTeardown = \(testEnvironment, _) ->
        [ Fixture.SetupAction
            { Fixture.setupAction =
                Cockroach.run_ testEnvironment setup,
              Fixture.teardownAction = \_ ->
                pure ()
            },
          Cockroach.setupTablesAction schema testEnvironment
        ]
    }

setup :: Text
setup = "create type \"_made_up_type\" as enum ('a', 'b', 'c');"

--------------------------------------------------------------------------------
-- Schema

textArrayType :: Schema.ScalarType
textArrayType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "text[]",
        Schema.bstCitus = Just "text[]",
        Schema.bstCockroach = Just "text[]"
      }

nestedIntArrayType :: Schema.ScalarType
nestedIntArrayType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "int[][]",
        Schema.bstCitus = Just "int[][]",
        Schema.bstCockroach = Just "int[]" -- nested arrays aren't supported in Cockroach, so we'll skip this test anyway
      }

jsonArrayType :: Schema.ScalarType
jsonArrayType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "json[]",
        Schema.bstCitus = Just "json[]",
        Schema.bstCockroach = Just "json" -- arrays of json aren't supported in Cockroach, so we'll skip this test
      }

enumArrayType :: Schema.ScalarType
enumArrayType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "_made_up_type[]",
        Schema.bstCitus = Just "_made_up_type[]",
        Schema.bstCockroach = Just "_made_up_type[]"
      }

schema :: [Schema.Table]
schema =
  [ (Schema.table "author")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "name" Schema.TStr,
            Schema.column "emails" textArrayType,
            Schema.column "grid" nestedIntArrayType,
            Schema.column "jsons" jsonArrayType,
            Schema.column "made_up" enumArrayType
          ],
        Schema.tablePrimaryKey = ["id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

singleArrayTests :: SpecWith TestEnvironment
singleArrayTests = do
  describe "Saves arrays" $ do
    it "Using native postgres array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Ash"
                      emails: ["ash@ash.com", "ash123@ash.com"]
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Ash",
                        emails: "{ash@ash.com, ash123@ash.com}",
                        grid: "{}",
                        jsons: "{}",
                        made_up: "{}"
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Enum array using native GraphQL array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Craig Cash"
                      made_up: ["a","b", "c"]
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Craig Cash",
                        emails: [],
                        grid: [],
                        jsons: [],
                        made_up: ["a","b","c"]
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      made_up
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Text array using native GraphQL array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Ash"
                      emails: ["ash@ash.com", "ash123@ash.com"]
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Ash",
                        emails: ["ash@ash.com", "ash123@ash.com"],
                        grid: [],
                        jsons: [],
                        made_up: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

jsonArrayTests :: SpecWith TestEnvironment
jsonArrayTests = do
  describe "saves JSON arrays" $ do
    it "JSON array using native GraphQL array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Bruce"
                      jsons: [{ name: "Mr Horse", age: 100}, { name: "Mr Dog", age: 1}]
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Bruce",
                        emails: ["something@something.com"]
                        grid: [],
                        jsons: ["{ \"name\": \"Mr Horse\", \"age\": 100}", "{\"name\":\"Mr Dog\", \"age\": 1}"],
                        made_up: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      jsons
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "JSON array using native GraphQL array syntax and variable" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                insert_hasura_author:
                  affected_rows: 1
                  returning:
                    - name: "Bruce"
                      jsons: [{ name: "Mr Horse", age: 100}, "horses"]
            |]

          actual :: IO Value
          actual =
            postGraphqlWithVariables
              testEnvironment
              [graphql|
                mutation json_variables_test($jsonArray: [json]) {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "Bruce",
                        emails: ["something2@something2.com"],
                        grid: [],
                        jsons: $jsonArray,
                        made_up: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      jsons
                    }
                  }
                }
              |]
              [yaml|
                jsonArray: [{ name: "Mr Horse", age: 100 }, "horses"]
              |]

      shouldReturnYaml testEnvironment actual expected

  describe "Filters with contains" $ do
    it "finds values using _contains" \testEnvironment -> do
      void
        $ postGraphql
          testEnvironment
          [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "contains",
                        emails: ["horse@horse.com", "dog@dog.com"],
                        grid: [],
                        jsons: [],
                        made_up: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                hasura_author:
                  - name: contains
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author (where: { emails: {_contains:["horse@horse.com"]}}) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "finds values using _contained_in" \testEnvironment -> do
      void
        $ postGraphql
          testEnvironment
          [graphql|
                mutation {
                  insert_hasura_author (
                    objects: [
                      {
                        name: "contained_in",
                        emails: ["horse@horse2.com", "dog@dog2.com"],
                        grid: [],
                        jsons: [],
                        made_up: []
                      }
                    ]
                  ) {
                    affected_rows
                    returning {
                      name
                      emails
                    }
                  }
                }
              |]

      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                hasura_author:
                  - name: contained_in
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author (where: { emails: {_contained_in:["cat@cat2.com","dog@dog2.com", "horse@horse2.com"]}}) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

nestedArrayTests :: SpecWith TestEnvironment
nestedArrayTests = do
  describe "Saves nested arrays" $ do
    -- Postgres introspection is able to tell us about thing[] but thing[][] always comes
    -- back as unknown, so the only way to operate on these values continues to
    -- be with Postgres native array syntax. This test is to ensure we have not
    -- broken that.
    it "Using native postgres array syntax" \testEnvironment -> do
      let expected :: Value
          expected =
            [interpolateYaml|
                data:
                  insert_hasura_author:
                    affected_rows: 1
                    returning:
                      - name: "Ash"
                        grid: [[1,2,3],
                              [4,5,6]]
              |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                  mutation {
                    insert_hasura_author (
                      objects: [
                        {
                          name: "Ash",
                          emails: "{}",
                          grid: "{{1,2,3},{4,5,6}}",
                          jsons: "{}",
                          made_up: "{}"
                        }
                      ]
                    ) {
                      affected_rows
                      returning {
                        name
                        grid
                      }
                    }
                  }
                |]

      shouldReturnYaml testEnvironment actual expected

introspectionTests :: SpecWith TestEnvironment
introspectionTests = do
  describe "Array types become GraphQL arrays via introspection" $ do
    it "Produces GraphQL arrays" $ \testEnvironment -> do
      let queryTypesIntrospection :: Value
          queryTypesIntrospection =
            [graphql|
                  {
                    __type(name:"hasura_author") {
                      kind
                      name
                      fields {
                        name
                        type {
                          kind
                          name
                          ofType {
                            kind
                            name
                            ofType {
                              kind
                              name
                              ofType {
                                kind
                                name
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                |]

          expected =
            [yaml|
                {
                  "data": {
                    "__type": {
                      "name": "hasura_author",
                      "kind": "OBJECT",
                      "fields": [
                        {
                          "name": "emails",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "LIST",
                              "name": null,
                              "ofType": {
                                "kind": "NON_NULL",
                                "name": null,
                                "ofType": {
                                  "kind": "SCALAR",
                                  "name": "String",
                                }
                              }
                            }
                          }
                        },
                        {
                          "name": "grid",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "LIST",
                              "name": null,
                              "ofType": {
                                "kind": "NON_NULL",
                                "name": null,
                                "ofType": {
                                  "kind": "SCALAR",
                                  "name": "Int",
                                }
                              }
                            }
                          }
                        },
                        {
                          "name": "id",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "Int",
                              "ofType": null
                            }
                          }
                        },
                        {
                          "name": "jsons",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "LIST",
                              "name": null,
                              "ofType": {
                                "kind": "NON_NULL",
                                "name": null,
                                "ofType": {
                                  "kind": "SCALAR",
                                  "name": "json",
                                }
                              }
                            }
                          }
                        },
                        {
                          "name": "made_up",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "LIST",
                              "name": null,
                              "ofType": {
                                "kind": "NON_NULL",
                                "name": null,
                                "ofType": {
                                  "kind": "SCALAR",
                                  "name": "_made_up_type",
                                }
                              }
                            }
                          }
                        },
                        {
                          "name": "name",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "String",
                              "ofType": null
                            }
                          }
                        }
                      ]
                    }
                  }
                }
                |]

      actual <- postGraphql testEnvironment queryTypesIntrospection

      actual `shouldBeYaml` expected

introspectionWithDisabledFeatureTests :: SpecWith TestEnvironment
introspectionWithDisabledFeatureTests = do
  describe "Array types remain unknown types via introspection" $ do
    it "Produces unknown types" $ \testEnvironment -> do
      let queryTypesIntrospection :: Value
          queryTypesIntrospection =
            [graphql|
                  {
                    __type(name:"hasura_author") {
                      kind
                      name
                      fields {
                        name
                        type {
                          kind
                          name
                          ofType {
                            kind
                            name
                            ofType {
                              kind
                              name
                              ofType {
                                kind
                                name
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                |]

          expected =
            [yaml|
                {
                  "data": {
                    "__type": {
                      "name": "hasura_author",
                      "kind": "OBJECT",
                      "fields": [
                        {
                          "name": "emails",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "_text",
                              "ofType": null
                            }
                          }
                        },
                        {
                          "name": "grid",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "_integer",
                              "ofType": null
                            }
                          }
                        },
                        {
                          "name": "id",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "Int",
                              "ofType": null
                            }
                          }
                        },
                        {
                          "name": "jsons",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "_json",
                              "ofType": null
                            }
                          }
                        },
                        {
                          "name": "made_up",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "__made_up_type",
                              "ofType": null
                            }
                          }
                        },
                        {
                          "name": "name",
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind": "SCALAR",
                              "name": "String",
                              "ofType": null
                            }
                          }
                        }
                      ]
                    }
                  }
                }
                |]

      actual <- postGraphql testEnvironment queryTypesIntrospection

      actual `shouldBeYaml` expected
