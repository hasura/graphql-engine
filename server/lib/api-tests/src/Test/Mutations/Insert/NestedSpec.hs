{-# LANGUAGE QuasiQuotes #-}

-- |
-- Insert nested objects.
--
-- https://hasura.io/docs/latest/mutations/postgres/insert/#pg-nested-inserts
module Test.Mutations.Insert.NestedSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GlobalTestEnvironment
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
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

--------------------------------------------------------------------------------
-- Schema

-- We have three tables: foo, bar, and baz with foreign key references:
--
--   baz.id -> bar.id -> foo.id
--
-- Since these relations are 1:1, we also define relationships with insertion
-- orders:
--
--   foo -> bar (after_parent)
--   foo -> baz (after_parent)
--   bar -> baz (after_parent)
--   bar -> foo (before_parent)
--   baz -> bar (before_parent)
--   baz -> foo (before_parent)
--
-- Since both 'bar' and 'baz' reference 'foo.id' transitively, any nested insert
-- has to insert 'foo' rows before any of 'bar' and 'baz' rows.
--
-- We expect to be able to make the following insertions:
--
--   foo{bar{baz}}
--   bar{foo, baz}
--   baz{bar{foo}}
--
-- We expect to fail the following insertions, because there is not sufficient
-- metadata available to infer what the combined insertion order should be.
--
--   bar{baz{foo}}
--   baz{bar, foo}
--
-- The following appears to work by luck:
--
--   foo{bar, baz}
--
-- The only information available from the above query is:
--
--  insert foo before bar
--  insert foo before baz
--
-- whereas the table definitions _also_ require 'bar' be inserted before 'baz'.
-- I expect this test would fail if we changed the foreign keys to be bar->baz->foo.

schema :: [Schema.Table]
schema =
  [ (table "foo")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "foo_data" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableManualRelationships =
          [ (Schema.reference "id" "bar" "id")
              { Schema.referenceInsertionOrder = Schema.AfterParent
              },
            (Schema.reference "id" "baz" "id")
              { Schema.referenceInsertionOrder = Schema.AfterParent
              }
          ]
      },
    (table "bar")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "bar_data" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [Schema.reference "id" "foo" "id"],
        tableManualRelationships =
          [ (Schema.reference "id" "baz" "id")
              { Schema.referenceInsertionOrder = Schema.AfterParent
              }
          ]
      },
    (table "baz")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "baz_data" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ (Schema.reference "id" "bar" "id")
              { Schema.referenceInsertionOrder = Schema.BeforeParent
              }
          ],
        tableManualRelationships =
          [ (Schema.reference "id" "foo" "id")
              { Schema.referenceInsertionOrder = Schema.BeforeParent
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Inserting foo{bar{baz}}" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation
              {
                insert_#{schemaName}_foo(
                  objects: 
                  {
                    foo_data: "foo",
                    id: 1,
                    bar_by_id_to_id:
                    {
                      data:
                      {
                        bar_data: "bar",
                        baz_by_id_to_id:
                        {
                          data:
                          {
                            baz_data: "baz"
                          }
                        }
                      }
                    }
                  }
                ) 
                {
                  returning 
                  {
                    foo_data
                    id
                    bar_by_id_to_id
                    {
                      bar_data
                      baz_by_id_to_id
                      {
                        baz_data
                      }
                    }
                  }
                }
              }

            |]

        expected :: Value
        expected =
          [interpolateYaml|
             data:
                 insert_#{schemaName}_foo:
                   returning:
                   - bar_by_id_to_id:
                       bar_data: bar
                       baz_by_id_to_id:
                         baz_data: baz
                     foo_data: foo
                     id: 1
          |]

    shouldReturnYaml testEnvironment actual expected

  it "(Possibly by lucky naming:) Inserting foo{bar, baz}" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation
              {
                insert_#{schemaName}_foo(
                  objects: 
                  {
                    foo_data: "foo",
                    id: 2,
                    baz_by_id_to_id:
                    {
                      data:
                      {
                        baz_data: "baz"
                      }
                    },
                    bar_by_id_to_id:
                    {
                      data:
                      {
                        bar_data: "bar"
                      }
                    }
                  }
                ) 
                {
                  returning 
                  {
                    foo_data
                    id
                    bar_by_id_to_id
                    {
                      bar_data
                      baz_by_id_to_id
                      {
                        baz_data
                      }
                    }
                  }
                }
              }

            |]

        expected :: Value
        expected =
          [interpolateYaml|
             data:
                 insert_#{schemaName}_foo:
                   returning:
                   - bar_by_id_to_id:
                       bar_data: bar
                       baz_by_id_to_id:
                         baz_data: baz
                     foo_data: foo
                     id: 2
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Inserting bar{foo, baz}" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation
              {
                insert_#{schemaName}_bar(
                  objects: 
                  {
                    bar_data: "bar",
                    foo_by_id_to_id:
                    {
                      data:
                      {
                        foo_data: "foo",
                        id: 3,
                      }
                    }
                    baz_by_id_to_id:
                    {
                      data:
                      {
                        baz_data: "baz"
                      }
                    }
                  }
                ) 
                {
                  returning 
                  {
                    bar_data
                    baz_by_id_to_id
                    {
                      baz_data
                    }
                    foo_by_id_to_id
                    {
                      foo_data
                      id
                    }
                  }
                }
              }

            |]

        expected :: Value
        expected =
          [interpolateYaml|
             data:
                 insert_#{schemaName}_bar:
                   returning:
                   - bar_data: bar
                     baz_by_id_to_id:
                       baz_data: baz
                     foo_by_id_to_id:
                       foo_data: foo
                       id: 3
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Inserting baz{bar{foo}}" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation
              {
                insert_#{schemaName}_baz(
                  objects: 
                  {
                    baz_data: "baz",
                    bar_by_id_to_id:
                    {
                      data:
                      {
                        bar_data: "bar",
                        foo_by_id_to_id:
                        {
                          data:
                          {
                            foo_data: "foo",
                            id: 4,
                          }
                        }
                      }
                    }
                  }
                ) 
                {
                  returning 
                  {
                    baz_data
                    bar_by_id_to_id
                    {
                      bar_data
                      foo_by_id_to_id
                      {
                        foo_data
                        id
                      }
                    }
                  }
                }
              }

            |]

        expected :: Value
        expected =
          [interpolateYaml|
             data:
               insert_#{schemaName}_baz:
                 returning:
                 - baz_data: baz
                   bar_by_id_to_id:
                     bar_data: bar
                     foo_by_id_to_id:
                       foo_data: foo
                       id: 4
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Unable to insert bar{baz{foo}}" \testEnvironment -> do
    -- because bar needs inserting before baz, but foo before _both_ bar and baz.

    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata

        actualAct :: IO Value
        actualAct =
          postGraphql
            testEnvironment
            [graphql|
              mutation
              {
                insert_#{schemaName}_bar(
                  objects: 
                  {
                    bar_data: "bar",
                    baz_by_id_to_id:
                    {
                      data:
                      {
                        baz_data: "baz"
                        foo_by_id_to_id:
                        {
                          data:
                          {
                            foo_data: "foo",
                            id: 0,
                          }
                        }
                      }
                    }
                  }
                ) 
                {
                  affected_rows
                }
              }

            |]

        expected :: Value
        expected =
          case backendType of
            "cockroach" ->
              [interpolateYaml|
                 errors:
                   - extensions:
                       code: unexpected
                       internal:
                         arguments: []
                         error:
                           description: null
                           exec_status: FatalError
                           hint: null
                           message: missing "id" primary key column
                           status_code: '42830'
                         prepared: false

              |]
            _ ->
              -- We're not asserting on the message text, because it has
              -- changed between postgres versions.
              [interpolateYaml|
                 errors:
                 - extensions:
                     code: constraint-violation
                     path: $.selectionSet.insert_hasura_bar.args.objects[0]
              |]

    actual <- actualAct
    actual `shouldAtLeastBe` expected

  it "Unable to insert baz{bar,foo}" \testEnvironment -> do
    -- Because information about key values only flow from parent to child,
    -- not between sibling fields.
    --
    -- So 'foo' cannot tell 'bar' which 'id' to use. (and 'bar' too can't tell
    -- 'baz' obviously).

    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata

        actualAct :: IO Value
        actualAct =
          postGraphql
            testEnvironment
            [graphql|
              mutation
              {
                insert_#{schemaName}_baz(
                  objects: 
                  {
                    baz_data: "baz",
                    foo_by_id_to_id:
                    {
                      data:
                      {
                        foo_data: "foo",
                        id: 5,
                      }
                    },
                    bar_by_id_to_id:
                    {
                      data:
                      {
                        bar_data: "bar",
                      }
                    }               
                  }
                ) 
                {
                  affected_rows
                }
              }

            |]

        expected :: Value
        expected =
          case backendType of
            "cockroach" ->
              [interpolateYaml|
                 errors:
                   - extensions:
                       code: unexpected
                       internal:
                         arguments: []
                         error:
                           description: null
                           exec_status: FatalError
                           hint: null
                           message: missing "id" primary key column
                           status_code: '42830'
                         prepared: false

              |]
            _ ->
              -- We're not asserting on the message text, because it has
              -- changed between postgres versions.
              [interpolateYaml|
                 errors:
                 - extensions:
                     code: constraint-violation
                     path: $.selectionSet.insert_hasura_baz.args.objects[0].bar_by_id_to_id.data
              |]

    actual <- actualAct
    actual `shouldAtLeastBe` expected
