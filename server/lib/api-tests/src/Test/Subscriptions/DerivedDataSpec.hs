{-# LANGUAGE QuasiQuotes #-}

-- | Test live queries with derived data
-- https://hasura.io/docs/latest/subscriptions/postgres/livequery/use-cases/#pg-subscribe-derived
module Test.Subscriptions.DerivedDataSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Database.PG.Query.Pool (sql)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Subscriptions (getNextResponse, withSubscriptions)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupPostgres testEnvironment,
                  setupMetadata Postgres.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment,
                  setupCitus testEnvironment,
                  setupMetadata Citus.backendTypeMetadata testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment,
                  setupCockroach testEnvironment,
                  setupMetadata Cockroach.backendTypeMetadata testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "poll")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "question" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "What is your favourite food?"]
          ]
      },
    (table "option")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "poll_id" Schema.TInt,
            Schema.column "text" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VInt 1, Schema.VStr "Pizza"],
            [Schema.VInt 2, Schema.VInt 1, Schema.VStr "Salad"],
            [Schema.VInt 3, Schema.VInt 1, Schema.VStr "Sandwich"]
          ]
      },
    (table "user")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Alice"],
            [Schema.VInt 2, Schema.VStr "Bob"],
            [Schema.VInt 3, Schema.VStr "Carlyle"]
          ]
      },
    (table "vote")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "option_id" Schema.TInt,
            Schema.column "poll_id" Schema.TInt,
            Schema.column "user_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableData = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = withSubscriptions $ do
  describe "Subscriptions involving derived data" do
    it "Fetches derived data correctly" \(mkSubscription, testEnvironment) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      query <-
        mkSubscription
          [graphql|
            subscription getResult {
              #{schemaName}_poll_results
              {
                poll {
                  question
                }
                option {
                  text
                }
                votes
              }
            }
          |]
          []

      do
        let expected :: Value
            expected =
              [interpolateYaml|
                data:
                  #{schemaName}_poll_results: []
              |]

            actual :: IO Value
            actual = getNextResponse query

        shouldReturnYaml testEnvironment actual expected

      -- add some data
      do
        let expected :: Value
            expected =
              [interpolateYaml|
                data:
                  insert_#{schemaName}_vote:
                    affected_rows: 1
              |]
            actual :: IO Value
            actual =
              postGraphql
                testEnvironment
                [graphql|
                  mutation {
                    insert_#{schemaName}_vote(
                      objects: [{id: 1, option_id: 1, poll_id: 1, user_id: 1}]
                    ) {
                      affected_rows
                    }
                  }
                |]
        shouldReturnYaml testEnvironment actual expected

      -- fetch the next response
      do
        let expected :: Value
            expected =
              [interpolateYaml|
                data:
                  #{schemaName}_poll_results:
                  - option:
                      text: "Pizza"
                    poll:
                      question: "What is your favourite food?"
                    votes: 1
              |]

            actual :: IO Value
            actual = getNextResponse query

        shouldReturnYaml testEnvironment actual expected

--------------------------------------------------------------------------------
-- SQL

setupViewSQL :: Text
setupViewSQL =
  [sql|
    CREATE OR REPLACE VIEW hasura.poll_results AS
        SELECT poll.id AS poll_id,
                o.option_id,
                count(*) AS votes
            FROM (
            (
                SELECT vote.option_id,
                    option.poll_id,
                    option.text
                FROM (
                    hasura.vote
                    LEFT JOIN hasura.option ON ((option.id = vote.option_id))
                )
              ) o
                  LEFT JOIN hasura.poll ON ((poll.id = o.poll_id))
          )
        GROUP BY poll.question, o.option_id, poll.id;
  |]

teardownViewSQL :: Text
teardownViewSQL =
  [sql|
    DROP VIEW IF EXISTS hasura.poll_results
  |]

--------------------------------------------------------------------------------
-- Postgres setup

setupPostgres :: TestEnvironment -> Fixture.SetupAction
setupPostgres testEnvironment =
  Fixture.SetupAction
    { Fixture.setupAction =
        Postgres.run_ testEnvironment setupViewSQL,
      Fixture.teardownAction = \_ ->
        Postgres.run_ testEnvironment teardownViewSQL
    }

--------------------------------------------------------------------------------
-- Citus setup

setupCitus :: TestEnvironment -> Fixture.SetupAction
setupCitus testEnvironment =
  Fixture.SetupAction
    { Fixture.setupAction =
        Citus.run_ testEnvironment setupViewSQL,
      Fixture.teardownAction = \_ ->
        Citus.run_ testEnvironment teardownViewSQL
    }

--------------------------------------------------------------------------------
-- Cockroach setup

setupCockroach :: TestEnvironment -> Fixture.SetupAction
setupCockroach testEnvironment =
  Fixture.SetupAction
    { Fixture.setupAction =
        Cockroach.run_ testEnvironment setupViewSQL,
      Fixture.teardownAction = \_ ->
        Cockroach.run_ testEnvironment teardownViewSQL
    }

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendTypeConfig -> TestEnvironment -> Fixture.SetupAction
setupMetadata backendMetadata testEnvironment = do
  let backendPrefix = Fixture.backendTypeString backendMetadata
  Fixture.SetupAction
    { Fixture.setupAction =
        postMetadata_
          testEnvironment
          [interpolateYaml|
            type: bulk
            source: #{source}
            args:
              - type: #{backendPrefix}_track_table
                args:
                  source: #{source}
                  table:
                    name: poll_results
                    schema: #{schemaName}
              - type: #{backendPrefix}_create_object_relationship
                args:
                  source: #{source}
                  name: poll
                  table:
                    name: poll_results
                    schema: #{schemaName}
                  using:
                    manual_configuration:
                      remote_table:
                        name: poll
                        schema: #{schemaName}
                      column_mapping:
                        poll_id: id
              - type: #{backendPrefix}_create_object_relationship
                args:
                  source: #{source}
                  name: option
                  table:
                    name: poll_results
                    schema: #{schemaName}
                  using:
                    manual_configuration:
                      remote_table:
                        name: option
                        schema: #{schemaName}
                      column_mapping:
                        option_id: id
              |],
      Fixture.teardownAction = \_ ->
        postMetadata_
          testEnvironment
          [interpolateYaml|
              type: #{backendPrefix}_untrack_table
              args:
                source: #{source}
                table:
                  name: poll_results
                  schema: #{schemaName}
          |]
    }
  where
    source :: String
    source = Fixture.backendSourceName backendMetadata

    schemaName :: Schema.SchemaName
    schemaName = Schema.getSchemaName testEnvironment
