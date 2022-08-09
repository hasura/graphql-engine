{-# LANGUAGE QuasiQuotes #-}

module Test.Schema.ViewsSpec (spec) where

import Data.Aeson (Value)
import Database.PG.Query.Pool (sql)
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    [ (Fixture.fixture $ Fixture.Backend Fixture.MySQL)
        { Fixture.setupTeardown = \(testEnvironment, _) ->
            [ Mysql.setupTablesAction schema testEnvironment,
              setupMysql,
              setupMetadata Fixture.MySQL testEnvironment
            ]
        },
      (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
        { Fixture.setupTeardown = \(testEnvironment, _) ->
            [ Postgres.setupTablesAction schema testEnvironment,
              setupPostgres,
              setupMetadata Fixture.Postgres testEnvironment
            ]
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Alice"],
            [Schema.VInt 2, Schema.VStr "Bob"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Queries involving views" do
    it "Queries views correctly" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                hasura_author_view:
                - id: 1
                  name: Alice
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  hasura_author_view(where: { id: { _eq: 1 } }) {
                    id
                    name
                  }
                }
              |]

      actual `shouldBe` expected

--------------------------------------------------------------------------------
-- MySQL setup

setupMysql :: Fixture.SetupAction
setupMysql =
  Fixture.SetupAction
    { Fixture.setupAction =
        Mysql.run_
          [sql|
            CREATE OR REPLACE VIEW author_view
            AS SELECT * FROM author
          |],
      Fixture.teardownAction = \_ ->
        Mysql.run_
          [sql|
            DROP VIEW IF EXISTS author_view
          |]
    }

--------------------------------------------------------------------------------
-- Postgres setup

setupPostgres :: Fixture.SetupAction
setupPostgres =
  Fixture.SetupAction
    { Fixture.setupAction =
        Postgres.run_
          [sql|
            CREATE OR REPLACE VIEW author_view
            AS SELECT * FROM author
          |],
      Fixture.teardownAction = \_ ->
        Postgres.run_
          [sql|
            DROP VIEW IF EXISTS author_view
          |]
    }

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: Fixture.BackendType -> TestEnvironment -> Fixture.SetupAction
setupMetadata backend testEnvironment =
  Fixture.SetupAction
    { Fixture.setupAction =
        postMetadata_
          testEnvironment
          [yaml|
            type: *track
            args:
              source: *label
              table:
                name: author_view
                schema: hasura
          |],
      Fixture.teardownAction = \_ ->
        postMetadata_
          testEnvironment
          [yaml|
            type: *untrack
            args:
              source: *label
              table:
                name: author_view
                schema: hasura
          |]
    }
  where
    label :: String
    label = Fixture.defaultBackendTypeString backend

    track :: String
    track = label <> "_track_table"

    untrack :: String
    untrack = label <> "_untrack_table"
