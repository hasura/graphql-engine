{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Test querying an entity for a couple fields.
module BasicFieldsSpec (spec) where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Int
import Data.Text (Text)
import Database.Persist
import Database.Persist.MySQL qualified as Mysql
import Database.Persist.Postgresql qualified as Postgresql
import Database.Persist.Sql
import Database.Persist.TH
import Harness.Constants as Constants
import Harness.Feature qualified as Feature
import Harness.Graphql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Mysql as Mysql
import Harness.Postgres as Postgres
import Harness.Sql
import Harness.State (State)
import Harness.Yaml
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Schema

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Author
    Id Int32
    name Text
    deriving Eq
    deriving Show
|]

--------------------------------------------------------------------------------
-- Data setup

setupDatabase :: (MonadIO m) => ReaderT SqlBackend m ()
setupDatabase = do
  void $
    insertKey
      (AuthorKey {unAuthorKey = 1} :: AuthorId)
      Author {authorName = "Author 1"}
  void $
    insertKey
      AuthorKey {unAuthorKey = 2}
      Author {authorName = "Author 2"}

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Feature.feature
    Feature.Feature
      { Feature.backends =
          [ Feature.Backend
              { name = "MySQL",
                setup = mysqlSetup,
                teardown = mysqlTeardown
              },
            Feature.Backend
              { name = "PostgreSQL",
                setup = postgresSetup,
                teardown = postgresTeardown
              }
          ],
        Feature.tests = tests
      }

--------------------------------------------------------------------------------
-- MySQL backend

mysqlSetup :: State -> IO ()
mysqlSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: mysql
    kind: mysql
    tables: []
    configuration:
      database: *mysqlDatabase
      user: *mysqlUser
      password: *mysqlPassword
      host: *mysqlHost
      port: *mysqlPort
      pool_settings: {}
|]

  -- Setup database
  Mysql.runPersistent do
    Mysql.runMigration migrateAll
    setupDatabase

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    schema: hasura
    name: author
|]

mysqlTeardown :: State -> IO ()
mysqlTeardown _ = do
  Mysql.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- PostgreSQL backend

postgresSetup :: State -> IO ()
postgresSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: postgres
    kind: postgres
    tables: []
    configuration:
      connection_info:
        database_url: *postgresqlConnectionString
        pool_settings: {}
|]

  -- Setup database
  Postgres.runPersistent do
    Postgresql.runMigration migrateAll
    setupDatabase

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: postgres_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: author
|]

postgresTeardown :: State -> IO ()
postgresTeardown _ = do
  Postgres.run_
    [sql|
DROP TABLE hasura.author;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith State
tests = do
  it "Author fields" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_author {
    name
    id
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
    id: 1
  - name: Author 2
    id: 2
|]
  it "Use operationName" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
operationName: chooseThisOne
query: |
  query ignoreThisOne {
    MyQuery {
      name
    }
  }
  query chooseThisOne {
    hasura_author {
      id
      name
    }
  }
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
    id: 1
  - name: Author 2
    id: 2
|]
  it "Missing field" $ \state -> do
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_author {
    id
    name
    notPresentCol
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    code: validation-failed
    path: $.selectionSet.hasura_author.selectionSet.notPresentCol
  message: |-
    field "notPresentCol" not found in type: 'hasura_author'
|]
  it "Missing table" $ \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  random {
    id
    name
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    code: validation-failed
    path: $.selectionSet.random
  message: |-
    field "random" not found in type: 'query_root'
|]
