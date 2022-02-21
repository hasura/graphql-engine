{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Testing manual, misconfigured object relationships.
--   Specifically, having manual relationships with one-to-many relationships.
--   Test case for bug reported at https://github.com/hasura/graphql-engine/issues/7936
module Test.ObjectRelationshipsLimitSpec (spec) where

import Harness.Backend.Postgres as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Sql
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.Postgres,
          mkLocalState = Context.noLocalState,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Tests

-- | Many of these may return non-deterministic results because graphql-engine
--   need to choose one of the available rows in a misconfigured manual relationship
--   that has a one-to-many relationship instead of the expected one-to-one.
--
--   Because of that, we use 'shouldReturnOneOfYaml' and list all of the possible (valid)
--   expected results.
tests :: Context.Options -> SpecWith State
tests opts = do
  it "Query by id" $ \state ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: {id: {_eq: 1}}) {
    id
    author {
      id
    }
  }
}
|]
      )
      [ [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 1
|],
        [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 3
|]
      ]

  it "Query limit 2" $ \state ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(limit: 2) {
    id
    author {
      id
    }
  }
}
|]
      )
      [ [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 1
  - id: 2
    author:
      id: 2
|],
        [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 3
  - id: 2
    author:
      id: 2
|]
      ]

  it "where author name" $ \state ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: { author: { name: { _eq: "Author 1" } } }) {
    id
    author {
      id
    }
  }
}
|]
      )
      [ [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 1
|],
        [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 3
|]
      ]

  it "order by author id" $ \state ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(order_by: {author: {id: asc}}) {
    id
    author {
      id
    }
  }
}
|]
      )
      [ [yaml|
data:
  hasura_article:
  - id: 1
    author:
      id: 1
  - id: 2
    author:
      id: 2
|],
        [yaml|
data:
  hasura_article:
  - id: 2
    author:
      id: 2
  - id: 1
    author:
      id: 3
|]
      ]

  it "count articles" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article_aggregate {
    aggregate {
      count
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_article_aggregate:
    aggregate:
      count: 2
|]

--------------------------------------------------------------------------------

-- * Postgres backend

----------------------

-- ** Setup

postgresSetup :: (State, ()) -> IO ()
postgresSetup (state, ()) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Postgres.defaultSourceMetadata
  postgresSetupTables
  postgresInsertValues
  postgresTrackTables state
  postgresCreateRelationships state

postgresSetupTables :: IO ()
postgresSetupTables = do
  -- Setup tables
  Postgres.run_
    [sql|
CREATE TABLE hasura.author
(
    id SERIAL PRIMARY KEY,
    name TEXT,
    created_at TIMESTAMP
);
|]

  Postgres.run_
    [sql|
CREATE TABLE hasura.article (
   id SERIAL PRIMARY KEY,
   author_name TEXT
);
|]

postgresInsertValues :: IO ()
postgresInsertValues = do
  Postgres.run_
    [sql|
INSERT INTO hasura.author
    (name, created_at)
VALUES
    ( 'Author 1', '2017-09-21 09:39:44' ),
    ( 'Author 2', '2017-09-21 09:50:44' ),
    ( 'Author 1', '2017-09-21 09:55:44' );
|]

  Postgres.run_
    [sql|
INSERT INTO hasura.article
    (author_name)
VALUES
    ( 'Author 1' ),
    ( 'Author 2' );
|]

postgresTrackTables :: State -> IO ()
postgresTrackTables state = do
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: author
|]
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: article
|]

postgresCreateRelationships :: State -> IO ()
postgresCreateRelationships state = do
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_create_object_relationship
args:
  source: postgres
  table:
    schema: hasura
    name: article
  name: author
  using:
    manual_configuration:
       remote_table:
         schema: hasura
         name: author
       column_mapping:
         author_name: name
|]

----------------------

-- ** Teardown

postgresTeardown :: (State, ()) -> IO ()
postgresTeardown _ = do
  Postgres.run_
    [sql|
DROP TABLE hasura.article;
|]
  Postgres.run_
    [sql|
DROP TABLE hasura.author;
|]
