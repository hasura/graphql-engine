{-# LANGUAGE QuasiQuotes #-}

-- | Testing manual, misconfigured object relationships.
--   Specifically, having manual relationships with one-to-many relationships.
--   Test case for bug reported at https://github.com/hasura/graphql-engine/issues/7936
module Test.ObjectRelationshipsLimitSpec (spec) where

import Harness.Backend.Postgres as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- * Schema

schema :: [Schema.Table]
schema = [author, article]

author :: Schema.Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr,
          Schema.column "createdAt" Schema.TUTCTime
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Author 1", Schema.parseUTCTimeOrError "2017-09-21 09:39:44"],
          [Schema.VInt 2, Schema.VStr "Author 2", Schema.parseUTCTimeOrError "2017-09-21 09:50:44"],
          [Schema.VInt 3, Schema.VStr "Author 1", Schema.parseUTCTimeOrError "2017-09-21 09:55:44"]
        ]
    }

article :: Schema.Table
article =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "author_name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableReferences = [], -- No references; we are using @manual_configuration@ to make the object relationship
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Author 1"],
          [Schema.VInt 2, Schema.VStr "Author 2"]
        ]
    }

--------------------------------------------------------------------------------

-- ** Setup and teardown override

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (testEnvironment, ()) = do
  Postgres.setup schema (testEnvironment, ())
  -- also setup special relationship
  GraphqlEngine.postMetadata_ testEnvironment $
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

postgresTeardown :: (TestEnvironment, ()) -> IO ()
postgresTeardown (testEnvironment, ()) = do
  -- first teardown special relationship
  GraphqlEngine.postMetadata_ testEnvironment $
    [yaml|
type: pg_drop_relationship
args:
  source: postgres
  table:
    schema: hasura
    name: article
  relationship: author
|]
  -- and then rest of the teardown
  Postgres.teardown schema (testEnvironment, ())

--------------------------------------------------------------------------------

-- * Tests

-- | Many of these may return non-deterministic results because graphql-engine
--   need to choose one of the available rows in a misconfigured manual relationship
--   that has a one-to-many relationship instead of the expected one-to-one.
--
--   Because of that, we use 'shouldReturnOneOfYaml' and list all of the possible (valid)
--   expected results.
tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Query by id" $ \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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

  it "Query limit 2" $ \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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

  it "where author name" $ \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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

  it "order by author id" $ \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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

  it "count articles" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
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
