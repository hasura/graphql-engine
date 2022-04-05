{-# LANGUAGE QuasiQuotes #-}

-- | Test insert check permissions
module Test.InsertCheckPermissionSpec (spec) where

import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Test.Hspec (SpecWith, it)
import Prelude

--------------------------------------------------------------------------------

-- ** Preamble

spec :: SpecWith State
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalState = Context.noLocalState,
          setup = mssqlSetup,
          teardown = mssqlTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------

-- ** Schema

schema :: [Schema.Table]
schema = [author, article]

author :: Schema.Table
author =
  Schema.Table
    "author"
    [ Schema.column "id" Schema.TInt,
      Schema.column "name" Schema.TStr
    ]
    ["id"]
    []
    [ [Schema.VInt 1, Schema.VStr "Author 1"],
      [Schema.VInt 2, Schema.VStr "Author 2"]
    ]

article :: Schema.Table
article =
  Schema.Table
    "article"
    [ Schema.column "id" Schema.TInt,
      Schema.column "title" Schema.TStr,
      Schema.columnNull "content" Schema.TStr,
      Schema.column "author_id" Schema.TInt
    ]
    ["id"]
    [Schema.Reference "author_id" "author" "id"]
    []

--------------------------------------------------------------------------------

-- ** Setup and teardown

mssqlSetup :: (State, ()) -> IO ()
mssqlSetup (state, ()) = do
  Sqlserver.setup schema (state, ())

  -- also setup permissions
  GraphqlEngine.postMetadata_ state $
    [yaml|
type: bulk
args:
- type: mssql_create_insert_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: article
    role: user
    permission:
      check:
        author_by_author_id:
          id: X-Hasura-User-Id
      columns:
      - id
      - title
      - content
      - author_id
- type: mssql_create_select_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: article
    role: user
    permission:
      filter:
        author_by_author_id:
          id: X-Hasura-User-Id
      columns:
      - id
      - title
      - content
      - author_id
- type: mssql_create_insert_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: author
    role: user
    permission:
      check:
        id: X-Hasura-User-Id
      columns:
      - id
      - name
|]

mssqlTeardown :: (State, ()) -> IO ()
mssqlTeardown (state, ()) = do
  -- teardown permissions
  GraphqlEngine.postMetadata_ state $
    [yaml|
type: bulk
args:
- type: mssql_drop_insert_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: article
    role: user
- type: mssql_drop_select_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: article
    role: user
- type: mssql_drop_insert_permission
  args:
    source: mssql
    table:
      schema: hasura
      name: author
    role: user
|]

  -- and then rest of the teardown
  Sqlserver.teardown schema (state, ())

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith State
tests opts = do
  let userHeaders = [("X-Hasura-Role", "user"), ("X-Hasura-User-Id", "2")]
  it "Insert article with mismatching author_id and X-Hasura-User-Id" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          userHeaders
          [graphql|
mutation {
  insert_hasura_article(
    objects:[{id: 1, title: "Author 1 article", author_id: 1}]
  ){
    affected_rows
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: "$"
    code: permission-error
  message: check constraint of an insert permission has failed
|]

  it "Insert article with matching author_id and X-Hasura-User-Id" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          userHeaders
          [graphql|
mutation {
  insert_hasura_article(
    objects:[{id: 1, title: "Author 2 article", author_id: 2}]
  ){
    affected_rows
    returning {
      id
      title
      content
      author_id
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_article:
    returning:
    - author_id: 2
      content: null
      id: 1
      title: Author 2 article
    affected_rows: 1
|]

  it "Insert author with mismatching id and X-Hasura-User-Id" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          state
          userHeaders
          [graphql|
mutation {
  insert_hasura_author(
    objects: [{id: 3, name: "Author 3"}]
  ){
    affected_rows
  }
}
|]
      )
      [yaml|
errors:
- extensions:
    path: "$"
    code: permission-error
  message: check constraint of an insert permission has failed
|]
