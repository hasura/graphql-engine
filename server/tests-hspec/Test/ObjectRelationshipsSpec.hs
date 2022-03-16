{-# LANGUAGE QuasiQuotes #-}

-- | Testing object relationships.
module Test.ObjectRelationshipsSpec (spec) where

import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec = do
  Context.run
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalState = Context.noLocalState,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        }
    ]
    mssqlTests

  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalState = Context.noLocalState,
          setup = Mysql.setup schema,
          teardown = Mysql.teardown schema,
          customOptions = Nothing
        }
    ]
    mysqlTests

--------------------------------------------------------------------------------
-- Schema

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
      Schema.columnNull "author_id" Schema.TInt
    ]
    ["id"]
    [Schema.Reference "author_id" "author" "id"]
    [ [ Schema.VInt 1,
        Schema.VInt 1
      ],
      [ Schema.VInt 2,
        Schema.VInt 1
      ],
      [ Schema.VInt 3,
        Schema.VInt 2
      ],
      [ Schema.VInt 4,
        Schema.VNull
      ]
    ]

--------------------------------------------------------------------------------
-- Tests

mssqlTests :: Context.Options -> SpecWith State
mssqlTests opts = do
  usingWhereClause opts
  nullField opts

mysqlTests :: Context.Options -> SpecWith State
mysqlTests opts = do
  usingWhereClause opts
  xdescribe
    "Pending: The MySQL backend currently fails with relationship fields that are null.\
    \ (https://github.com/hasura/graphql-engine-mono/issues/3650)"
    (nullField opts)

usingWhereClause :: Context.Options -> SpecWith State
usingWhereClause opts = do
  it "Author of article where id=1" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: {id: {_eq: 1}}) {
    id
    author_by_author_id {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_article:
  - id: 1
    author_by_author_id:
      id: 1
|]

nullField :: Context.Options -> SpecWith State
nullField opts = do
  it "Can realise a null relationship field" $ \state ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          state
          [graphql|
query {
  hasura_article(where: {id: {_eq: 4}}) {
    id
    author_by_author_id {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_article:
  - author_by_author_id: null
    id: 4
|]
