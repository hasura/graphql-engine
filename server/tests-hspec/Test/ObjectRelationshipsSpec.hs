{-# LANGUAGE QuasiQuotes #-}

-- | Testing object relationships.
module Test.ObjectRelationshipsSpec (spec) where

import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Sqlserver qualified as Sqlserver
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
-- Preamble

spec :: SpecWith TestEnvironment
spec = do
  Context.run
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        }
    ]
    mssqlTests

  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
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
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Author 1"],
          [Schema.VInt 2, Schema.VStr "Author 2"]
        ]
    }

article :: Schema.Table
article =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.columnNull "author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences = [Schema.Reference "author_id" "author" "id"],
      tableData =
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
    }

--------------------------------------------------------------------------------
-- Tests

mssqlTests :: Context.Options -> SpecWith TestEnvironment
mssqlTests opts = do
  usingWhereClause opts
  nullField opts

mysqlTests :: Context.Options -> SpecWith TestEnvironment
mysqlTests opts = do
  usingWhereClause opts
  xdescribe
    "Pending: The MySQL backend currently fails with relationship fields that are null.\
    \ (https://github.com/hasura/graphql-engine-mono/issues/3650)"
    (nullField opts)

usingWhereClause :: Context.Options -> SpecWith TestEnvironment
usingWhereClause opts = do
  it "Author of article where id=1" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_article(where: {id: {_eq: 1}}) {
    id
    author_by_author_id_to_id {
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
    author_by_author_id_to_id:
      id: 1
|]

nullField :: Context.Options -> SpecWith TestEnvironment
nullField opts = do
  it "Can realise a null relationship field" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_article(where: {id: {_eq: 4}}) {
    id
    author_by_author_id_to_id {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_article:
  - author_by_author_id_to_id: null
    id: 4
|]
