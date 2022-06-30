{-# LANGUAGE QuasiQuotes #-}

module Test.BigQuery.GraphQLQueryBasicSpec (spec) where

import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = BigQuery.setup schema,
          teardown = BigQuery.teardown schema,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Schema
--
-- This data comes straight from pytest for now.

schema :: [Table]
schema = [authorTable, articleTable]

authorTable :: Table
authorTable =
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

articleTable :: Table
articleTable =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.column "content" Schema.TStr,
          Schema.column "author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences = [Schema.Reference "author_id" "author" "id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Title 1",
            Schema.VStr "Content 1",
            Schema.VInt 1
          ],
          [ Schema.VInt 2,
            Schema.VStr "Title 2",
            Schema.VStr "Content 2",
            Schema.VInt 2
          ],
          [ Schema.VInt 3,
            Schema.VStr "Title 3",
            Schema.VStr "Content 3",
            Schema.VInt 3
          ],
          [ Schema.VInt 4,
            Schema.VStr "Title 4",
            Schema.VStr "Content 4",
            Schema.VInt 4
          ]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "User can see article fields" \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_article(order_by: {id: asc}) {
    id
    title
    content
  }
}
|]
      )
      [yaml|
data:
  hasura_article:
  - id: '1'
    title: Title 1
    content: Content 1
  - id: '2'
    title: Title 2
    content: Content 2
  - id: '3'
    title: Title 3
    content: Content 3
  - id: '4'
    title: Title 4
    content: Content 4
|]
