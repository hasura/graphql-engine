{-# LANGUAGE QuasiQuotes #-}

-- | Testing array relationships.
module Test.ArrayRelationshipsSpec (spec) where

import Harness.Backend.Mysql as Mysql
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
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Mysql.setup schema,
          teardown = Mysql.teardown schema,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Schema

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
          [Schema.VInt 2, Schema.VStr "Author 2", Schema.parseUTCTimeOrError "2017-09-21 09:50:44"]
        ]
    }

article :: Schema.Table
article =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.column "content" Schema.TStr,
          Schema.column "is_published" Schema.TBool,
          Schema.column "published_on" Schema.TUTCTime,
          Schema.columnNull "author_id" Schema.TInt,
          Schema.columnNull "co_author_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences =
        [ Schema.Reference "author_id" "author" "id",
          Schema.Reference "co_author_id" "author" "id"
        ],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Article 1",
            Schema.VStr "Sample article content 1",
            Schema.VBool False,
            Schema.parseUTCTimeOrError "2022-01-01 00:00:00",
            Schema.VInt 1,
            Schema.VInt 2
          ],
          [ Schema.VInt 2,
            Schema.VStr "Article 2",
            Schema.VStr "Sample article content 2",
            Schema.VBool True,
            Schema.parseUTCTimeOrError "2022-01-01 00:00:00",
            Schema.VInt 1,
            Schema.VInt 2
          ],
          [ Schema.VInt 3,
            Schema.VStr "Article 3",
            Schema.VStr "Sample article content 3",
            Schema.VBool True,
            Schema.parseUTCTimeOrError "2022-01-01 00:00:00",
            Schema.VInt 2,
            Schema.VInt 1
          ]
        ]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Select an author and one of their articles" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  # we put id=1 restrictions here because we don't assume ordering support
  hasura_author(where: {id: {_eq: 1}}) {
    id
    # the _by_author_id part is necessary to distinguish between multiple foreign key relationships between the same two tables
    articles_by_author_id(where: {id: {_eq: 1}}) {
      id
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - id: 1
    articles_by_author_id:
      - id: 1
|]
