{-# LANGUAGE QuasiQuotes #-}

-- | Testing nested relationships.
module Test.NestedRelationshipsSpec (spec) where

import Data.Aeson (Value)
import Data.Text (Text)
import Harness.Backend.BigQuery qualified as Bigquery
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.Test.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarType (..),
    ScalarValue (..),
    Table (..),
    defaultBackendScalarType,
    defaultBackendScalarValue,
    table,
  )
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml
import Hasura.Prelude (tshow)
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
        },
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.Citus,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Citus.setup schema,
          teardown = Citus.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Sqlserver.setup schema,
          teardown = Sqlserver.teardown schema,
          customOptions = Nothing
        },
      Context.Context
        { name = Context.Backend Context.BigQuery,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Bigquery.setup schema,
          teardown = Bigquery.teardown schema,
          customOptions =
            Just $
              Context.Options
                { stringifyNumbers = True
                }
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
        [ [ Schema.VInt 1,
            Schema.VStr "Author 1",
            Schema.parseUTCTimeOrError "2017-09-21 09:39:44"
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author 2",
            Schema.parseUTCTimeOrError "2017-09-21 09:50:44"
          ]
        ]
    }

article :: Schema.Table
article =
  (table "article")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.columnNull "title" textType,
          Schema.columnNull "content" textType,
          Schema.columnNull "is_published" bitType,
          Schema.columnNull "published_on" timestampType,
          Schema.columnNull "author_id" intUnsignedType,
          Schema.columnNull "co_author_id" intUnsignedType
        ],
      tablePrimaryKey = ["id"],
      tableReferences = [Schema.Reference "author_id" "author" "id"],
      tableData =
        [ mkArticle 1 "Article 1" "Sample article content 1" 1 False,
          mkArticle 2 "Article 2" "Sample article content 2" 1 True,
          mkArticle 3 "Article 3" "Sample article content 3" 2 True
        ]
    }
  where
    textType :: ScalarType
    textType =
      TCustomType $
        defaultBackendScalarType
          { bstMysql = Just "TEXT",
            bstMssql = Just "TEXT",
            bstCitus = Just "TEXT",
            bstPostgres = Just "TEXT",
            bstBigQuery = Just "STRING"
          }

    bitType :: ScalarType
    bitType =
      TCustomType $
        defaultBackendScalarType
          { bstMysql = Just "BIT",
            bstMssql = Just "BIT",
            bstCitus = Just "BOOLEAN",
            bstPostgres = Just "BOOLEAN",
            bstBigQuery = Just "BOOL"
          }

    timestampType :: ScalarType
    timestampType =
      TCustomType $
        defaultBackendScalarType
          { bstMysql = Just "TIMESTAMP NULL",
            bstMssql = Just "DATETIME",
            bstCitus = Just "TIMESTAMP",
            bstPostgres = Just "TIMESTAMP",
            bstBigQuery = Just "DATETIME"
          }

    intUnsignedType :: ScalarType
    intUnsignedType =
      TCustomType $
        defaultBackendScalarType
          { bstMysql = Just "INT UNSIGNED",
            bstMssql = Just "INT",
            bstCitus = Just "INT",
            bstPostgres = Just "INT",
            bstBigQuery = Just "INT64"
          }

    backendBool :: Bool -> Int
    backendBool True = 1
    backendBool False = 0

    mkArticle :: Int -> Text -> Text -> Int -> Bool -> [ScalarValue]
    mkArticle pid title content authorId isPublished =
      [ Schema.VInt pid,
        Schema.VStr title,
        Schema.VStr content,
        Schema.VCustomValue $
          defaultBackendScalarValue
            { bsvMysql = Schema.unquotedValue (tshow $ backendBool isPublished),
              bsvCitus = Schema.quotedValue (tshow $ backendBool isPublished),
              bsvPostgres = Schema.quotedValue (tshow $ backendBool isPublished),
              bsvMssql = Schema.unquotedValue (tshow $ backendBool isPublished),
              bsvBigQuery = Schema.unquotedValue (tshow isPublished)
            },
        Schema.VNull,
        Schema.VCustomValue $
          defaultBackendScalarValue
            { bsvMysql = Schema.unquotedValue (tshow authorId),
              bsvCitus = Schema.unquotedValue (tshow authorId),
              bsvPostgres = Schema.unquotedValue (tshow authorId),
              bsvMssql = Schema.unquotedValue (tshow authorId),
              bsvBigQuery = Schema.unquotedValue (tshow authorId)
            },
        Schema.VNull
      ]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Deep nested select with where" $ \testEnvironment ->
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
      articles_by_id_to_author_id(where: {id: {_eq: 1}}) {
         id
         author_by_author_id_to_id {
           id
           articles_by_id_to_author_id(where: {id: {_eq: 1}}) {
             id
             author_by_author_id_to_id {
               id
            }
          }
        }
      }
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
      articles_by_id_to_author_id:
      - id: 1
        author_by_author_id_to_id:
          id: 1
          articles_by_id_to_author_id:
          - id: 1
            author_by_author_id_to_id:
              id: 1
|]

  it "Nested select with where condition" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author (where: {name: {_eq: "Author 1"}}) {
    id
    name
    articles_by_id_to_author_id (where: {is_published: {_eq: true}}) {
      id
      title
      content
    }
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - id: 1
    name: Author 1
    articles_by_id_to_author_id:
    - id: 2
      title: Article 2
      content: Sample article content 2
|]

  it "Nested select on article" $ \testEnvironment -> do
    let articleOne =
          [yaml|
id: 1
title: Article 1
content: Sample article content 1
author_by_author_id_to_id:
  id: 1
  name: Author 1
|]
        articleTwo =
          [yaml|
id: 2
title: Article 2
content: Sample article content 2
author_by_author_id_to_id:
  id: 1
  name: Author 1
|]
        articleThree =
          [yaml|
id: 3
title: Article 3
content: Sample article content 3
author_by_author_id_to_id:
  id: 2
  name: Author 2
|]
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_article {
    id
    title
    content
    author_by_author_id_to_id {
      id
      name
    }
  }
}

|]
      )
      (combinationsObject response (map fromObject [articleOne, articleTwo, articleThree]))

  it "Nested select on article with where condition" $ \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_article (where: {author_by_author_id_to_id: {name: {_eq: "Author 1"}}} ) {
    id
    title
    content
    author_by_author_id_to_id {
      id
      name
    }
  }
}
|]
      )
      ( combinationsObject
          response
          ( map
              fromObject
              [ [yaml|
id: 1
title: Article 1
content: Sample article content 1
author_by_author_id_to_id:
  id: 1
  name: Author 1
|],
                [yaml|
id: 2
title: Article 2
content: Sample article content 2
author_by_author_id_to_id:
  id: 1
  name: Author 1
|]
              ]
          )
      )

response :: Value -> Value
response articles =
  [yaml|
data:
 hasura_article: *articles
|]
