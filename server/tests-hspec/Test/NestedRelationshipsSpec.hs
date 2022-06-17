{-# LANGUAGE QuasiQuotes #-}

-- | Testing nested relationships.
--
-- Original inspiration for this module Test.is <https://github.com/hasura/graphql-engine-mono/blob/08caf7df10cad0aea0916327736147a0a8f808d1/server/tests-py/queries/graphql_query/mysql/nested_select_query_deep.yaml>
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
    ManualRelationship (..),
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
          setup =
            Bigquery.setupWithAdditionalRelationship
              schema
              [authorArticles],
          teardown =
            Bigquery.teardownWithAdditionalRelationship
              schema
              [authorArticles],
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

authorArticles :: ManualRelationship
authorArticles =
  ManualRelationship
    { relSourceTable = "author",
      relTargetTable = "article",
      relSourceColumn = "id",
      relTargetColumn = "author_id"
    }

author :: Schema.Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr,
          Schema.column "createdAt" dateTimeType
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "Author 1",
            Schema.VCustomValue $
              defaultBackendScalarValue
                { bsvMysql = Schema.quotedValue "2017-09-21 09:39:44",
                  bsvCitus = Schema.quotedValue "2017-09-21T09:39:44",
                  bsvMssql = Schema.quotedValue "2017-09-21T09:39:44Z",
                  bsvPostgres = Schema.quotedValue "2017-09-21T09:39:44",
                  bsvBigQuery = Schema.quotedValue "2017-09-21T09:39:44"
                }
          ],
          [ Schema.VInt 2,
            Schema.VStr "Author 2",
            Schema.VCustomValue $
              defaultBackendScalarValue
                { bsvMysql = Schema.quotedValue "2017-09-21 09:50:44",
                  bsvCitus = Schema.quotedValue "2017-09-21T09:50:44",
                  bsvMssql = Schema.quotedValue "2017-09-21T09:50:44Z",
                  bsvPostgres = Schema.quotedValue "2017-09-21T09:50:44",
                  bsvBigQuery = Schema.quotedValue "2017-09-21T09:50:44"
                }
          ]
        ]
    }
  where
    dateTimeType :: ScalarType
    dateTimeType =
      TCustomType $
        defaultBackendScalarType
          { bstMysql = Just "DATETIME",
            bstMssql = Just "DATETIME",
            bstCitus = Just "TIMESTAMP",
            bstPostgres = Just "TIMESTAMP",
            bstBigQuery = Just "DATETIME"
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
          Schema.columnNull "author_id" intUnsingedType,
          Schema.columnNull "co_author_id" intUnsingedType
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

    intUnsingedType :: ScalarType
    intUnsingedType =
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

-- Equivalent python suite: test_nested_select_query_deep
-- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L280
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
    author_by_author_id {
      id
      articles_by_author_id(where: {id: {_eq: 1}}) {
         id
         author_by_author_id {
           id
           articles_by_author_id(where: {id: {_eq: 1}}) {
             id
             author_by_author_id {
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
    author_by_author_id:
      id: 1
      articles_by_author_id:
      - id: 1
        author_by_author_id:
          id: 1
          articles_by_author_id:
          - id: 1
            author_by_author_id:
              id: 1
|]
  -- Equivalent python suite: test_nested_select_query_where
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L283
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
    articles_by_author_id (where: {is_published: {_eq: true}}) {
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
    articles_by_author_id:
    - id: 2
      title: Article 2
      content: Sample article content 2
|]
  -- Equivalent python suite: test_nested_select_query_article_author
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L277
  it "Nested select on article" $ \testEnvironment -> do
    let articleOne =
          [yaml|
id: 1
title: Article 1
content: Sample article content 1
author_by_author_id:
  id: 1
  name: Author 1
|]
        articleTwo =
          [yaml|
id: 2
title: Article 2
content: Sample article content 2
author_by_author_id:
  id: 1
  name: Author 1
|]
        articleThree =
          [yaml|
id: 3
title: Article 3
content: Sample article content 3
author_by_author_id:
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
    author_by_author_id {
      id
      name
    }
  }
}

|]
      )
      (combinationsObject response (map fromObject [articleOne, articleTwo, articleThree]))
  -- Equivalent python suite: test_nested_select_query_where_on_relationship
  -- https://github.com/hasura/graphql-engine/blob/369d1ab2f119634b0e27e9ed353fa3d08c22d3fb/server/tests-py/test_graphql_queries.py#L286
  it "Nested select on article with where condition" $ \testEnvironment ->
    shouldReturnOneOfYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_article (where: {author_by_author_id: {name: {_eq: "Author 1"}}} ) {
    id
    title
    content
    author_by_author_id {
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
author_by_author_id:
  id: 1
  name: Author 1
|],
                [yaml|
id: 2
title: Article 2
content: Sample article content 2
author_by_author_id:
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
