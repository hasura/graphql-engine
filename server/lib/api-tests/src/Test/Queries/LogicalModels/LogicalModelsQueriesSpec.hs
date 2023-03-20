{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Queries.LogicalModels.LogicalModelsQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Database.PG.Query qualified as PG
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] $
    Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Citus.setupTablesAction schema testEnvironment
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

-- we add and track a table here as it's the only way we can currently define a
-- return type
schema :: [Schema.Table]
schema =
  [ (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "date" Schema.TUTCTime
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Dogs",
              Schema.VStr "I like to eat dog food I am a dogs I like to eat dog food I am a dogs I like to eat dog food I am a dogs",
              Schema.VUTCTime (UTCTime (fromOrdinalDate 2000 1) 0)
            ]
          ]
      }
  ]

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let query :: Text
      query = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"

      helloWorldLogicalModel :: Schema.LogicalModel
      helloWorldLogicalModel =
        (Schema.logicalModel "hello_world_function" query)
          { Schema.logicalModelColumns =
              [ Schema.logicalModelColumn "one" Schema.TStr,
                Schema.logicalModelColumn "two" Schema.TStr
              ]
          }

      shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Testing Logical Models" $ do
    it "Runs the absolute simplest query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
                    - one: "welcome"
                      two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function {
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs simple query with a basic where clause" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel source helloWorldLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function (where: { two: { _eq: "world" } }){
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query using distinct_on and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          queryWithDuplicates :: Text
          queryWithDuplicates = "SELECT * FROM (VALUES ('hello', 'world'), ('hello', 'friend')) as t(\"one\", \"two\")"

          helloWorldLogicalModelWithDuplicates :: Schema.LogicalModel
          helloWorldLogicalModelWithDuplicates =
            (Schema.logicalModel "hello_world_function" queryWithDuplicates)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldLogicalModelWithDuplicates testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function (
                  distinct_on: [one]
                  order_by: [{one:asc}]
                ){
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query that takes no parameters" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      Schema.trackLogicalModel sourceName helloWorldLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function:
                    - one: "hello"
                      two: "world"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function(where: {one: {_eq: "hello"}}) {
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query that takes one dummy parameter and order_by" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloWorldLogicalModelWithDummyArgument :: Schema.LogicalModel
          helloWorldLogicalModelWithDummyArgument =
            (Schema.logicalModel "hello_world_function_with_dummy" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "dummy" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldLogicalModelWithDummyArgument testEnvironment

      let expected =
            [yaml|
                data:
                  hello_world_function_with_dummy:
                    - two: "world"
                    - two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_world_function_with_dummy(args: {dummy: "ignored"}, order_by: {one: asc}) {
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query that takes no parameters but ends with a comment" $ \testEnvironment -> do
      let spicyQuery :: Text
          spicyQuery = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\") -- my query"

      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          helloCommentLogicalModel :: Schema.LogicalModel
          helloCommentLogicalModel =
            (Schema.logicalModel "hello_comment_function" spicyQuery)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloCommentLogicalModel testEnvironment

      let expected =
            [yaml|
                data:
                  hello_comment_function:
                    - one: "hello"
                      two: "world"
                    - one: "welcome"
                      two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                hello_comment_function {
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected

    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" spicyQuery)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt(args: { length: 34 }) {
                  id
                  title
                  date
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  article_with_excerpt:
                    - id: 1
                      title: "Dogs"
                      date: "2000-01-01T00:00:00"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      actual `shouldBe` expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

          mkArticleWithExcerptLogicalModel :: Text -> Schema.LogicalModel
          mkArticleWithExcerptLogicalModel name =
            (Schema.logicalModel name spicyQuery)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel
        source
        (mkArticleWithExcerptLogicalModel "article_with_excerpt_1")
        testEnvironment

      Schema.trackLogicalModel
        source
        (mkArticleWithExcerptLogicalModel "article_with_excerpt_2")
        testEnvironment

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                article_with_excerpt_1(args: { length: 34 }) {
                  excerpt
                }
                article_with_excerpt_2(args: { length: "13" }) {
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  article_with_excerpt_1:
                    - excerpt: "I like to eat dog food I am a dogs..."
                  article_with_excerpt_2:
                    - excerpt: "I like to eat..."
              |]

      actual `shouldBe` expected

    it "Uses a one parameter query and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" spicyQuery)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment

      let actual :: IO Value
          actual =
            GraphqlEngine.postGraphql
              testEnvironment
              [graphql|
              query {
                first: article_with_excerpt(args: { length: 34 }) {
                  excerpt
                }
                second: article_with_excerpt(args: { length: 13 }) {
                  excerpt
                }
              }
           |]

          expected =
            [yaml|
                data:
                  first:
                    - excerpt: "I like to eat dog food I am a dogs..."
                  second:
                    - excerpt: "I like to eat..."
              |]

      actual `shouldBe` expected

    it "Uses a one parameter query, passing it a GraphQL variable" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      let spicyQuery :: Text
          spicyQuery =
            [PG.sql| select
                            id,
                            title,
                            (substring(content, 1, {{length}}) || (case when length(content) < {{length}} then '' else '...' end)) as excerpt,
                            date
                          from article
                      |]

          articleWithExcerptLogicalModel :: Schema.LogicalModel
          articleWithExcerptLogicalModel =
            (Schema.logicalModel "article_with_excerpt" spicyQuery)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "id" Schema.TInt,
                    Schema.logicalModelColumn "title" Schema.TStr,
                    Schema.logicalModelColumn "excerpt" Schema.TStr,
                    Schema.logicalModelColumn "date" Schema.TUTCTime
                  ],
                Schema.logicalModelArguments =
                  [ Schema.logicalModelColumn "length" Schema.TInt
                  ]
              }

      Schema.trackLogicalModel source articleWithExcerptLogicalModel testEnvironment

      let variables =
            [yaml|
              length: 34
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithVariables
              testEnvironment
              [graphql|
                query MyQuery($length: Int!) {
                  article_with_excerpt(args: { length: $length }) {
                    excerpt
                  }
                }
             |]
              variables

          expected =
            [yaml|
                data:
                  article_with_excerpt:
                    - excerpt: "I like to eat dog food I am a dogs..."
              |]

      actual `shouldBe` expected

    it "Uses a column permission that we are allowed to access" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermLogicalModel :: Schema.LogicalModel
          helloWorldPermLogicalModel =
            (Schema.logicalModel "hello_world_perms" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldPermLogicalModel testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    root_field_name: hello_world_perms
                    role: "test"
                    permission:
                      columns:
                        - one
                      filter: {}
            |]
        )
        [yaml|
          - message: success
        |]

      let expected =
            [yaml|
                data:
                  hello_world_perms:
                    - one: "hello"
                    - one: "welcome"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_perms {
                  one
                }
              }
           |]

      actual `shouldBe` expected

    it "Fails because we access a column we do not have permissions for" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermLogicalModel :: Schema.LogicalModel
          helloWorldPermLogicalModel =
            (Schema.logicalModel "hello_world_perms" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldPermLogicalModel testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    root_field_name: hello_world_perms
                    role: "test"
                    permission:
                      columns:
                        - two
                      filter: {}
            |]
        )
        [yaml|
          - message: success
        |]

      let expected =
            [yaml|
                errors:
                  - extensions:
                      code: validation-failed
                      path: $.selectionSet.hello_world_perms.selectionSet.one
                    message: "field 'one' not found in type: 'hello_world_perms'"
            |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_perms {
                  one
                }
              }
           |]

      actual `shouldBe` expected

    it "Using row permissions filters out some results" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          backendType = BackendType.backendTypeString backendTypeMetadata
          createPermRequestType = backendType <> "_create_logical_model_select_permission"

          helloWorldPermLogicalModel :: Schema.LogicalModel
          helloWorldPermLogicalModel =
            (Schema.logicalModel "hello_world_perms" query)
              { Schema.logicalModelColumns =
                  [ Schema.logicalModelColumn "one" Schema.TStr,
                    Schema.logicalModelColumn "two" Schema.TStr
                  ]
              }

      Schema.trackLogicalModel source helloWorldPermLogicalModel testEnvironment

      shouldReturnYaml
        opts
        ( GraphqlEngine.postMetadata
            testEnvironment
            [yaml|
              type: bulk
              args:
                - type: *createPermRequestType
                  args:
                    source: *source
                    root_field_name: hello_world_perms
                    role: "test"
                    permission:
                      columns: "*"
                      filter:
                        one:
                          _eq: "welcome"
            |]
        )
        [yaml|
          - message: success
        |]

      let expected =
            [yaml|
                data:
                  hello_world_perms:
                    - one: "welcome"
                      two: "friend"
              |]

          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [("X-Hasura-Role", "test")]
              [graphql|
              query {
                hello_world_perms {
                  one
                  two
                }
              }
           |]

      actual `shouldBe` expected
