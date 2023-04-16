{-# LANGUAGE QuasiQuotes #-}

-- | Access to the SQL
module Test.Databases.SQLServer.NativeQueriesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] $
    Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Sqlserver.setupTablesAction schema testEnvironment
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

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

tests :: SpecWith TestEnvironment
tests = do
  let articleQuery :: Schema.SchemaName -> Text
      articleQuery schemaName =
        "select id, title,(substring(content, 1, {{length}}) + (case when len(content) < {{length}} then '' else '...' end)) as excerpt,date from [" <> Schema.unSchemaName schemaName <> "].[article]"

      articleWithExcerptReturnType :: Schema.CustomType
      articleWithExcerptReturnType =
        (Schema.customType "article_with_excerpt")
          { Schema.customTypeColumns =
              [ Schema.nativeQueryColumn "id" Schema.TInt,
                Schema.nativeQueryColumn "title" Schema.TStr,
                Schema.nativeQueryColumn "excerpt" Schema.TStr,
                Schema.nativeQueryColumn "date" Schema.TUTCTime
              ]
          }

      articleWithExcerptNativeQuery :: Text -> Schema.SchemaName -> Schema.NativeQuery
      articleWithExcerptNativeQuery name schemaName =
        (Schema.nativeQuery name (articleQuery schemaName) "article_with_excerpt")
          { Schema.nativeQueryArguments =
              [ Schema.nativeQueryColumn "length" Schema.TInt
              ]
          }

  describe "Testing Native Queries" $ do
    it "Runs a simple query that takes one parameter and uses it multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      Schema.trackCustomType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery source (articleWithExcerptNativeQuery "article_with_excerpt" schemaName) testEnvironment

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
                      date: "00:00:00"
                      excerpt: "I like to eat dog food I am a dogs..."
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Uses two queries with the same argument names and ensure they don't mess with one another" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      Schema.trackCustomType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery
        source
        (articleWithExcerptNativeQuery "article_with_excerpt_1" schemaName)
        testEnvironment

      Schema.trackNativeQuery
        source
        (articleWithExcerptNativeQuery "article_with_excerpt_2" schemaName)
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
                article_with_excerpt_2(args: { length: 13 }) {
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

      shouldReturnYaml testEnvironment actual expected

    it "Uses the same one parameter query multiple times" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      Schema.trackCustomType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery source (articleWithExcerptNativeQuery "article_with_excerpt" schemaName) testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Uses a one parameter query, passing it a GraphQL variable" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata
          schemaName = Schema.getSchemaName testEnvironment

      Schema.trackCustomType source articleWithExcerptReturnType testEnvironment

      Schema.trackNativeQuery source (articleWithExcerptNativeQuery "article_with_excerpt" schemaName) testEnvironment

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

      shouldReturnYaml testEnvironment actual expected

    it "Runs a query that uses a built-in Stored Procedure" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

          goodQuery = "EXEC sp_databases"

          storedProcedureReturnType :: Schema.CustomType
          storedProcedureReturnType =
            (Schema.customType "stored_procedure")
              { Schema.customTypeColumns =
                  [ Schema.nativeQueryColumn "database_name" Schema.TStr,
                    Schema.nativeQueryColumn "database_size" Schema.TInt,
                    Schema.nativeQueryColumn "remarks" Schema.TStr
                  ]
              }

          useStoredProcedure :: Schema.NativeQuery
          useStoredProcedure =
            (Schema.nativeQuery "use_stored_procedure" goodQuery "stored_procedure")

      Schema.trackCustomType source storedProcedureReturnType testEnvironment

      Schema.trackNativeQuery source useStoredProcedure testEnvironment

      -- making an assumption here that an SQLServer instance will always have
      -- a `master` database
      let expected =
            [yaml|
                data:
                  use_stored_procedure:
                    - database_name: "master"
              |]

      actual <-
        GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
              query {
                use_stored_procedure {
                  database_name
                }
              }
           |]

      actual `shouldAtLeastBe` expected
