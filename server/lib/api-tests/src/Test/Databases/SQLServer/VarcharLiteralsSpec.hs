{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests around varchar literals in where clauses
module Test.Databases.SQLServer.VarcharLiteralsSpec (spec) where

import Data.Aeson (Value, (.:))
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Vector (Vector, (!?))
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postExplain, postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it, shouldSatisfy)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "test_string_compare")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "varchar" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "varchar(max)"
                  },
            Schema.column "nvarchar" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "nvarchar(max)"
                  },
            Schema.column "char" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "char(100)"
                  },
            Schema.column "nchar" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "nchar(100)"
                  }
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "horse",
              Schema.VStr "horse",
              Schema.VStr "horse",
              Schema.VStr "horse"
            ],
            [ Schema.VInt 2,
              Schema.VStr "££££",
              Schema.VStr "££££",
              Schema.VStr "££££",
              Schema.VStr "££££"
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  let sqlShouldSatisfy :: IO Value -> (Text -> Bool) -> IO ()
      sqlShouldSatisfy ioVal predicate = do
        value <- ioVal

        let parseVectorHead :: Vector a -> J.Parser a
            parseVectorHead as = case as !? 0 of
              Just a -> pure a
              _ -> J.parseFail "expected non-empty array"

        -- return type is [{sql: <text_we_want> }]
        let parseSqlString :: Value -> J.Parser Text
            parseSqlString =
              J.withArray
                "explain"
                ( parseVectorHead
                    >=> J.withObject "firstItem" (\o -> o .: "sql")
                    >=> J.withText "sqlText" pure
                )

        case J.parseEither parseSqlString value of
          Right sql -> sql `shouldSatisfy` predicate
          Left e -> error $ show e

  it "Query comparing string with varchar" \testEnvironment -> do
    let expected =
          [yaml|
                data:
                  hasura_test_string_compare:
                    - id: 1
              |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {varchar: {_eq: "horse"}}
              ) {
                id
              }
            }
           |]

    shouldReturnYaml testEnvironment actual expected

  it "Query comparing string with char" \testEnvironment -> do
    let expected =
          [yaml|
                data:
                  hasura_test_string_compare:
                    - id: 1
              |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {char: {_eq: "horse"}}
              ) {
                id
              }
            }
           |]

    shouldReturnYaml testEnvironment actual expected

  it "Query comparing string with nvarchar" \testEnvironment -> do
    let expected =
          [yaml|
                data:
                  hasura_test_string_compare:
                    - id: 1
              |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {nvarchar: {_eq: "horse"}}
              ) {
                id
              }
            }
           |]

    shouldReturnYaml testEnvironment actual expected

  it "Query comparing string with nchar" \testEnvironment -> do
    let expected =
          [yaml|
                data:
                  hasura_test_string_compare:
                    - id: 1
              |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {nchar: {_eq: "horse"}}
              ) {
                id
              }
            }
           |]

    shouldReturnYaml testEnvironment actual expected

  it "Query comparing non-ascii string with varchar" \testEnvironment -> do
    let expected =
          [yaml|
                data:
                  hasura_test_string_compare:
                    - id: 2
              |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {varchar: {_eq: "££££"}}
              ) {
                id
              }
            }
           |]

    shouldReturnYaml testEnvironment actual expected

  it "Query comparing non-ascii string with nvarchar" \testEnvironment -> do
    let expected =
          [yaml|
                data:
                  hasura_test_string_compare:
                    - id: 2
              |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {nvarchar: {_eq: "££££"}}
              ) {
                id
              }
            }
           |]

    shouldReturnYaml testEnvironment actual expected

  it "Explain comparing string with varchar" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postExplain
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {varchar: {_eq: "horse"}}
              ) {
                varchar
                nvarchar
              }
            }
           |]

    actual
      `sqlShouldSatisfy` ( \sql ->
                             "'horse'"
                               `T.isInfixOf` sql
                               && not ("N'horse'" `T.isInfixOf` sql)
                         )

  it "Explain comparing string with char" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postExplain
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {char: {_eq: "horse"}}
              ) {
                char
                nchar
              }
            }
           |]

    actual
      `sqlShouldSatisfy` ( \sql ->
                             "'horse'"
                               `T.isInfixOf` sql
                               && not ("N'horse'" `T.isInfixOf` sql)
                         )

  it "Explain comparing string with nvarchar" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postExplain
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {nvarchar: {_eq: "horse"}}
              ) {
                varchar
                nvarchar
              }
            }
           |]

    actual `sqlShouldSatisfy` ("N'horse'" `T.isInfixOf`)

  it "Explain comparing string with nchar" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postExplain
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {nchar: {_eq: "horse"}}
              ) {
                char
                nchar
              }
            }
           |]

    actual `sqlShouldSatisfy` ("N'horse'" `T.isInfixOf`)

  it "Literal is still nvarchar on varchar column when it contains non-ascii characters" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postExplain
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {varchar: {_eq: "££££"}}
              ) {
                varchar
                nvarchar
              }
            }
           |]

    let expected = "NCHAR(163)+''+NCHAR(163)+''+NCHAR(163)+''+NCHAR(163)"

    actual `sqlShouldSatisfy` (\sql -> expected `T.isInfixOf` sql)

  it "Literal is still nchar on char column when it contains non-ascii characters" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postExplain
            testEnvironment
            [graphql|
            query {
              hasura_test_string_compare(
                where: {char: {_eq: "££££"}}
              ) {
                char
                nchar
              }
            }
           |]

    let expected = "NCHAR(163)+''+NCHAR(163)+''+NCHAR(163)+''+NCHAR(163)"

    actual `sqlShouldSatisfy` (\sql -> expected `T.isInfixOf` sql)
