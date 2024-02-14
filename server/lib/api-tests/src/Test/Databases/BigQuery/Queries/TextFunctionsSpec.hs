{-# LANGUAGE QuasiQuotes #-}

-- | Test text search functions in BigQuery
module Test.Databases.BigQuery.Queries.TextFunctionsSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "languages")
      { Schema.tableColumns =
          [ Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = [],
        Schema.tableData =
          [ [Schema.VStr "Python"],
            [Schema.VStr "C"],
            [Schema.VStr "C++"],
            [Schema.VStr "Java"],
            [Schema.VStr "C#"],
            [Schema.VStr "JavaScript"],
            [Schema.VStr "PHP"],
            [Schema.VStr "Visual Basic"],
            [Schema.VStr "SQL"],
            [Schema.VStr "Scratch"],
            [Schema.VStr "Go"],
            [Schema.VStr "Fortran"],
            [Schema.VStr "Delphi"],
            [Schema.VStr "MATLAB"],
            [Schema.VStr "Assembly"],
            [Schema.VStr "Swift"],
            [Schema.VStr "Kotlin"],
            [Schema.VStr "Ruby"],
            [Schema.VStr "Rust"],
            [Schema.VStr "COBOL"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Text predicates" do
    it "ilike" \testEnvironment -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_languages:
                - name: Assembly
                - name: Fortran
                - name: Java
                - name: JavaScript
                - name: MATLAB
                - name: Scratch
                - name: Visual Basic
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_languages (
                    order_by: { name: asc },
                    where: { name: { _ilike: "%a%" } }
                  ) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "like" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_languages:
                - name: Fortran
                - name: Java
                - name: JavaScript
                - name: Scratch
                - name: Visual Basic
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_languages (
                    order_by: { name: asc },
                    where: { name: { _like: "%a%" } }
                  ) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "nlike" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_languages:
                - name: Assembly
                - name: C
                - name: C#
                - name: C++
                - name: COBOL
                - name: Delphi
                - name: Go
                - name: Kotlin
                - name: MATLAB
                - name: PHP
                - name: Python
                - name: Ruby
                - name: Rust
                - name: SQL
                - name: Swift
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_languages (
                    order_by: { name: asc },
                    where: { name: { _nlike: "%a%" } }
                  ) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "nilike" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_languages:
                  - name: C
                  - name: C#
                  - name: C++
                  - name: COBOL
                  - name: Delphi
                  - name: Go
                  - name: Kotlin
                  - name: PHP
                  - name: Python
                  - name: Ruby
                  - name: Rust
                  - name: SQL
                  - name: Swift
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_languages (
                    order_by: { name: asc },
                    where: { name: { _nilike: "%a%" } }
                  ) {
                    name
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
