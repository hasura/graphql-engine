{-# LANGUAGE QuasiQuotes #-}

-- Test that references to root tables in permissions ('$') are set correctly
-- for queries with where-clauses that traverse relationships.
module Test.Queries.Relationship.PermissionRootReferenceSpec (spec) where

import Data.Aeson (Value)
import Data.Has
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Permissions (Permission (..), SelectPermissionDetails (..), selectPermission)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema
import Harness.Services.GraphqlEngine
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment)
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    ( Fixture.withPermissions
        ( NE.fromList
            [ SelectPermission
                selectPermission
                  { selectPermissionTable = "author",
                    selectPermissionColumns = ["author_id", "name"],
                    selectPermissionRows =
                      [interpolateYaml|
                        _exists:
                          _table:
                            name: author_access
                            schema: hasura
                          _where:
                            _and:
                            - author_access_author_id:
                                _ceq:
                                - $
                                - author_id
                        |]
                  },
              SelectPermission
                selectPermission
                  { selectPermissionTable = "article",
                    selectPermissionColumns = ["article_id", "article_author_id", "name"]
                  }
            ]
        )
        tests
    )

--------------------------------------------------------------------------------
-- Schema

schema :: [Table]
schema =
  [ (table "author_access")
      { tableColumns =
          [ column "author_access_author_id" TInt
          ],
        tableData =
          [ [ VInt 1
            ]
          ]
      },
    (table "author")
      { tableColumns =
          [ column "author_id" TInt,
            column "name" TStr
          ],
        tablePrimaryKey = ["author_id"],
        tableData =
          [ [ VInt 1,
              VStr "Author 1"
            ]
          ]
      },
    (table "article")
      { tableColumns =
          [ column "article_id" TInt,
            column "article_author_id" TInt,
            column "name" TStr
          ],
        tablePrimaryKey = ["article_id"],
        tableData =
          [ [ VInt 1,
              VInt 1,
              VStr "Article 1"
            ]
          ],
        tableReferences = [reference "article_author_id" "author" "author_id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests ::
  forall testEnvironment.
  ( Has PostGraphql testEnvironment,
    Has ShouldReturnYamlF testEnvironment,
    Has SchemaName testEnvironment
  ) =>
  SpecWith testEnvironment
tests = do
  describe "Querying an object relationship translate permissions correctly" do
    let expected :: SchemaName -> Value
        expected schemaName =
          [interpolateYaml|
              data:
                #{schemaName}_article:
                - author_by_article_author_id_to_author_id:
                    author_id: 1
                    name: Author 1
                  article_id: 1
            |]

    it "without any `_where` predicates" \testEnvironment -> do
      let schemaName = getter @SchemaName testEnvironment
          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article {
                    article_id
                    author_by_article_author_id_to_author_id {
                      author_id
                      name
                      }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual (expected schemaName)

    it "with a `_where` predicate" \testEnvironment -> do
      let schemaName = getter @SchemaName testEnvironment
          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_article(where: {author_by_article_author_id_to_author_id: {author_id: {_is_null: false}}})  {
                    article_id
                    author_by_article_author_id_to_author_id {
                      author_id
                      name
                      }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual (expected schemaName)
