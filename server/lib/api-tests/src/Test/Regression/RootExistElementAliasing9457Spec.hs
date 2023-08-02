-- |
-- https://github.com/hasura/graphql-engine/issues/9446
module Test.Regression.RootExistElementAliasing9457Spec where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphqlWithHeaders, postMetadata_)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

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
    tests

schema :: [Schema.Table]
schema =
  [ (table "organization")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "group" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VInt 1],
            [Schema.VInt 2, Schema.VInt 1],
            [Schema.VInt 3, Schema.VInt 2],
            [Schema.VInt 4, Schema.VInt 2]
          ]
      },
    (table "user")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Alice"],
            [Schema.VInt 2, Schema.VStr "Bob"],
            [Schema.VInt 3, Schema.VStr "Charlie"],
            [Schema.VInt 4, Schema.VStr "Dave"]
          ]
      },
    (table "organization_member")
      { tableColumns =
          [ Schema.column "organization_id" Schema.TInt,
            Schema.column "user_id" Schema.TInt
          ],
        tableReferences =
          [ Schema.reference "organization_id" "organization" "id",
            Schema.reference "user_id" "user" "id"
          ],
        tableData =
          [ [Schema.VInt 1, Schema.VInt 1],
            [Schema.VInt 2, Schema.VInt 2],
            [Schema.VInt 3, Schema.VInt 3],
            [Schema.VInt 4, Schema.VInt 4]
          ]
      },
    (table "org_stuff")
      { tableColumns =
          [ Schema.column "organization_id" Schema.TInt,
            Schema.column "group" Schema.TInt,
            Schema.column "data" Schema.TStr,
            Schema.column "public" Schema.TBool
          ],
        tableReferences =
          [ Schema.reference "organization_id" "organization" "id"
          ],
        tableData =
          [ [Schema.VInt 1, Schema.VInt 1, Schema.VStr "First data", Schema.VBool True],
            [Schema.VInt 2, Schema.VInt 2, Schema.VStr "Second data", Schema.VBool True],
            [Schema.VInt 3, Schema.VInt 3, Schema.VStr "Third data", Schema.VBool True],
            [Schema.VInt 4, Schema.VInt 4, Schema.VStr "Fourth data", Schema.VBool False]
          ]
      }
  ]

tests :: SpecWith TestEnvironment
tests = do
  it "Hasn't broken regular EXISTS permissions" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        backendTypeMetadata :: BackendType.BackendTypeConfig
        backendTypeMetadata = fromMaybe (error "Unknown backend") do
          getBackendTypeConfig testEnvironment

        source :: String
        source = BackendType.backendSourceName backendTypeMetadata

    postMetadata_
      testEnvironment
      [interpolateYaml|
        type: pg_create_select_permission
        args:
          source: #{source}
          table:
            name: user
            schema: #{schemaName}
          role: "test-role-1"
          permission:
            columns: "*"
            filter:
              _exists:
                _table:
                  name: user
                  schema: #{schemaName}
                _where:
                  id:
                    _eq: X-Hasura-User-Id
        |]

    shouldReturnYaml
      testEnvironment
      ( postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", "test-role-1"),
            ("X-Hasura-User-Id", "1")
          ]
          [graphql|
            query {
              #{schemaName}_user {
                name
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_user:
            - name: Alice
            - name: Bob
            - name: Charlie
            - name: Dave
      |]

    shouldReturnYaml
      testEnvironment
      ( postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", "test-role-1"),
            ("X-Hasura-User-Id", "10")
          ]
          [graphql|
            query {
              #{schemaName}_user {
                name
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_user: []
      |]

  it "Returns the correct rows according to the permissions" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        backendTypeMetadata :: BackendType.BackendTypeConfig
        backendTypeMetadata = fromMaybe (error "Unknown backend") do
          getBackendTypeConfig testEnvironment

        source :: String
        source = BackendType.backendSourceName backendTypeMetadata

    -- Each user is in an organization, each organization has a group. A user
    -- can see any row whose group doesn't match one of their organizations.

    postMetadata_
      testEnvironment
      [interpolateYaml|
        type: pg_create_select_permission
        args:
          source: #{source}
          table:
            name: org_stuff
            schema: #{schemaName}
          role: "test-role-2"
          permission:
            columns: "*"
            filter:
              _and:
              - public:
                  _eq: true
              - _not:
                  _exists:
                    _table:
                      name: org_stuff
                      schema: #{schemaName}
                    _where:
                      _and:
                        - organization_by_organization_id_to_id:
                            organization_members_by_id_to_organization_id:
                              user_id:
                                _eq: X-Hasura-User-Id
                        - group:
                            _ceq:
                            - $
                            - group
        |]

    -- User 1 is a member of organization 1, whose group is 1. This means that
    -- we should only see public rows whose group is NOT 1. Row 4 is not
    -- public, and row 1 is in group 1, so we see groups 2 and 3.

    shouldReturnYaml
      testEnvironment
      ( postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", "test-role-2"),
            ("X-Hasura-User-Id", "1")
          ]
          [graphql|
            query {
              #{schemaName}_org_stuff {
                data
              }
            }
          |]
      )
      [interpolateYaml|
        data:
          #{schemaName}_org_stuff:
            - data: "Second data"
            - data: "Third data"
      |]
