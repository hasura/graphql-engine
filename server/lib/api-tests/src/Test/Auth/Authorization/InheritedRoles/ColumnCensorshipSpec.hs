module Test.Auth.Authorization.InheritedRoles.ColumnCensorshipSpec
  ( spec,
  )
where

import Data.Aeson (Value (String), object, (.=))
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (InheritedRoleDetails (..), Permission (..), SelectPermissionDetails (..), selectPermission)
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (setupPermissionsAction)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                Postgres.setupTablesAction schema testEnv
                  : computedFieldSetupActions testEnv
                    <> [setupPermissionsAction permissions testEnv]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [employee]

employee :: Schema.Table
employee =
  (table "employee")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "first_name" Schema.TStr,
          Schema.column "last_name" Schema.TStr,
          Schema.column "monthly_salary" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "David", Schema.VStr "Holden", Schema.VInt 5000],
          [Schema.VInt 2, Schema.VStr "Grant", Schema.VStr "Smith", Schema.VInt 6000],
          [Schema.VInt 3, Schema.VStr "Xin", Schema.VStr "Cheng", Schema.VInt 5500],
          [Schema.VInt 4, Schema.VStr "Sarah", Schema.VStr "Smith", Schema.VInt 6100]
        ]
    }

computedFieldSetupActions :: TestEnvironment -> [Fixture.SetupAction]
computedFieldSetupActions testEnv =
  let schemaName = Schema.getSchemaName testEnv
      backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
      source = Fixture.backendSourceName backendTypeMetadata
   in [ Fixture.SetupAction
          { Fixture.setupAction =
              Postgres.run_ testEnv
                $ [i|
                  CREATE FUNCTION #{ employee_yearly_salary schemaName }(employee_row employee)
                  RETURNS integer AS $$
                    SELECT employee_row.monthly_salary * 12
                  $$ LANGUAGE sql STABLE;
                |],
            Fixture.teardownAction = \_ -> pure ()
          },
        Fixture.SetupAction
          { Fixture.setupAction =
              Schema.trackComputedField
                source
                employee
                "employee_yearly_salary"
                "yearly_salary"
                [yaml| null |]
                [yaml| null |]
                testEnv,
            Fixture.teardownAction = \_ -> pure ()
          }
      ]

employee_yearly_salary :: Schema.SchemaName -> Text
employee_yearly_salary (Schema.SchemaName name) = name <> ".employee_yearly_salary"

--------------------------------------------------------------------------------
-- Permissions

permissions :: [Permission]
permissions =
  [ SelectPermission
      selectPermission
        { selectPermissionTable = "employee",
          selectPermissionRole = "employee_public_info",
          selectPermissionColumns = ["id", "first_name", "last_name"],
          selectPermissionAllowAggregations = True,
          selectPermissionRows = object []
        },
    SelectPermission
      selectPermission
        { selectPermissionTable = "employee",
          selectPermissionRole = "employee_private_info",
          selectPermissionColumns = ["id", "first_name", "last_name", "monthly_salary"],
          selectPermissionComputedFields = ["yearly_salary"],
          selectPermissionAllowAggregations = True,
          selectPermissionRows =
            object
              [ "id" .= String "X-Hasura-Employee-Id"
              ]
        },
    InheritedRole
      InheritedRoleDetails
        { inheritedRoleName = "employee",
          inheritedRoleRoleSet = ["employee_public_info", "employee_private_info"]
        }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Censorship in column selection sets" $ do
    it "Check censorship in regular queries" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "employee"),
                ("X-Hasura-Employee-Id", "3")
              ]
              [graphql|
                query {
                  #{schemaName}_employee(order_by: { id: asc }) {
                    id
                    first_name
                    last_name
                    monthly_salary
                    yearly_salary
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to
          -- the monthly_salary column & yearly_salary computed field,
          -- but the 'employee_private_info' role does, but only for the current
          -- employee's record (ie. hers)
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_employee:
                - id: 1
                  first_name: David
                  last_name: Holden
                  monthly_salary: null
                  yearly_salary: null
                - id: 2
                  first_name: Grant
                  last_name: Smith
                  monthly_salary: null
                  yearly_salary: null
                - id: 3
                  first_name: Xin
                  last_name: Cheng
                  monthly_salary: 5500
                  yearly_salary: 66000
                - id: 4
                  first_name: Sarah
                  last_name: Smith
                  monthly_salary: null
                  yearly_salary: null
            |]

      shouldReturnYaml testEnvironment actual expected

    it "Check column censorship in nodes in aggregate queries" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "employee"),
                ("X-Hasura-Employee-Id", "3")
              ]
              [graphql|
                query {
                  #{schemaName}_employee_aggregate(order_by: { id: asc }) {
                    nodes {
                      id
                      first_name
                      last_name
                      monthly_salary
                      yearly_salary
                    }
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to
          -- the monthly_salary column & yearly_salary computed field,
          -- but the 'employee_private_info' role does, but only for the current
          -- employee's record (ie. hers)
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_employee_aggregate:
                  nodes:
                  - id: 1
                    first_name: David
                    last_name: Holden
                    monthly_salary: null
                    yearly_salary: null
                  - id: 2
                    first_name: Grant
                    last_name: Smith
                    monthly_salary: null
                    yearly_salary: null
                  - id: 3
                    first_name: Xin
                    last_name: Cheng
                    monthly_salary: 5500
                    yearly_salary: 66000
                  - id: 4
                    first_name: Sarah
                    last_name: Smith
                    monthly_salary: null
                    yearly_salary: null
            |]

      shouldReturnYaml testEnvironment actual expected

  describe "Censorship in aggregation calculations" $ do
    it "Check censorship of input values to aggregation functions" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "employee"),
                ("X-Hasura-Employee-Id", "3")
              ]
              [graphql|
                query {
                  #{schemaName}_employee_aggregate {
                    aggregate {
                      count
                      sum {
                        monthly_salary
                        yearly_salary
                      }
                    }
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to
          -- the monthly_salary column & yearly_salary computed field,
          -- but the 'employee_private_info' role does, but only for the current
          -- employee's record (ie. hers)
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_employee_aggregate:
                  aggregate:
                    count: 4
                    sum:
                      monthly_salary: 5500
                      yearly_salary: 66000
            |]

      shouldReturnYaml testEnvironment actual expected

    it "Check censorship of input values to count aggregations that use columns" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "employee"),
                ("X-Hasura-Employee-Id", "3")
              ]
              [graphql|
                query {
                  #{schemaName}_employee_aggregate {
                    aggregate {
                      count
                      count_distinct_salary: count(distinct: true, columns:[monthly_salary])
                      count_not_distinct_salary: count(distinct: false, columns:[monthly_salary])
                    }
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to
          -- the monthly_salary column & yearly_salary computed field,
          -- but the 'employee_private_info' role does, but only for the current
          -- employee's record (ie. hers)
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_employee_aggregate:
                  aggregate:
                    count: 4
                    count_distinct_salary: 1
                    count_not_distinct_salary: 1
            |]

      shouldReturnYaml testEnvironment actual expected
