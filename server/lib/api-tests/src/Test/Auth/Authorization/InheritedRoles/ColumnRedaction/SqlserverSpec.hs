module Test.Auth.Authorization.InheritedRoles.ColumnRedaction.SqlserverSpec
  ( spec,
  )
where

import Data.Aeson (Value (String), object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (InheritedRoleDetails (..), Permission (..), SelectPermissionDetails (..), selectPermission)
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (setupPermissionsAction)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv,
                  setupPermissionsAction permissions testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [manager, employee]

manager :: Schema.Table
manager =
  (table "manager")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "first_name" Schema.TStr,
          Schema.column "last_name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "Ryan", Schema.VStr "Ray"],
          [Schema.VInt 2, Schema.VStr "Martin", Schema.VStr "Graham"],
          [Schema.VInt 3, Schema.VStr "Althea", Schema.VStr "Weiss"],
          [Schema.VInt 4, Schema.VStr "Bec", Schema.VStr "Vo"]
        ]
    }

employee :: Schema.Table
employee =
  (table "employee")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "first_name" Schema.TStr,
          Schema.column "last_name" Schema.TStr,
          Schema.column "nationality" Schema.TStr,
          Schema.column "monthly_salary" Schema.TInt,
          Schema.column "engineering_manager_id" Schema.TInt,
          Schema.column "hr_manager_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableReferences =
        [ (Schema.reference "engineering_manager_id" "manager" "id")
            { Schema.referenceCascade = False
            },
          (Schema.reference "hr_manager_id" "manager" "id")
            { Schema.referenceCascade = False
            }
        ],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "David", Schema.VStr "Holden", Schema.VStr "Australian", Schema.VInt 5000, Schema.VInt 1, Schema.VInt 3],
          [Schema.VInt 2, Schema.VStr "Grant", Schema.VStr "Smith", Schema.VStr "Australian", Schema.VInt 6000, Schema.VInt 1, Schema.VInt 4],
          [Schema.VInt 3, Schema.VStr "Xin", Schema.VStr "Cheng", Schema.VStr "Chinese", Schema.VInt 5500, Schema.VInt 2, Schema.VInt 3],
          [Schema.VInt 4, Schema.VStr "Sarah", Schema.VStr "Smith", Schema.VStr "British", Schema.VInt 4000, Schema.VInt 2, Schema.VInt 4]
        ]
    }

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
        },
    SelectPermission
      selectPermission
        { selectPermissionTable = "manager",
          selectPermissionRole = "all_managers",
          selectPermissionColumns = ["id", "first_name", "last_name"],
          selectPermissionAllowAggregations = True,
          selectPermissionRows = object []
        },
    SelectPermission
      selectPermission
        { selectPermissionTable = "employee",
          selectPermissionRole = "all_managers",
          selectPermissionColumns = ["id", "first_name", "last_name", "engineering_manager_id", "hr_manager_id"],
          selectPermissionAllowAggregations = True,
          selectPermissionRows = object []
        },
    SelectPermission
      selectPermission
        { selectPermissionTable = "employee",
          selectPermissionRole = "manager_employee_private_info",
          selectPermissionColumns = ["id", "first_name", "last_name", "nationality", "monthly_salary", "engineering_manager_id", "hr_manager_id"],
          selectPermissionAllowAggregations = True,
          selectPermissionRows =
            object
              [ "hr_manager_id" .= String "X-Hasura-Manager-Id"
              ]
        },
    InheritedRole
      InheritedRoleDetails
        { inheritedRoleName = "hr_manager",
          inheritedRoleRoleSet = ["all_managers", "manager_employee_private_info"]
        }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Redaction in column selection sets" $ do
    it "Check redaction in regular queries" \testEnvironment -> do
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
                - id: 2
                  first_name: Grant
                  last_name: Smith
                  monthly_salary: null
                - id: 3
                  first_name: Xin
                  last_name: Cheng
                  monthly_salary: 5500
                - id: 4
                  first_name: Sarah
                  last_name: Smith
                  monthly_salary: null
            |]

      shouldReturnYaml testEnvironment actual expected

    it "Check column redaction in nodes in aggregate queries" \testEnvironment -> do
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
                  - id: 2
                    first_name: Grant
                    last_name: Smith
                    monthly_salary: null
                  - id: 3
                    first_name: Xin
                    last_name: Cheng
                    monthly_salary: 5500
                  - id: 4
                    first_name: Sarah
                    last_name: Smith
                    monthly_salary: null
            |]

      shouldReturnYaml testEnvironment actual expected

  describe "Redaction in aggregation calculations" $ do
    it "Check redaction of input values to aggregation functions" \testEnvironment -> do
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
                      }
                    }
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to the monthly_salary column,
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
            |]

      shouldReturnYaml testEnvironment actual expected

    it "Check redaction of input values to count aggregations that use columns" \testEnvironment -> do
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
                      count_distinct_salary: count(distinct: true, column: monthly_salary)
                      count_not_distinct_salary: count(distinct: false, column: monthly_salary)
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

  describe "Redaction in ordering" $ do
    it "ordering by column is applied over redacted column value" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "employee"),
                ("X-Hasura-Employee-Id", "3")
              ]
              -- Note that this test differs slightly from its Postgres counterpart, since
              -- the MSSQL backend defaults to sorting nulls last.
              [graphql|
                query {
                  #{schemaName}_employee(order_by: [{ monthly_salary: desc_nulls_first }, {id: desc}]) {
                    id
                    first_name
                    last_name
                    monthly_salary
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to
          -- the monthly_salary column, but the 'employee_private_info' role
          -- does, but only for the current employee's record (ie. hers).
          -- This means when she orders by monthly salary, the ordering
          -- should not know the value of any salary other than hers and therefore
          -- should fall back to order by the id since all other salaries should
          -- appear as null.
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_employee:
                - id: 4
                  first_name: Sarah
                  last_name: Smith
                  monthly_salary: null
                - id: 2
                  first_name: Grant
                  last_name: Smith
                  monthly_salary: null
                - id: 1
                  first_name: David
                  last_name: Holden
                  monthly_salary: null
                - id: 3
                  first_name: Xin
                  last_name: Cheng
                  monthly_salary: 5500
            |]

      shouldReturnYaml testEnvironment actual expected

    it "ordering by aggregate is applied over the aggregate over the redacted column value" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
          actual :: IO Value
          actual =
            GraphqlEngine.postGraphqlWithHeaders
              testEnvironment
              [ ("X-Hasura-Role", "hr_manager"),
                ("X-Hasura-Manager-Id", "3")
              ]
              -- Note that this test differs slightly from its Postgres counterpart, since
              -- the MSSQL backend defaults to sorting nulls last.
              [graphql|
                query {
                  #{schemaName}_manager(order_by: [{employees_by_id_to_engineering_manager_id_aggregate: { sum: { monthly_salary: desc_nulls_first } }}, {id: asc}]) {
                    id
                    first_name
                    last_name
                    employees_by_id_to_engineering_manager_id_aggregate {
                      aggregate {
                        sum {
                          monthly_salary
                        }
                      }
                    }
                  }
                }
              |]

          -- Althea Weiss can only see the salaries of the employees she is HR manager for.
          -- This is because the 'manager_employee_private_info' role provides access to the salary
          -- for the current manager's HR-managed employees, but the rest of the employees
          -- are accessed via 'all_managers', which does not expose 'monthly_salary'.
          -- So when Althea orders all managers by the sum of the salary of the employees they
          -- are the _engineering manager_ for, she should only be ordering them by
          -- aggregate of the salaries she can see.
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_manager:
                - id: 3
                  first_name: Althea
                  last_name: Weiss
                  employees_by_id_to_engineering_manager_id_aggregate:
                    aggregate:
                      sum:
                        monthly_salary: null
                - id: 4
                  first_name: Bec
                  last_name: Vo
                  employees_by_id_to_engineering_manager_id_aggregate:
                    aggregate:
                      sum:
                        monthly_salary: null
                - id: 2
                  first_name: Martin
                  last_name: Graham
                  employees_by_id_to_engineering_manager_id_aggregate:
                    aggregate:
                      sum:
                        monthly_salary: 5500
                - id: 1
                  first_name: Ryan
                  last_name: Ray
                  employees_by_id_to_engineering_manager_id_aggregate:
                    aggregate:
                      sum:
                        monthly_salary: 5000
            |]

      shouldReturnYaml testEnvironment actual expected

  describe "Redaction in filtering" $ do
    it "filtering by column is applied against redacted column value" \testEnvironment -> do
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
                  #{schemaName}_employee(where: { monthly_salary: { _eq: 5000 } }) {
                    id
                  }
                }
              |]

          -- Xin Cheng can see her own salary, but not her peers' because the
          -- 'employee_public_info' role does not provide access to
          -- the monthly_salary column, but the 'employee_private_info' role
          -- does, but only for the current employee's record (ie. hers).
          -- This means she should not be able to compare against salaries
          -- she does not have access to, such as David Holden's salary
          expected :: Value
          expected =
            [interpolateYaml|
              data:
                #{schemaName}_employee: []
            |]

      shouldReturnYaml testEnvironment actual expected
