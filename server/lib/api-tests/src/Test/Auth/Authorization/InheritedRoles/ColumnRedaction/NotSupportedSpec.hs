module Test.Auth.Authorization.InheritedRoles.ColumnRedaction.NotSupportedSpec
  ( spec,
  )
where

import Data.Aeson (Value (String), object, (.=))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (InheritedRoleDetails (..), LogicalModelSelectPermissionDetails (..), Permission (..), SelectPermissionDetails (..), createPermissionMetadata, logicalModelSelectPermission, selectPermission)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (setupPermissionsAction)
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlite.setupTablesAction schema testEnv,
                  logicalModelsSetupAction testEnv,
                  setupPermissionsAction permissions testEnv
                ]
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
          Schema.column "nationality" Schema.TStr,
          Schema.column "monthly_salary" Schema.TDouble
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "David", Schema.VStr "Holden", Schema.VStr "Australian", Schema.VDouble 5000],
          [Schema.VInt 2, Schema.VStr "Grant", Schema.VStr "Smith", Schema.VStr "Australian", Schema.VDouble 6000],
          [Schema.VInt 3, Schema.VStr "Xin", Schema.VStr "Cheng", Schema.VStr "Chinese", Schema.VDouble 5500],
          [Schema.VInt 4, Schema.VStr "Sarah", Schema.VStr "Smith", Schema.VStr "British", Schema.VDouble 4000]
        ]
    }

logicalEmployee :: Schema.LogicalModel
logicalEmployee =
  (Schema.logicalModel "logical_employee")
    { Schema.logicalModelColumns =
        [ Schema.logicalModelScalar "id" Schema.TInt,
          Schema.logicalModelScalar "first_name" Schema.TStr,
          Schema.logicalModelScalar "last_name" Schema.TStr,
          Schema.logicalModelScalar "nationality" Schema.TStr,
          Schema.logicalModelScalar "monthly_salary" Schema.TDouble,
          Schema.logicalModelScalar "engineering_manager_id" Schema.TInt,
          Schema.logicalModelScalar "hr_manager_id" Schema.TInt
        ]
    }

logicalModelsSetupAction :: TestEnvironment -> Fixture.SetupAction
logicalModelsSetupAction testEnv =
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnv
      source = Fixture.backendSourceName backendTypeMetadata
      setupAction = do
        Schema.trackLogicalModel source logicalEmployee testEnv
   in Fixture.SetupAction
        { Fixture.setupAction = setupAction,
          Fixture.teardownAction = \_ -> pure ()
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
    LogicalModelSelectPermission
      logicalModelSelectPermission
        { lmSelectPermissionName = "logical_employee",
          lmSelectPermissionRole = "logical_employee_public_info",
          lmSelectPermissionColumns = ["id", "first_name", "last_name"],
          lmSelectPermissionFilter = object []
        },
    LogicalModelSelectPermission
      logicalModelSelectPermission
        { lmSelectPermissionName = "logical_employee",
          lmSelectPermissionRole = "logical_employee_private_info",
          lmSelectPermissionColumns = ["id", "first_name", "last_name", "monthly_salary"],
          lmSelectPermissionFilter =
            object
              [ "id" .= String "X-Hasura-Employee-Id"
              ]
        }
  ]

tests :: SpecWith TestEnvironment
tests = do
  it "Redaction is not supported for tables" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    let source = Fixture.backendSourceName backendTypeMetadata
    let schemaName = Schema.getSchemaName testEnvironment
    let actual =
          GraphqlEngine.postMetadataWithStatus 400 testEnvironment
            $ createPermissionMetadata testEnvironment
            $ InheritedRole
              InheritedRoleDetails
                { inheritedRoleName = "employee",
                  inheritedRoleRoleSet = ["employee_public_info", "employee_private_info"]
                }

    let expected =
          [interpolateYaml|
            code: invalid-configuration
            error:
              'Inconsistent object: The source "#{source}" does not support inherited roles
              where the columns allowed are different in the column select permissions in the
              parent roles. This occurs for the column select permissions defined for the table
              "#{schemaName}.employee" for the inherited role "employee", which combines the roles "employee_public_info",
              "employee_private_info"'
            internal:
            - definition:
                role_name: employee
                role_set:
                - employee_public_info
                - employee_private_info
              name: inherited_role inherited role employee
              reason:
                'Inconsistent object: The source "#{source}" does not support inherited roles
                where the columns allowed are different in the column select permissions in the
                parent roles. This occurs for the column select permissions defined for the table
                "#{schemaName}.employee" for the inherited role "employee", which combines the roles "employee_public_info",
                "employee_private_info"'
              type: inherited_role
            path: $.args
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Redaction is not supported for logical models" \testEnvironment -> do
    let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
    let source = Fixture.backendSourceName backendTypeMetadata
    let actual =
          GraphqlEngine.postMetadataWithStatus 400 testEnvironment
            $ createPermissionMetadata testEnvironment
            $ InheritedRole
              InheritedRoleDetails
                { inheritedRoleName = "employee",
                  inheritedRoleRoleSet = ["logical_employee_public_info", "logical_employee_private_info"]
                }

    let expected =
          [interpolateYaml|
            code: invalid-configuration
            error:
              'Inconsistent object: The source "#{source}" does not support inherited roles
              where the columns allowed are different in the column select permissions in the
              parent roles. This occurs for the column select permissions defined for the logical
              model "logical_employee" for the inherited role "employee", which combines the roles
              "logical_employee_private_info", "logical_employee_public_info"'
            internal:
            - definition:
                role_name: employee
                role_set:
                - logical_employee_private_info
                - logical_employee_public_info
              name: inherited_role inherited role employee
              reason:
                'Inconsistent object: The source "#{source}" does not support inherited roles
                where the columns allowed are different in the column select permissions in the
                parent roles. This occurs for the column select permissions defined for the logical
                model "logical_employee" for the inherited role "employee", which combines the
                roles "logical_employee_private_info", "logical_employee_public_info"'
              type: inherited_role
            path: $.args
          |]

    shouldReturnYaml testEnvironment actual expected
