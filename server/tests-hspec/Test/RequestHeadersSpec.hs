{-# LANGUAGE QuasiQuotes #-}

-- | Tests related to request headers
module Test.RequestHeadersSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema
  ( BackendScalarType (..),
    Table (..),
    defaultBackendScalarType,
    table,
  )
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------

-- * Preamble

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv,
                  sqlserverPermissionsSetup testEnv
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema = [author]

author :: Table
author =
  (table "author")
    { tableColumns =
        [ Schema.column
            "uuid"
            ( Schema.TCustomType $
                defaultBackendScalarType
                  { bstMssql = Just "VARCHAR(50)"
                  }
            ),
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["uuid"],
      tableData =
        [ [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7997087", Schema.VStr "Author 1"],
          [Schema.VStr "36a6257b-08bb-45ef-a5cf-c1b7a7", Schema.VStr "Author 2"]
        ]
    }

--------------------------------------------------------------------------------
-- Setup and teardown override

sqlserverPermissionsSetup :: TestEnvironment -> Fixture.SetupAction
sqlserverPermissionsSetup testEnvironment =
  Fixture.SetupAction
    { setupAction =
        GraphqlEngine.postMetadata_
          testEnvironment
          [yaml|
          type: mssql_create_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
            permission:
              filter:
                uuid: X-Hasura-User-Id
              columns: '*'
          |],
      teardownAction = \_ ->
        GraphqlEngine.postMetadata_
          testEnvironment
          [yaml|
          type: mssql_drop_select_permission
          args:
            source: mssql
            table:
              schema: hasura
              name: author
            role: user
          |]
    }

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  -- See https://github.com/hasura/graphql-engine/issues/8158
  it "session variable string values are not truncated to default (30) length" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", "user"),
            ("X-Hasura-User-Id", "36a6257b-08bb-45ef-a5cf-c1b7a7997087")
          ]
          [graphql|
          query {
            hasura_author {
              name
              uuid
            }
          }
          |]
      )
      [yaml|
      data:
        hasura_author:
        - name: 'Author 1'
          uuid: '36a6257b-08bb-45ef-a5cf-c1b7a7997087'
      |]
