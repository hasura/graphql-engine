{-# LANGUAGE QuasiQuotes #-}

-- | Select Permissions Tests for Data Connector Backend
module Test.DataConnector.SelectPermissionsSpec
  ( spec,
  )
where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector (defaultBackendConfig)
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.DataConnector,
              mkLocalTestEnvironment = Context.noLocalTestEnvironment,
              setup = DataConnector.setupFixture sourceMetadata defaultBackendConfig,
              teardown = DataConnector.teardown,
              customOptions = Nothing
            }
        ]
    )
    tests

testRoleName :: ByteString
testRoleName = "test-role"

sourceMetadata :: Value
sourceMetadata =
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [Employee]
            array_relationships:
              - name: SupportRepForCustomers
                using:
                  manual_configuration:
                    remote_table: [Customer]
                    column_mapping:
                      EmployeeId: SupportRepId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - EmployeeId
                    - FirstName
                    - LastName
                    - Country
                  filter:
                    SupportRepForCustomers:
                      Country:
                        _ceq: [ "$", "Country" ]
          - table: [Customer]
            object_relationships:
              - name: SupportRep
                using:
                  manual_configuration:
                    remote_table: [Employee]
                    column_mapping:
                      SupportRepId: EmployeeId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - CustomerId
                    - FirstName
                    - LastName
                    - Country
                    - SupportRepId
                  filter:
                    SupportRep:
                      Country:
                        _ceq: [ "$", "Country" ]
        configuration: {}
|]

tests :: Context.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "SelectPermissionsSpec" $ do
  it "permissions filter using _ceq that traverses an object relationship" $ \(testEnvironment, _) ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", testRoleName)]
          [graphql|
            query getEmployee {
              Employee {
                EmployeeId
                FirstName
                LastName
                Country
              }
            }
          |]
      )
      [yaml|
        data:
          Employee:
            - Country: Canada
              EmployeeId: 3
              FirstName: Jane
              LastName: Peacock
            - Country: Canada
              EmployeeId: 4
              FirstName: Margaret
              LastName: Park
            - Country: Canada
              EmployeeId: 5
              FirstName: Steve
              LastName: Johnson
      |]

  it "permissions filter using _ceq that traverses an array relationship" $ \(testEnvironment, _) ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", testRoleName)]
          [graphql|
            query getCustomer {
              Customer {
                CustomerId
                FirstName
                LastName
                Country
                SupportRepId
              }
            }
          |]
      )
      [yaml|
        data:
          Customer:
          - Country: Canada
            CustomerId: 3
            FirstName: François
            LastName: Tremblay
            SupportRepId: 3
          - Country: Canada
            CustomerId: 14
            FirstName: Mark
            LastName: Philips
            SupportRepId: 5
          - Country: Canada
            CustomerId: 15
            FirstName: Jennifer
            LastName: Peterson
            SupportRepId: 3
          - Country: Canada
            CustomerId: 29
            FirstName: Robert
            LastName: Brown
            SupportRepId: 3
          - Country: Canada
            CustomerId: 30
            FirstName: Edward
            LastName: Francis
            SupportRepId: 3
          - Country: Canada
            CustomerId: 31
            FirstName: Martha
            LastName: Silk
            SupportRepId: 5
          - Country: Canada
            CustomerId: 32
            FirstName: Aaron
            LastName: Mitchell
            SupportRepId: 4
          - Country: Canada
            CustomerId: 33
            FirstName: Ellie
            LastName: Sullivan
            SupportRepId: 3
      |]

  it "Query involving two tables with their own permissions filter" $ \(testEnvironment, _) ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", testRoleName)]
          [graphql|
            query getEmployee {
              Employee {
                EmployeeId
                FirstName
                LastName
                Country
                SupportRepForCustomers {
                  CustomerId
                  FirstName
                  LastName
                  Country
                }
              }
            }
          |]
      )
      [yaml|
        data:
          Employee:
            - Country: Canada
              EmployeeId: 3
              FirstName: Jane
              LastName: Peacock
              SupportRepForCustomers:
              - Country: Canada
                CustomerId: 3
                FirstName: François
                LastName: Tremblay
              - Country: Canada
                CustomerId: 15
                FirstName: Jennifer
                LastName: Peterson
              - Country: Canada
                CustomerId: 29
                FirstName: Robert
                LastName: Brown
              - Country: Canada
                CustomerId: 30
                FirstName: Edward
                LastName: Francis
              - Country: Canada
                CustomerId: 33
                FirstName: Ellie
                LastName: Sullivan
            - Country: Canada
              EmployeeId: 4
              FirstName: Margaret
              LastName: Park
              SupportRepForCustomers:
              - Country: Canada
                CustomerId: 32
                FirstName: Aaron
                LastName: Mitchell
            - Country: Canada
              EmployeeId: 5
              FirstName: Steve
              LastName: Johnson
              SupportRepForCustomers:
              - Country: Canada
                CustomerId: 14
                FirstName: Mark
                LastName: Philips
              - Country: Canada
                CustomerId: 31
                FirstName: Martha
                LastName: Silk
      |]
