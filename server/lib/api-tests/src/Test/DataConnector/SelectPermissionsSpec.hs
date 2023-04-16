{-# LANGUAGE QuasiQuotes #-}

-- | Select Permissions Tests for Data Connector Backend
module Test.DataConnector.SelectPermissionsSpec (spec, tests) where

--------------------------------------------------------------------------------

import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Chinook qualified as Chinook
import Harness.Backend.DataConnector.Chinook.Reference qualified as Reference
import Harness.Backend.DataConnector.Chinook.Sqlite qualified as Sqlite
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ Reference.chinookFixture,
          Sqlite.chinookFixture
        ]
    )
    tests

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, a)
tests = describe "SelectPermissionsSpec" $ do
  it "permissions filter using _ceq that traverses an object relationship" $ \(testEnvironment, _) ->
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", Chinook.testRoleName)]
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
      testEnvironment
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", Chinook.testRoleName)]
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
      testEnvironment
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", Chinook.testRoleName)]
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

  it "Query that orders by a related table that has a permissions filter" $ \(testEnvironment, _) -> do
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [("X-Hasura-Role", Chinook.testRoleName)]
          [graphql|
            query getEmployee {
              Employee(order_by: {SupportRepForCustomers_aggregate: {count: desc}}) {
                FirstName
                LastName
                Country
                SupportRepForCustomers {
                  Country
                  CustomerId
                }
              }
            }
          |]
      )
      [yaml|
        data:
          Employee:
            - FirstName: Jane
              LastName: Peacock
              Country: Canada
              SupportRepForCustomers:
                - Country: Canada
                  CustomerId: 3
                - Country: Canada
                  CustomerId: 15
                - Country: Canada
                  CustomerId: 29
                - Country: Canada
                  CustomerId: 30
                - Country: Canada
                  CustomerId: 33
            - FirstName: Steve
              LastName: Johnson
              Country: Canada
              SupportRepForCustomers:
                - Country: Canada
                  CustomerId: 14
                - Country: Canada
                  CustomerId: 31
            - FirstName: Margaret
              LastName: Park
              Country: Canada
              SupportRepForCustomers:
                - Country: Canada
                  CustomerId: 32
      |]

  it "Query that allows access to a table using an exists-based permissions filter" $ \(testEnvironment, _) -> do
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", Chinook.testRoleName),
            ("X-Hasura-CustomerId", "1")
          ]
          [graphql|
            query getAlbums {
              Album(order_by: {AlbumId: asc}, limit: 3) {
                AlbumId
              }
            }
          |]
      )
      [yaml|
        data:
          Album:
            - AlbumId: 1
            - AlbumId: 2
            - AlbumId: 3
      |]

  it "Query that disallows access to a table using an exists-based permissions filter" $ \(testEnvironment, _) -> do
    shouldReturnYaml
      testEnvironment
      ( GraphqlEngine.postGraphqlWithHeaders
          testEnvironment
          [ ("X-Hasura-Role", Chinook.testRoleName),
            ("X-Hasura-CustomerId", "0")
          ]
          [graphql|
            query getAlbums {
              Album(order_by: {AlbumId: asc}, limit: 3) {
                AlbumId
              }
            }
          |]
      )
      [yaml|
        data:
          Album: []
      |]
