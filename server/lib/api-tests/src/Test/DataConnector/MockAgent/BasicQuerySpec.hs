{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.BasicQuerySpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((?~))
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentGraphqlTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

testRoleName :: ByteString
testRoleName = "test-role"

sourceMetadata :: J.Value
sourceMetadata =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [Album]
            configuration:
              custom_root_fields:
                select: albums
                select_by_pk: albums_by_pk
              column_config:
                AlbumId:
                  custom_name: id
                Title:
                  custom_name: title
                ArtistId:
                  custom_name: artist_id
            object_relationships:
              - name: artist
                using:
                  manual_configuration:
                    remote_table: [Artist]
                    column_mapping:
                      ArtistId: ArtistId
          - table: [Artist]
            configuration:
              custom_root_fields:
                select: artists
              column_config:
                ArtistId:
                  custom_name: id
                Name:
                  custom_name: name
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - ArtistId
                    - Name
                  limit: 3
                  filter: {}
          - table: [Employee]
          - table: [Customer]
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - CustomerId
                  filter:
                    _exists:
                      _table: [Employee]
                      _where:
                        EmployeeId:
                          _eq: X-Hasura-EmployeeId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Basic Tests" $ do
  mockAgentGraphqlTest "works with simple object query" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              albums(limit: 1) {
                id
                title
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("id", API.mkColumnFieldValue $ J.Number 1),
                ("title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          albums:
            - id: 1
              title: For Those About To Rock We Salute You
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("id", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                      ("title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)
                    ]
                    & API.qLimit
                  ?~ 1
              )
        )

  mockAgentGraphqlTest "works with simple object query with missing field" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              albums(limit: 1) {
                id
                title
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("id", API.mkColumnFieldValue $ J.Number 1)
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          albums:
            - id: 1
              title: null
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("id", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                      ("title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)
                    ]
                    & API.qLimit
                  ?~ 1
              )
        )

  mockAgentGraphqlTest "permissions-based row limits are applied" $ \_testEnv performGraphqlRequest -> do
    let headers = [("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            query getArtists {
              artists(limit: 5) {
                id
                name
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("id", API.mkColumnFieldValue $ J.Number 1),
                ("name", API.mkColumnFieldValue $ J.String "AC/DC")
              ],
              [ ("id", API.mkColumnFieldValue $ J.Number 2),
                ("name", API.mkColumnFieldValue $ J.String "Accept")
              ],
              [ ("id", API.mkColumnFieldValue $ J.Number 3),
                ("name", API.mkColumnFieldValue $ J.String "Aerosmith")
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          artists:
            - id: 1
              name: AC/DC
            - id: 2
              name: Accept
            - id: 3
              name: Aerosmith
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Artist")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("id", API.ColumnField (API.ColumnName "ArtistId") (API.ScalarType "number") Nothing),
                      ("name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)
                    ]
                    & API.qLimit
                  ?~ 3 -- The permissions limit is smaller than the query limit, so it is used
              )
        )

  mockAgentGraphqlTest "works with an exists-based permissions filter" $ \_testEnv performGraphqlRequest -> do
    let headers =
          [ ("X-Hasura-Role", testRoleName),
            ("X-Hasura-EmployeeId", "1")
          ]
    let graphqlRequest =
          [graphql|
            query getCustomers {
              Customer {
                CustomerId
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("CustomerId", API.mkColumnFieldValue $ J.Number 1)
              ],
              [ ("CustomerId", API.mkColumnFieldValue $ J.Number 2)
              ],
              [ ("CustomerId", API.mkColumnFieldValue $ J.Number 3)
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Customer:
            - CustomerId: 1
            - CustomerId: 2
            - CustomerId: 3
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Customer")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("CustomerId", API.ColumnField (API.ColumnName "CustomerId") (API.ScalarType "number") Nothing)
                    ]
                    & API.qWhere
                  ?~ API.Exists
                    (API.UnrelatedTable $ mkTableName "Employee")
                    ( API.ApplyBinaryComparisonOperator
                        API.Equal
                        (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "EmployeeId") (API.ScalarType "number") Nothing)
                        (API.ScalarValueComparison $ API.ScalarValue (J.Number 1) (API.ScalarType "number"))
                    )
              )
        )
