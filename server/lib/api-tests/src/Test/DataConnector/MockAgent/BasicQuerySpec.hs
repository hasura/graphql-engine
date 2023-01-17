{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.BasicQuerySpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
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

sourceMetadata :: Aeson.Value
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
                select_by_pk: artists_by_pk
              column_config:
                ArtistId:
                  custom_name: id
                Name:
                  custom_name: name
            array_relationships:
              - name: albums
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      ArtistId: ArtistId
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

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests _opts = describe "Basic Tests" $ do
  mockAgentTest "works with simple object query" $ \performGraphqlRequest -> do
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
          rowsResponse
            [ [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 1),
                (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrGraphqlResponse
      `shouldBeYaml` [yaml|
        data:
          albums:
            - id: 1
              title: For Those About To Rock We Salute You
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            API.QueryRequest
              { _qrTable = API.TableName ("Album" :| []),
                _qrTableRelationships = [],
                _qrQuery =
                  API.Query
                    { _qFields =
                        Just $
                          HashMap.fromList
                            [ (API.FieldName "id", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number")),
                              (API.FieldName "title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string"))
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Just 1,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Nothing
                    }
              }
        )

  mockAgentTest "works with order_by id" $ \performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              albums(limit: 3, order_by: {id: asc}) {
                id
                title
              }
            }
          |]
    let queryResponse =
          rowsResponse
            [ [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 1),
                (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
              ],
              [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 2),
                (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "Balls to the Wall")
              ],
              [ (API.FieldName "id", API.mkColumnFieldValue $ Aeson.Number 3),
                (API.FieldName "title", API.mkColumnFieldValue $ Aeson.String "Restless and Wild")
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrGraphqlResponse
      `shouldBeYaml` [yaml|
        data:
          albums:
            - id: 1
              title: For Those About To Rock We Salute You
            - id: 2
              title: Balls to the Wall
            - id: 3
              title: Restless and Wild
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            API.QueryRequest
              { _qrTable = API.TableName ("Album" :| []),
                _qrTableRelationships = [],
                _qrQuery =
                  API.Query
                    { _qFields =
                        Just $
                          HashMap.fromList
                            [ (API.FieldName "id", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                              (API.FieldName "title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Just 3,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Just (API.OrderBy mempty (API.OrderByElement [] (API.OrderByColumn (API.ColumnName "AlbumId")) API.Ascending :| []))
                    }
              }
        )

  mockAgentTest "works with an exists-based permissions filter" $ \performGraphqlRequest -> do
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
          rowsResponse
            [ [ (API.FieldName "CustomerId", API.mkColumnFieldValue $ Aeson.Number 1)
              ],
              [ (API.FieldName "CustomerId", API.mkColumnFieldValue $ Aeson.Number 2)
              ],
              [ (API.FieldName "CustomerId", API.mkColumnFieldValue $ Aeson.Number 3)
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrGraphqlResponse
      `shouldBeYaml` [yaml|
        data:
          Customer:
            - CustomerId: 1
            - CustomerId: 2
            - CustomerId: 3
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            API.QueryRequest
              { _qrTable = API.TableName ("Customer" :| []),
                _qrTableRelationships = [],
                _qrQuery =
                  API.Query
                    { _qFields =
                        Just $
                          HashMap.fromList
                            [ (API.FieldName "CustomerId", API.ColumnField (API.ColumnName "CustomerId") $ API.ScalarType "number")
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Nothing,
                      _qOffset = Nothing,
                      _qWhere =
                        Just $
                          API.Exists (API.UnrelatedTable $ API.TableName ("Employee" :| [])) $
                            API.ApplyBinaryComparisonOperator
                              API.Equal
                              (API.ComparisonColumn API.CurrentTable (API.ColumnName "EmployeeId") $ API.ScalarType "number")
                              (API.ScalarValue (Aeson.Number 1) $ API.ScalarType "number"),
                      _qOrderBy = Nothing
                    }
              }
        )

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
