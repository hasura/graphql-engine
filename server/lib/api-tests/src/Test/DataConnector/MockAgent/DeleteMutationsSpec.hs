module Test.DataConnector.MockAgent.DeleteMutationsSpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentGraphqlTest, mockMutationResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec

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
            object_relationships:
              - name: Artist
                using:
                  manual_configuration:
                    remote_table: [Artist]
                    column_mapping:
                      ArtistId: ArtistId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - AlbumId
                    - ArtistId
                    - Title
                  filter: {}
            delete_permissions:
              - role: *testRoleName
                permission:
                  filter:
                    ArtistId: "X-Hasura-ArtistId"
          - table: [Artist]
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - ArtistId
                    - Name
                  filter: {}
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests _opts = do
  mockAgentGraphqlTest "delete rows with delete permissions" $ \performGraphqlRequest -> do
    let headers = [("X-Hasura-ArtistId", "90"), ("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            mutation DeleteMutation {
              delete_Album(where: {AlbumId: {_gt: 111}}) {
                deleteCount: affected_rows
                deletedRows: returning {
                  AlbumId
                  Title
                  Artist {
                    ArtistId
                    Name
                  }
                }
              }
            }
          |]
    let mockAgentResponse =
          API.MutationResponse
            [ API.MutationOperationResults
                { API._morAffectedRows = 3,
                  API._morReturning =
                    Just
                      [ HashMap.fromList
                          [ (API.FieldName "deletedRows_AlbumId", API.mkColumnFieldValue $ Aeson.Number 112),
                            (API.FieldName "deletedRows_Title", API.mkColumnFieldValue $ Aeson.String "The Number of The Beast"),
                            ( API.FieldName "deletedRows_Artist",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [ (API.FieldName "ArtistId", API.mkColumnFieldValue $ Aeson.Number 90),
                                      (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ],
                        HashMap.fromList
                          [ (API.FieldName "deletedRows_AlbumId", API.mkColumnFieldValue $ Aeson.Number 113),
                            (API.FieldName "deletedRows_Title", API.mkColumnFieldValue $ Aeson.String "The X Factor"),
                            ( API.FieldName "deletedRows_Artist",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [ (API.FieldName "ArtistId", API.mkColumnFieldValue $ Aeson.Number 90),
                                      (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ],
                        HashMap.fromList
                          [ (API.FieldName "deletedRows_AlbumId", API.mkColumnFieldValue $ Aeson.Number 114),
                            (API.FieldName "deletedRows_Title", API.mkColumnFieldValue $ Aeson.String "Virtual XI"),
                            ( API.FieldName "deletedRows_Artist",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [ (API.FieldName "ArtistId", API.mkColumnFieldValue $ Aeson.Number 90),
                                      (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ]
                      ]
                }
            ]
    let mockConfig = Mock.chinookMock & mockMutationResponse mockAgentResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
          data:
            delete_Album:
              deleteCount: 3
              deletedRows:
                - AlbumId: 112
                  Title: The Number of The Beast
                  Artist:
                    ArtistId: 90
                    Name: Iron Maiden
                - AlbumId: 113
                  Title: The X Factor
                  Artist:
                    ArtistId: 90
                    Name: Iron Maiden
                - AlbumId: 114
                  Title: Virtual XI
                  Artist:
                    ArtistId: 90
                    Name: Iron Maiden
      |]

    let expectedRequest =
          API.MutationRequest
            { API._mrTableRelationships =
                [ API.TableRelationships
                    { API._trSourceTable = API.TableName ("Album" :| []),
                      API._trRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Artist",
                              API.Relationship
                                { API._rTargetTable = API.TableName ("Artist" :| []),
                                  API._rRelationshipType = API.ObjectRelationship,
                                  API._rColumnMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
                                }
                            )
                          ]
                    }
                ],
              API._mrInsertSchema = [],
              API._mrOperations =
                [ API.DeleteOperation $
                    API.DeleteMutationOperation
                      { API._dmoTable = API.TableName ("Album" :| []),
                        API._dmoWhere =
                          Just $
                            API.And
                              [ API.ApplyBinaryComparisonOperator
                                  API.Equal
                                  (API.ComparisonColumn API.CurrentTable (API.ColumnName "ArtistId") $ API.ScalarType "number")
                                  (API.ScalarValue (Aeson.Number 90) $ API.ScalarType "number"),
                                API.ApplyBinaryComparisonOperator
                                  API.GreaterThan
                                  (API.ComparisonColumn API.CurrentTable (API.ColumnName "AlbumId") $ API.ScalarType "number")
                                  (API.ScalarValue (Aeson.Number 111) $ API.ScalarType "number")
                              ],
                        API._dmoReturningFields =
                          HashMap.fromList
                            [ (API.FieldName "deletedRows_AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number")),
                              (API.FieldName "deletedRows_Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string")),
                              ( API.FieldName "deletedRows_Artist",
                                API.RelField
                                  ( API.RelationshipField
                                      (API.RelationshipName "Artist")
                                      API.Query
                                        { _qFields =
                                            Just $
                                              HashMap.fromList
                                                [ (API.FieldName "ArtistId", API.ColumnField (API.ColumnName "ArtistId") $ API.ScalarType "number"),
                                                  (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") $ API.ScalarType "string")
                                                ],
                                          _qAggregates = Nothing,
                                          _qLimit = Nothing,
                                          _qOffset = Nothing,
                                          _qWhere = Nothing,
                                          _qOrderBy = Nothing
                                        }
                                  )
                              )
                            ]
                      }
                ]
            }
    _mrrRecordedRequest `shouldBe` Just (Mutation expectedRequest)

  mockAgentGraphqlTest "delete row by pk with delete permissions" $ \performGraphqlRequest -> do
    let headers = [("X-Hasura-ArtistId", "90"), ("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            mutation DeleteMutation {
              delete_Album_by_pk(AlbumId: 112) {
                AlbumId
                Title
                Artist {
                  ArtistId
                  Name
                }
              }
            }
          |]
    let mockAgentResponse =
          API.MutationResponse
            [ API.MutationOperationResults
                { API._morAffectedRows = 1,
                  API._morReturning =
                    Just
                      [ HashMap.fromList
                          [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 112),
                            (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "The Number of The Beast"),
                            ( API.FieldName "Artist",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [ (API.FieldName "ArtistId", API.mkColumnFieldValue $ Aeson.Number 90),
                                      (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ]
                      ]
                }
            ]
    let mockConfig = Mock.chinookMock & mockMutationResponse mockAgentResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
          data:
            delete_Album_by_pk:
              AlbumId: 112
              Title: The Number of The Beast
              Artist:
                ArtistId: 90
                Name: Iron Maiden
      |]

    let expectedRequest =
          API.MutationRequest
            { API._mrTableRelationships =
                [ API.TableRelationships
                    { API._trSourceTable = API.TableName ("Album" :| []),
                      API._trRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Artist",
                              API.Relationship
                                { API._rTargetTable = API.TableName ("Artist" :| []),
                                  API._rRelationshipType = API.ObjectRelationship,
                                  API._rColumnMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
                                }
                            )
                          ]
                    }
                ],
              API._mrInsertSchema = [],
              API._mrOperations =
                [ API.DeleteOperation $
                    API.DeleteMutationOperation
                      { API._dmoTable = API.TableName ("Album" :| []),
                        API._dmoWhere =
                          Just $
                            API.And
                              [ API.ApplyBinaryComparisonOperator
                                  API.Equal
                                  (API.ComparisonColumn API.CurrentTable (API.ColumnName "ArtistId") $ API.ScalarType "number")
                                  (API.ScalarValue (Aeson.Number 90) $ API.ScalarType "number"),
                                API.ApplyBinaryComparisonOperator
                                  API.Equal
                                  (API.ComparisonColumn API.CurrentTable (API.ColumnName "AlbumId") $ API.ScalarType "number")
                                  (API.ScalarValue (Aeson.Number 112) $ API.ScalarType "number")
                              ],
                        API._dmoReturningFields =
                          HashMap.fromList
                            [ (API.FieldName "AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number")),
                              (API.FieldName "Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string")),
                              ( API.FieldName "Artist",
                                API.RelField
                                  ( API.RelationshipField
                                      (API.RelationshipName "Artist")
                                      API.Query
                                        { _qFields =
                                            Just $
                                              HashMap.fromList
                                                [ (API.FieldName "ArtistId", API.ColumnField (API.ColumnName "ArtistId") $ API.ScalarType "number"),
                                                  (API.FieldName "Name", API.ColumnField (API.ColumnName "Name") $ API.ScalarType "string")
                                                ],
                                          _qAggregates = Nothing,
                                          _qLimit = Nothing,
                                          _qOffset = Nothing,
                                          _qWhere = Nothing,
                                          _qOrderBy = Nothing
                                        }
                                  )
                              )
                            ]
                      }
                ]
            }
    _mrrRecordedRequest `shouldBe` Just (Mutation expectedRequest)

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
