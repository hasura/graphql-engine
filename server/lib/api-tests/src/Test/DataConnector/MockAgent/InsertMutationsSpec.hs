module Test.DataConnector.MockAgent.InsertMutationsSpec
  ( spec,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentTest, mockMutationResponse)
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
            insert_permissions:
              - role: *testRoleName
                permission:
                  check:
                    ArtistId: "X-Hasura-ArtistId"
                  set:
                    ArtistId: "X-Hasura-ArtistId"
                  columns:
                    - AlbumId
                    - ArtistId
                    - Title
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
  mockAgentTest "insert multiple rows with insert permissions" $ \performGraphqlRequest -> do
    let headers = [("X-Hasura-ArtistId", "2"), ("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            mutation InsertMutation {
              insert_Album(objects: [
                {AlbumId: 9001, Title: "Super Mega Rock"},
                {AlbumId: 9002, Title: "Accept This"}
              ]) {
                insertCount: affected_rows
                insertedRows: returning {
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
                { API._morAffectedRows = 2,
                  API._morReturning =
                    Just
                      [ HashMap.fromList
                          [ (API.FieldName "insertedRows_AlbumId", API.mkColumnFieldValue $ Aeson.Number 9001),
                            (API.FieldName "insertedRows_Title", API.mkColumnFieldValue $ Aeson.String "Super Mega Rock"),
                            ( API.FieldName "insertedRows_Artist",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [ (API.FieldName "ArtistId", API.mkColumnFieldValue $ Aeson.Number 2),
                                      (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Accept")
                                    ]
                                  ]
                            )
                          ],
                        HashMap.fromList
                          [ (API.FieldName "insertedRows_AlbumId", API.mkColumnFieldValue $ Aeson.Number 9002),
                            (API.FieldName "insertedRows_Title", API.mkColumnFieldValue $ Aeson.String "Accept This"),
                            ( API.FieldName "insertedRows_Artist",
                              API.mkRelationshipFieldValue $
                                rowsResponse
                                  [ [ (API.FieldName "ArtistId", API.mkColumnFieldValue $ Aeson.Number 2),
                                      (API.FieldName "Name", API.mkColumnFieldValue $ Aeson.String "Accept")
                                    ]
                                  ]
                            )
                          ]
                      ]
                }
            ]
    let mockConfig = Mock.chinookMock & mockMutationResponse mockAgentResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrGraphqlResponse
      `shouldBeYaml` [yaml|
          data:
            insert_Album:
              insertCount: 2
              insertedRows:
                - AlbumId: 9001
                  Title: Super Mega Rock
                  Artist:
                    ArtistId: 2
                    Name: Accept
                - AlbumId: 9002
                  Title: Accept This
                  Artist:
                    ArtistId: 2
                    Name: Accept
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
              API._mrInsertSchema =
                [ API.TableInsertSchema
                    { API._tisTable = API.TableName ("Album" :| []),
                      API._tisFields =
                        HashMap.fromList
                          [ (API.FieldName "AlbumId", API.ColumnInsert $ API.ColumnInsertSchema (API.ColumnName "AlbumId") (API.ScalarType "number")),
                            (API.FieldName "ArtistId", API.ColumnInsert $ API.ColumnInsertSchema (API.ColumnName "ArtistId") (API.ScalarType "number")),
                            (API.FieldName "Title", API.ColumnInsert $ API.ColumnInsertSchema (API.ColumnName "Title") (API.ScalarType "string"))
                          ]
                    }
                ],
              API._mrOperations =
                [ API.InsertOperation $
                    API.InsertMutationOperation
                      { API._imoTable = API.TableName ("Album" :| []),
                        API._imoRows =
                          [ API.RowObject $
                              HashMap.fromList
                                [ (API.FieldName "AlbumId", API.mkColumnInsertFieldValue $ Aeson.Number 9001),
                                  (API.FieldName "ArtistId", API.mkColumnInsertFieldValue $ Aeson.Number 2),
                                  (API.FieldName "Title", API.mkColumnInsertFieldValue $ Aeson.String "Super Mega Rock")
                                ],
                            API.RowObject $
                              HashMap.fromList
                                [ (API.FieldName "AlbumId", API.mkColumnInsertFieldValue $ Aeson.Number 9002),
                                  (API.FieldName "ArtistId", API.mkColumnInsertFieldValue $ Aeson.Number 2),
                                  (API.FieldName "Title", API.mkColumnInsertFieldValue $ Aeson.String "Accept This")
                                ]
                          ],
                        API._imoPostInsertCheck =
                          Just $
                            API.ApplyBinaryComparisonOperator
                              API.Equal
                              (API.ComparisonColumn API.CurrentTable (API.ColumnName "ArtistId") $ API.ScalarType "number")
                              (API.ScalarValue (Aeson.Number 2) $ API.ScalarType "number"),
                        API._imoReturningFields =
                          HashMap.fromList
                            [ (API.FieldName "insertedRows_AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number")),
                              (API.FieldName "insertedRows_Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string")),
                              ( API.FieldName "insertedRows_Artist",
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
