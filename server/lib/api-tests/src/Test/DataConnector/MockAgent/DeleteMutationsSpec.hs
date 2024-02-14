module Test.DataConnector.MockAgent.DeleteMutationsSpec
  ( spec,
  )
where

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
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
import Test.DataConnector.MockAgent.TestHelpers
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

sourceMetadata :: J.Value
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

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = do
  mockAgentGraphqlTest "delete rows with delete permissions" $ \_testEnv performGraphqlRequest -> do
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
                      [ mkFieldsMap
                          [ ("deletedRows_AlbumId", API.mkColumnFieldValue $ J.Number 112),
                            ("deletedRows_Title", API.mkColumnFieldValue $ J.String "The Number of The Beast"),
                            ( "deletedRows_Artist",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("ArtistId", API.mkColumnFieldValue $ J.Number 90),
                                      ("Name", API.mkColumnFieldValue $ J.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ],
                        mkFieldsMap
                          [ ("deletedRows_AlbumId", API.mkColumnFieldValue $ J.Number 113),
                            ("deletedRows_Title", API.mkColumnFieldValue $ J.String "The X Factor"),
                            ( "deletedRows_Artist",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("ArtistId", API.mkColumnFieldValue $ J.Number 90),
                                      ("Name", API.mkColumnFieldValue $ J.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ],
                        mkFieldsMap
                          [ ("deletedRows_AlbumId", API.mkColumnFieldValue $ J.Number 114),
                            ("deletedRows_Title", API.mkColumnFieldValue $ J.String "Virtual XI"),
                            ( "deletedRows_Artist",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("ArtistId", API.mkColumnFieldValue $ J.Number 90),
                                      ("Name", API.mkColumnFieldValue $ J.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ]
                      ]
                }
            ]
    let mockConfig = mockMutationResponse mockAgentResponse

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
          emptyMutationRequest
            & API.mrRelationships
            .~ Set.fromList
              [ API.RTable
                  $ API.TableRelationships
                    { API._trelSourceTable = mkTableName "Album",
                      API._trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Artist",
                              API.Relationship
                                { API._rTarget = mkTableTarget "Artist",
                                  API._rRelationshipType = API.ObjectRelationship,
                                  API._rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
                                }
                            )
                          ]
                    }
              ]
              & API.mrOperations
            .~ [ API.DeleteOperation
                   $ API.DeleteMutationOperation
                     { API._dmoTable = mkTableName "Album",
                       API._dmoWhere =
                         Just
                           . API.And
                           $ Set.fromList
                             [ API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "ArtistId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 90) (API.ScalarType "number")),
                               API.ApplyBinaryComparisonOperator
                                 API.GreaterThan
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 111) (API.ScalarType "number"))
                             ],
                       API._dmoReturningFields =
                         mkFieldsMap
                           [ ("deletedRows_AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                             ("deletedRows_Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing),
                             ( "deletedRows_Artist",
                               API.RelField
                                 ( API.RelationshipField
                                     (API.RelationshipName "Artist")
                                     ( emptyQuery
                                         & API.qFields
                                         ?~ mkFieldsMap
                                           [ ("ArtistId", API.ColumnField (API.ColumnName "ArtistId") (API.ScalarType "number") Nothing),
                                             ("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)
                                           ]
                                     )
                                 )
                             )
                           ]
                     }
               ]
    _mrrRecordedRequest `shouldBe` Just (Mutation expectedRequest)

  mockAgentGraphqlTest "delete row by pk with delete permissions" $ \_testEnv performGraphqlRequest -> do
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
                      [ mkFieldsMap
                          [ ("AlbumId", API.mkColumnFieldValue $ J.Number 112),
                            ("Title", API.mkColumnFieldValue $ J.String "The Number of The Beast"),
                            ( "Artist",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("ArtistId", API.mkColumnFieldValue $ J.Number 90),
                                      ("Name", API.mkColumnFieldValue $ J.String "Iron Maiden")
                                    ]
                                  ]
                            )
                          ]
                      ]
                }
            ]
    let mockConfig = mockMutationResponse mockAgentResponse

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
          emptyMutationRequest
            & API.mrRelationships
            .~ Set.fromList
              [ API.RTable
                  $ API.TableRelationships
                    { API._trelSourceTable = mkTableName "Album",
                      API._trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Artist",
                              API.Relationship
                                { API._rTarget = mkTableTarget "Artist",
                                  API._rRelationshipType = API.ObjectRelationship,
                                  API._rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
                                }
                            )
                          ]
                    }
              ]
              & API.mrOperations
            .~ [ API.DeleteOperation
                   $ API.DeleteMutationOperation
                     { API._dmoTable = mkTableName "Album",
                       API._dmoWhere =
                         Just
                           . API.And
                           $ Set.fromList
                             [ API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "ArtistId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 90) (API.ScalarType "number")),
                               API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 112) (API.ScalarType "number"))
                             ],
                       API._dmoReturningFields =
                         mkFieldsMap
                           [ ("AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                             ("Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing),
                             ( "Artist",
                               API.RelField
                                 ( API.RelationshipField
                                     (API.RelationshipName "Artist")
                                     ( emptyQuery
                                         & API.qFields
                                         ?~ mkFieldsMap
                                           [ ("ArtistId", API.ColumnField (API.ColumnName "ArtistId") (API.ScalarType "number") Nothing),
                                             ("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)
                                           ]
                                     )
                                 )
                             )
                           ]
                     }
               ]
    _mrrRecordedRequest `shouldBe` Just (Mutation expectedRequest)
