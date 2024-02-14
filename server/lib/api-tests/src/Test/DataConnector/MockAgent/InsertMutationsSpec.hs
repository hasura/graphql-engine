module Test.DataConnector.MockAgent.InsertMutationsSpec
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

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = do
  mockAgentGraphqlTest "insert multiple rows with insert permissions" $ \_testEnv performGraphqlRequest -> do
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
                      [ mkFieldsMap
                          [ ("insertedRows_AlbumId", API.mkColumnFieldValue $ J.Number 9001),
                            ("insertedRows_Title", API.mkColumnFieldValue $ J.String "Super Mega Rock"),
                            ( "insertedRows_Artist",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("ArtistId", API.mkColumnFieldValue $ J.Number 2),
                                      ("Name", API.mkColumnFieldValue $ J.String "Accept")
                                    ]
                                  ]
                            )
                          ],
                        mkFieldsMap
                          [ ("insertedRows_AlbumId", API.mkColumnFieldValue $ J.Number 9002),
                            ("insertedRows_Title", API.mkColumnFieldValue $ J.String "Accept This"),
                            ( "insertedRows_Artist",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("ArtistId", API.mkColumnFieldValue $ J.Number 2),
                                      ("Name", API.mkColumnFieldValue $ J.String "Accept")
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
              & API.mrInsertSchema
            .~ Set.fromList
              [ API.TableInsertSchema
                  { API._tisTable = mkTableName "Album",
                    API._tisPrimaryKey = Just $ API.ColumnName "AlbumId" :| [],
                    API._tisFields =
                      mkFieldsMap
                        [ ("AlbumId", API.ColumnInsert $ API.ColumnInsertSchema (API.ColumnName "AlbumId") (API.ColumnTypeScalar $ API.ScalarType "number") False (Just API.AutoIncrement)),
                          ("ArtistId", API.ColumnInsert $ API.ColumnInsertSchema (API.ColumnName "ArtistId") (API.ColumnTypeScalar $ API.ScalarType "number") False Nothing),
                          ("Title", API.ColumnInsert $ API.ColumnInsertSchema (API.ColumnName "Title") (API.ColumnTypeScalar $ API.ScalarType "string") False Nothing)
                        ]
                  }
              ]
              & API.mrOperations
            .~ [ API.InsertOperation
                   $ API.InsertMutationOperation
                     { API._imoTable = mkTableName "Album",
                       API._imoRows =
                         [ API.RowObject
                             $ mkFieldsMap
                               [ ("AlbumId", API.mkColumnInsertFieldValue $ J.Number 9001),
                                 ("ArtistId", API.mkColumnInsertFieldValue $ J.Number 2),
                                 ("Title", API.mkColumnInsertFieldValue $ J.String "Super Mega Rock")
                               ],
                           API.RowObject
                             $ mkFieldsMap
                               [ ("AlbumId", API.mkColumnInsertFieldValue $ J.Number 9002),
                                 ("ArtistId", API.mkColumnInsertFieldValue $ J.Number 2),
                                 ("Title", API.mkColumnInsertFieldValue $ J.String "Accept This")
                               ]
                         ],
                       API._imoPostInsertCheck =
                         Just
                           $ API.ApplyBinaryComparisonOperator
                             API.Equal
                             (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "ArtistId") (API.ScalarType "number") Nothing)
                             (API.ScalarValueComparison $ API.ScalarValue (J.Number 2) (API.ScalarType "number")),
                       API._imoReturningFields =
                         mkFieldsMap
                           [ ("insertedRows_AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                             ("insertedRows_Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing),
                             ( "insertedRows_Artist",
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
