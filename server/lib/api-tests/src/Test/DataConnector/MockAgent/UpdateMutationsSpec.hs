module Test.DataConnector.MockAgent.UpdateMutationsSpec
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
import Language.GraphQL.Draft.Syntax.QQ qualified as G
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
          - table: [Track]
            object_relationships:
              - name: Genre
                using:
                  manual_configuration:
                    remote_table: [Genre]
                    column_mapping:
                      GenreId: GenreId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - TrackId
                    - Name
                    - AlbumId
                    - MediaTypeId
                    - GenreId
                    - Composer
                    - Milliseconds
                    - Bytes
                    - UnitPrice
                  filter: {}
            update_permissions:
              - role: *testRoleName
                permission:
                  filter:
                    AlbumId: "X-Hasura-AlbumId"
                  check:
                    UnitPrice:
                      _gt: 0
                  set:
                    AlbumId: "X-Hasura-AlbumId"
                  columns:
                    - Name
                    - AlbumId
                    - MediaTypeId
                    - GenreId
                    - Composer
                    - Milliseconds
                    - Bytes
                    - UnitPrice
          - table: [Genre]
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - GenreId
                    - Name
                  filter: {}
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = do
  mockAgentGraphqlTest "update rows with update permissions" $ \_testEnv performGraphqlRequest -> do
    let headers = [("X-Hasura-AlbumId", "3"), ("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            mutation UpdateMutation {
              update_Track(where: {GenreId: {_eq: 1}}, _set: {Name: "Another Name"}, _inc: {Milliseconds: 1000}) {
                updatedRowCount: affected_rows
                updatedRows: returning {
                  TrackId
                  Name
                  Genre {
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
                          [ ("updatedRows_TrackId", API.mkColumnFieldValue $ J.Number 3),
                            ("updatedRows_Name", API.mkColumnFieldValue $ J.String "Another Name"),
                            ( "updatedRows_Genre",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("Name", API.mkColumnFieldValue $ J.String "Rock")
                                    ]
                                  ]
                            )
                          ],
                        mkFieldsMap
                          [ ("updatedRows_TrackId", API.mkColumnFieldValue $ J.Number 4),
                            ("updatedRows_Name", API.mkColumnFieldValue $ J.String "Another Name"),
                            ( "updatedRows_Genre",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("Name", API.mkColumnFieldValue $ J.String "Rock")
                                    ]
                                  ]
                            )
                          ],
                        mkFieldsMap
                          [ ("updatedRows_TrackId", API.mkColumnFieldValue $ J.Number 5),
                            ("updatedRows_Name", API.mkColumnFieldValue $ J.String "Another Name"),
                            ( "updatedRows_Genre",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("Name", API.mkColumnFieldValue $ J.String "Rock")
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
            update_Track:
              updatedRowCount: 3
              updatedRows:
                - TrackId: 3
                  Name: Another Name
                  Genre:
                    Name: Rock
                - TrackId: 4
                  Name: Another Name
                  Genre:
                    Name: Rock
                - TrackId: 5
                  Name: Another Name
                  Genre:
                    Name: Rock
      |]

    let expectedRequest =
          emptyMutationRequest
            & API.mrRelationships
            .~ Set.fromList
              [ API.RTable
                  $ API.TableRelationships
                    { API._trelSourceTable = mkTableName "Track",
                      API._trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Genre",
                              API.Relationship
                                { API._rTarget = mkTableTarget "Genre",
                                  API._rRelationshipType = API.ObjectRelationship,
                                  API._rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "GenreId", API.mkColumnSelector $ API.ColumnName "GenreId")]
                                }
                            )
                          ]
                    }
              ]
              & API.mrOperations
            .~ [ API.UpdateOperation
                   $ API.UpdateMutationOperation
                     { API._umoTable = mkTableName "Track",
                       API._umoUpdates =
                         Set.fromList
                           [ API.SetColumn
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "Name",
                                   API._rcovValue = J.String "Another Name",
                                   API._rcovValueType = API.ScalarType "string"
                                 },
                             API.CustomUpdateColumnOperator (API.UpdateColumnOperatorName [G.name|inc|])
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "Milliseconds",
                                   API._rcovValue = J.Number 1000,
                                   API._rcovValueType = API.ScalarType "number"
                                 },
                             API.SetColumn
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "AlbumId",
                                   API._rcovValue = J.Number 3,
                                   API._rcovValueType = API.ScalarType "number"
                                 }
                           ],
                       API._umoWhere =
                         Just
                           . API.And
                           $ Set.fromList
                             [ API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 3) (API.ScalarType "number")),
                               API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "GenreId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 1) (API.ScalarType "number"))
                             ],
                       API._umoPostUpdateCheck =
                         Just
                           $ API.ApplyBinaryComparisonOperator
                             API.GreaterThan
                             (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "UnitPrice") (API.ScalarType "number") Nothing)
                             (API.ScalarValueComparison $ API.ScalarValue (J.Number 0) (API.ScalarType "number")),
                       API._umoReturningFields =
                         mkFieldsMap
                           [ ("updatedRows_TrackId", API.ColumnField (API.ColumnName "TrackId") (API.ScalarType "number") Nothing),
                             ("updatedRows_Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing),
                             ( "updatedRows_Genre",
                               API.RelField
                                 ( API.RelationshipField
                                     (API.RelationshipName "Genre")
                                     (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)])
                                 )
                             )
                           ]
                     }
               ]
    _mrrRecordedRequest `shouldBe` Just (Mutation expectedRequest)

  mockAgentGraphqlTest "update_many rows with update permissions" $ \_testEnv performGraphqlRequest -> do
    let headers = [("X-Hasura-AlbumId", "3"), ("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            mutation UpdateMutation {
              update_Track_many(updates: [
                  { where: {TrackId: {_eq: 3}}, _set: {Name: "Another Name"}, _inc: {Milliseconds: 1000} },
                  { where: {TrackId: {_gt: 3}}, _set: {Name: "Better Name"}, _inc: {UnitPrice: 1} }
                ]) {
                updatedRowCount: affected_rows
                updatedRows: returning {
                  TrackId
                  Name
                  Genre {
                    Name
                  }
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
                          [ ("updatedRows_TrackId", API.mkColumnFieldValue $ J.Number 3),
                            ("updatedRows_Name", API.mkColumnFieldValue $ J.String "Another Name"),
                            ( "updatedRows_Genre",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("Name", API.mkColumnFieldValue $ J.String "Rock")
                                    ]
                                  ]
                            )
                          ]
                      ]
                },
              API.MutationOperationResults
                { API._morAffectedRows = 2,
                  API._morReturning =
                    Just
                      [ mkFieldsMap
                          [ ("updatedRows_TrackId", API.mkColumnFieldValue $ J.Number 4),
                            ("updatedRows_Name", API.mkColumnFieldValue $ J.String "Better Name"),
                            ( "updatedRows_Genre",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("Name", API.mkColumnFieldValue $ J.String "Rock")
                                    ]
                                  ]
                            )
                          ],
                        mkFieldsMap
                          [ ("updatedRows_TrackId", API.mkColumnFieldValue $ J.Number 5),
                            ("updatedRows_Name", API.mkColumnFieldValue $ J.String "Better Name"),
                            ( "updatedRows_Genre",
                              API.mkRelationshipFieldValue
                                $ mkRowsQueryResponse
                                  [ [ ("Name", API.mkColumnFieldValue $ J.String "Rock")
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
            update_Track_many:
              - updatedRowCount: 1
                updatedRows:
                  - TrackId: 3
                    Name: Another Name
                    Genre:
                      Name: Rock
              - updatedRowCount: 2
                updatedRows:
                  - TrackId: 4
                    Name: Better Name
                    Genre:
                      Name: Rock
                  - TrackId: 5
                    Name: Better Name
                    Genre:
                      Name: Rock
      |]

    let sharedPostUpdateCheck =
          Just
            $ API.ApplyBinaryComparisonOperator
              API.GreaterThan
              (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "UnitPrice") (API.ScalarType "number") Nothing)
              (API.ScalarValueComparison $ API.ScalarValue (J.Number 0) (API.ScalarType "number"))
    let sharedReturning =
          mkFieldsMap
            [ ("updatedRows_TrackId", API.ColumnField (API.ColumnName "TrackId") (API.ScalarType "number") Nothing),
              ("updatedRows_Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing),
              ( "updatedRows_Genre",
                API.RelField
                  ( API.RelationshipField
                      (API.RelationshipName "Genre")
                      (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)])
                  )
              )
            ]
    let expectedRequest =
          emptyMutationRequest
            & API.mrRelationships
            .~ Set.fromList
              [ API.RTable
                  $ API.TableRelationships
                    { API._trelSourceTable = mkTableName "Track",
                      API._trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Genre",
                              API.Relationship
                                { API._rTarget = mkTableTarget "Genre",
                                  API._rRelationshipType = API.ObjectRelationship,
                                  API._rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "GenreId", API.mkColumnSelector $ API.ColumnName "GenreId")]
                                }
                            )
                          ]
                    }
              ]
              & API.mrOperations
            .~ [ API.UpdateOperation
                   $ API.UpdateMutationOperation
                     { API._umoTable = mkTableName "Track",
                       API._umoUpdates =
                         Set.fromList
                           [ API.SetColumn
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "Name",
                                   API._rcovValue = J.String "Another Name",
                                   API._rcovValueType = API.ScalarType "string"
                                 },
                             API.CustomUpdateColumnOperator (API.UpdateColumnOperatorName [G.name|inc|])
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "Milliseconds",
                                   API._rcovValue = J.Number 1000,
                                   API._rcovValueType = API.ScalarType "number"
                                 },
                             API.SetColumn
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "AlbumId",
                                   API._rcovValue = J.Number 3,
                                   API._rcovValueType = API.ScalarType "number"
                                 }
                           ],
                       API._umoWhere =
                         Just
                           . API.And
                           $ Set.fromList
                             [ API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 3) (API.ScalarType "number")),
                               API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "TrackId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 3) (API.ScalarType "number"))
                             ],
                       API._umoPostUpdateCheck = sharedPostUpdateCheck,
                       API._umoReturningFields = sharedReturning
                     },
                 API.UpdateOperation
                   $ API.UpdateMutationOperation
                     { API._umoTable = mkTableName "Track",
                       API._umoUpdates =
                         Set.fromList
                           [ API.SetColumn
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "Name",
                                   API._rcovValue = J.String "Better Name",
                                   API._rcovValueType = API.ScalarType "string"
                                 },
                             API.CustomUpdateColumnOperator (API.UpdateColumnOperatorName [G.name|inc|])
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "UnitPrice",
                                   API._rcovValue = J.Number 1,
                                   API._rcovValueType = API.ScalarType "number"
                                 },
                             API.SetColumn
                               $ API.RowColumnOperatorValue
                                 { API._rcovColumn = API.ColumnName "AlbumId",
                                   API._rcovValue = J.Number 3,
                                   API._rcovValueType = API.ScalarType "number"
                                 }
                           ],
                       API._umoWhere =
                         Just
                           . API.And
                           $ Set.fromList
                             [ API.ApplyBinaryComparisonOperator
                                 API.Equal
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 3) (API.ScalarType "number")),
                               API.ApplyBinaryComparisonOperator
                                 API.GreaterThan
                                 (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "TrackId") (API.ScalarType "number") Nothing)
                                 (API.ScalarValueComparison $ API.ScalarValue (J.Number 3) (API.ScalarType "number"))
                             ],
                       API._umoPostUpdateCheck = sharedPostUpdateCheck,
                       API._umoReturningFields = sharedReturning
                     }
               ]
    _mrrRecordedRequest `shouldBe` Just (Mutation expectedRequest)
