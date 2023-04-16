{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.QueryRelationshipsSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
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
          - table: [Artist]
          - table: [Genre]
          - table: [MediaType]
          - table: [Track]
            object_relationships:
              - name: Album
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      AlbumId: AlbumId
              - name: Genre
                using:
                  manual_configuration:
                    remote_table: [Genre]
                    column_mapping:
                      GenreId: GenreId
              - name: MediaType
                using:
                  manual_configuration:
                    remote_table: [MediaType]
                    column_mapping:
                      MediaTypeId: MediaTypeId
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

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Object Relationships Tests" $ do
  mockAgentGraphqlTest "works with multiple object relationships" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getTrack {
              Track(limit: 1) {
                Name
                Genre {
                  Name
                }
                MediaType {
                  Name
                }
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("Name", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock (We Salute You)"),
                ( "Genre",
                  API.mkRelationshipFieldValue $
                    mkRowsQueryResponse
                      [ [("Name", API.mkColumnFieldValue $ Aeson.String "Rock")]
                      ]
                ),
                ( "MediaType",
                  API.mkRelationshipFieldValue $
                    mkRowsQueryResponse
                      [ [("Name", API.mkColumnFieldValue $ Aeson.String "MPEG audio file")]
                      ]
                )
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Track:
            - Genre:
                Name: "Rock"
              MediaType:
                Name: "MPEG audio file"
              Name: "For Those About To Rock (We Salute You)"
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            mkQueryRequest
              (mkTableName "Track")
              ( emptyQuery
                  & API.qFields
                    ?~ mkFieldsMap
                      [ ("Name", API.ColumnField (API.ColumnName "Name") $ API.ScalarType "string"),
                        ( "Genre",
                          API.RelField
                            ( API.RelationshipField
                                (API.RelationshipName "Genre")
                                (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") $ API.ScalarType "string")])
                            )
                        ),
                        ( "MediaType",
                          API.RelField
                            ( API.RelationshipField
                                (API.RelationshipName "MediaType")
                                (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") $ API.ScalarType "string")])
                            )
                        )
                      ]
                  & API.qLimit ?~ 1
              )
              & API.qrTableRelationships
                .~ [ API.TableRelationships
                       { _trSourceTable = mkTableName "Track",
                         _trRelationships =
                           HashMap.fromList
                             [ ( API.RelationshipName "Genre",
                                 API.Relationship
                                   { _rTargetTable = mkTableName "Genre",
                                     _rRelationshipType = API.ObjectRelationship,
                                     _rColumnMapping = HashMap.fromList [(API.ColumnName "GenreId", API.ColumnName "GenreId")]
                                   }
                               ),
                               ( API.RelationshipName "MediaType",
                                 API.Relationship
                                   { _rTargetTable = mkTableName "MediaType",
                                     _rRelationshipType = API.ObjectRelationship,
                                     _rColumnMapping =
                                       HashMap.fromList
                                         [(API.ColumnName "MediaTypeId", API.ColumnName "MediaTypeId")]
                                   }
                               )
                             ]
                       }
                   ]
        )

  mockAgentGraphqlTest "works with an order by that navigates relationships" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getTrack {
              Track(order_by: [{Album: {Artist: {Name: desc}}}, { Name: asc }], limit: 1) {
                Album {
                  Artist {
                    Name
                  }
                }
                Name
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ( "Album",
                  API.mkRelationshipFieldValue $
                    mkRowsQueryResponse
                      [ [ ( "Artist",
                            API.mkRelationshipFieldValue $
                              mkRowsQueryResponse
                                [[("Name", API.mkColumnFieldValue $ Aeson.String "Zeca Pagodinho")]]
                          )
                        ]
                      ]
                ),
                ("Name", API.mkColumnFieldValue $ Aeson.String "Camarão que Dorme e Onda Leva")
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Track:
            - Album:
                Artist:
                  Name: Zeca Pagodinho
              Name: Camarão que Dorme e Onda Leva
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            mkQueryRequest
              (mkTableName "Track")
              ( emptyQuery
                  & API.qFields
                    ?~ mkFieldsMap
                      [ ("Name", API.ColumnField (API.ColumnName "Name") $ API.ScalarType "string"),
                        ( "Album",
                          API.RelField
                            ( API.RelationshipField
                                (API.RelationshipName "Album")
                                ( emptyQuery
                                    & API.qFields
                                      ?~ mkFieldsMap
                                        [ ( "Artist",
                                            API.RelField
                                              ( API.RelationshipField
                                                  (API.RelationshipName "Artist")
                                                  (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string"))])
                                              )
                                          )
                                        ]
                                )
                            )
                        )
                      ]
                  & API.qLimit ?~ 1
                  & API.qOrderBy
                    ?~ API.OrderBy
                      ( HashMap.fromList
                          [ ( API.RelationshipName "Album",
                              API.OrderByRelation
                                Nothing
                                ( HashMap.fromList
                                    [ ( API.RelationshipName "Artist",
                                        API.OrderByRelation
                                          Nothing
                                          mempty
                                      )
                                    ]
                                )
                            )
                          ]
                      )
                      ( NE.fromList
                          [ API.OrderByElement [API.RelationshipName "Album", API.RelationshipName "Artist"] (API.OrderByColumn (API.ColumnName "Name")) API.Descending,
                            API.OrderByElement [] (API.OrderByColumn (API.ColumnName "Name")) API.Ascending
                          ]
                      )
              )
              & API.qrTableRelationships
                .~ [ API.TableRelationships
                       { _trSourceTable = mkTableName "Track",
                         _trRelationships =
                           HashMap.fromList
                             [ ( API.RelationshipName "Album",
                                 API.Relationship
                                   { _rTargetTable = mkTableName "Album",
                                     _rRelationshipType = API.ObjectRelationship,
                                     _rColumnMapping = HashMap.fromList [(API.ColumnName "AlbumId", API.ColumnName "AlbumId")]
                                   }
                               )
                             ]
                       },
                     API.TableRelationships
                       { _trSourceTable = mkTableName "Album",
                         _trRelationships =
                           HashMap.fromList
                             [ ( API.RelationshipName "Artist",
                                 API.Relationship
                                   { _rTargetTable = mkTableName "Artist",
                                     _rRelationshipType = API.ObjectRelationship,
                                     _rColumnMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
                                   }
                               )
                             ]
                       }
                   ]
        )

  mockAgentGraphqlTest "works with an order by that navigates a relationship with table permissions" $ \_testEnv performGraphqlRequest -> do
    let headers = [("X-Hasura-Role", testRoleName)]
    let graphqlRequest =
          [graphql|
            query getEmployee {
              Employee(limit: 1, order_by: {SupportRepForCustomers_aggregate: {count: desc}}) {
                EmployeeId
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("EmployeeId", API.mkColumnFieldValue $ Aeson.Number 3)
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Employee:
            - EmployeeId: 3
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query $
            mkQueryRequest
              (mkTableName "Employee")
              ( emptyQuery
                  & API.qFields ?~ mkFieldsMap [("EmployeeId", API.ColumnField (API.ColumnName "EmployeeId") $ API.ScalarType "number")]
                  & API.qLimit ?~ 1
                  & API.qWhere
                    ?~ API.Exists
                      (API.RelatedTable $ API.RelationshipName "SupportRepForCustomers")
                      ( API.ApplyBinaryComparisonOperator
                          API.Equal
                          (API.ComparisonColumn API.CurrentTable (API.ColumnName "Country") $ API.ScalarType "string")
                          (API.AnotherColumnComparison (API.ComparisonColumn API.QueryTable (API.ColumnName "Country") $ API.ScalarType "string"))
                      )
                  & API.qOrderBy
                    ?~ API.OrderBy
                      ( HashMap.fromList
                          [ ( API.RelationshipName "SupportRepForCustomers",
                              API.OrderByRelation
                                ( Just $
                                    API.Exists (API.RelatedTable $ API.RelationshipName "SupportRep") $
                                      API.ApplyBinaryComparisonOperator
                                        API.Equal
                                        (API.ComparisonColumn API.CurrentTable (API.ColumnName "Country") $ API.ScalarType "string")
                                        (API.AnotherColumnComparison (API.ComparisonColumn API.QueryTable (API.ColumnName "Country") $ API.ScalarType "string"))
                                )
                                mempty
                            )
                          ]
                      )
                      (API.OrderByElement [API.RelationshipName "SupportRepForCustomers"] API.OrderByStarCountAggregate API.Descending :| [])
              )
              & API.qrTableRelationships
                .~ [ API.TableRelationships
                       { _trSourceTable = mkTableName "Customer",
                         _trRelationships =
                           HashMap.fromList
                             [ ( API.RelationshipName "SupportRep",
                                 API.Relationship
                                   { _rTargetTable = mkTableName "Employee",
                                     _rRelationshipType = API.ObjectRelationship,
                                     _rColumnMapping = HashMap.fromList [(API.ColumnName "SupportRepId", API.ColumnName "EmployeeId")]
                                   }
                               )
                             ]
                       },
                     API.TableRelationships
                       { _trSourceTable = mkTableName "Employee",
                         _trRelationships =
                           HashMap.fromList
                             [ ( API.RelationshipName "SupportRepForCustomers",
                                 API.Relationship
                                   { _rTargetTable = mkTableName "Customer",
                                     _rRelationshipType = API.ArrayRelationship,
                                     _rColumnMapping = HashMap.fromList [(API.ColumnName "EmployeeId", API.ColumnName "SupportRepId")]
                                   }
                               )
                             ]
                       }
                   ]
        )
