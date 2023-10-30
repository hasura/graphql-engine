{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.QueryRelationshipsSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockConfig, MockRequestResults (..), mockAgentGraphqlTest, mockAgentMetadataTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml (interpolateYaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec = do
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
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment' noRelationshipsCapabilityMockConfig,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction noRelationshipsCapabilitySourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    noRelationshipsCapabilityTests

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
            [ [ ("Name", API.mkColumnFieldValue $ J.String "For Those About To Rock (We Salute You)"),
                ( "Genre",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [("Name", API.mkColumnFieldValue $ J.String "Rock")]
                      ]
                ),
                ( "MediaType",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [("Name", API.mkColumnFieldValue $ J.String "MPEG audio file")]
                      ]
                )
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

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
        ( Query
            $ mkTableRequest
              (mkTableName "Track")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing),
                      ( "Genre",
                        API.RelField
                          ( API.RelationshipField
                              (API.RelationshipName "Genre")
                              (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)])
                          )
                      ),
                      ( "MediaType",
                        API.RelField
                          ( API.RelationshipField
                              (API.RelationshipName "MediaType")
                              (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)])
                          )
                      )
                    ]
                    & API.qLimit
                  ?~ 1
              )
            & API.qrRelationships
            .~ Set.fromList
              [ API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Track",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Genre",
                              API.Relationship
                                { _rTarget = mkTableTarget "Genre",
                                  _rRelationshipType = API.ObjectRelationship,
                                  _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "GenreId", API.mkColumnSelector $ API.ColumnName "GenreId")]
                                }
                            ),
                            ( API.RelationshipName "MediaType",
                              API.Relationship
                                { _rTarget = mkTableTarget "MediaType",
                                  _rRelationshipType = API.ObjectRelationship,
                                  _rColumnMapping =
                                    API.ColumnPathMapping
                                      $ HashMap.fromList
                                        [(API.mkColumnSelector $ API.ColumnName "MediaTypeId", API.mkColumnSelector $ API.ColumnName "MediaTypeId")]
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
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [ ( "Artist",
                            API.mkRelationshipFieldValue
                              $ mkRowsQueryResponse
                                [[("Name", API.mkColumnFieldValue $ J.String "Zeca Pagodinho")]]
                          )
                        ]
                      ]
                ),
                ("Name", API.mkColumnFieldValue $ J.String "Camarão que Dorme e Onda Leva")
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

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
        ( Query
            $ mkTableRequest
              (mkTableName "Track")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing),
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
                                              (emptyQuery & API.qFields ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)])
                                          )
                                      )
                                    ]
                              )
                          )
                      )
                    ]
                    & API.qLimit
                  ?~ 1
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
                        [ API.OrderByElement [API.RelationshipName "Album", API.RelationshipName "Artist"] (API.OrderByColumn (API.mkColumnSelector $ API.ColumnName "Name") Nothing) API.Descending,
                          API.OrderByElement [] (API.OrderByColumn (API.mkColumnSelector $ API.ColumnName "Name") Nothing) API.Ascending
                        ]
                    )
              )
            & API.qrRelationships
            .~ Set.fromList
              [ API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Track",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Album",
                              API.Relationship
                                { _rTarget = mkTableTarget "Album",
                                  _rRelationshipType = API.ObjectRelationship,
                                  _rColumnMapping =
                                    API.ColumnPathMapping
                                      $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "AlbumId", API.mkColumnSelector $ API.ColumnName "AlbumId")]
                                }
                            )
                          ]
                    },
                API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Album",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Artist",
                              API.Relationship
                                { _rTarget = mkTableTarget "Artist",
                                  _rRelationshipType = API.ObjectRelationship,
                                  _rColumnMapping =
                                    API.ColumnPathMapping
                                      $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
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
            [ [ ("EmployeeId", API.mkColumnFieldValue $ J.Number 3)
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Employee:
            - EmployeeId: 3
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Employee")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap [("EmployeeId", API.ColumnField (API.ColumnName "EmployeeId") (API.ScalarType "number") Nothing)]
                    & API.qLimit
                  ?~ 1
                    & API.qWhere
                  ?~ API.Exists
                    (API.RelatedTable $ API.RelationshipName "SupportRepForCustomers")
                    ( API.ApplyBinaryComparisonOperator
                        API.Equal
                        (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "Country") (API.ScalarType "string") Nothing)
                        (API.AnotherColumnComparison (API.ComparisonColumn API.QueryTable (API.mkColumnSelector $ API.ColumnName "Country") (API.ScalarType "string") Nothing))
                    )
                    & API.qOrderBy
                  ?~ API.OrderBy
                    ( HashMap.fromList
                        [ ( API.RelationshipName "SupportRepForCustomers",
                            API.OrderByRelation
                              ( Just
                                  $ API.Exists (API.RelatedTable $ API.RelationshipName "SupportRep")
                                  $ API.ApplyBinaryComparisonOperator
                                    API.Equal
                                    (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "Country") (API.ScalarType "string") Nothing)
                                    (API.AnotherColumnComparison (API.ComparisonColumn API.QueryTable (API.mkColumnSelector $ API.ColumnName "Country") (API.ScalarType "string") Nothing))
                              )
                              mempty
                          )
                        ]
                    )
                    (API.OrderByElement [API.RelationshipName "SupportRepForCustomers"] API.OrderByStarCountAggregate API.Descending :| [])
              )
            & API.qrRelationships
            .~ Set.fromList
              [ API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Customer",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "SupportRep",
                              API.Relationship
                                { _rTarget = mkTableTarget "Employee",
                                  _rRelationshipType = API.ObjectRelationship,
                                  _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "SupportRepId", API.mkColumnSelector $ API.ColumnName "EmployeeId")]
                                }
                            )
                          ]
                    },
                API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Employee",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "SupportRepForCustomers",
                              API.Relationship
                                { _rTarget = mkTableTarget "Customer",
                                  _rRelationshipType = API.ArrayRelationship,
                                  _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "EmployeeId", API.mkColumnSelector $ API.ColumnName "SupportRepId")]
                                }
                            )
                          ]
                    }
              ]
        )

--------------------------------------------------------------------------------

noRelationshipsCapabilityMockConfig :: MockConfig
noRelationshipsCapabilityMockConfig =
  Mock.chinookMock
    { Mock._capabilitiesResponse =
        Mock._capabilitiesResponse Mock.chinookMock
          & API.crCapabilities
          . API.cRelationships
          .~ Nothing -- Remove relationships capability
    }

noRelationshipsCapabilitySourceMetadata :: J.Value
noRelationshipsCapabilitySourceMetadata =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [Album]
          - table: [Artist]
        configuration: {}
      |]

--------------------------------------------------------------------------------

noRelationshipsCapabilityTests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
noRelationshipsCapabilityTests = describe "No Relationships Capability Tests" $ do
  mockAgentMetadataTest "create object relationship returns error" $ \testEnvironment performMetadataRequest -> do
    let backendTypeMetadata = fromMaybe (error "Missing backend type config") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata
        sourceString = BackendType.backendSourceName backendTypeMetadata

    let request =
          [interpolateYaml|
            type: #{backendType}_create_object_relationship
            args:
              source: #{sourceString}
              table: [Album]
              name: Artist
              using:
                manual_configuration:
                  remote_table: [Artist]
                  column_mapping:
                    ArtistId: ArtistId
          |]

    let expectedStatusCode = 400
    MockRequestResults {..} <- performMetadataRequest Mock.defaultMockRequestConfig expectedStatusCode request

    _mrrResponse
      `shouldBe` [interpolateYaml|
        path: $.args
        code: invalid-configuration
        error: "Inconsistent object: in table \"Album\": in relationship \"Artist\": Local object and array relationships are not supported for '#{sourceString}'. Instead consider using remote relationships to join between tables on the same source."
        internal:
          - definition:
              source: #{sourceString}
              table: [Album]
              name: Artist
              using:
                manual_configuration:
                  remote_table: [Artist]
                  column_mapping:
                    ArtistId: ArtistId
                  insertion_order: null
              comment: null
            name: object_relation Artist in table Album in source #{sourceString}
            reason: "Inconsistent object: in table \"Album\": in relationship \"Artist\": Local object and array relationships are not supported for '#{sourceString}'. Instead consider using remote relationships to join between tables on the same source."
            type: object_relation
      |]

  mockAgentMetadataTest "create array relationship returns error" $ \testEnvironment performMetadataRequest -> do
    let backendTypeMetadata = fromMaybe (error "Missing backend type config") $ getBackendTypeConfig testEnvironment
        backendType = BackendType.backendTypeString backendTypeMetadata
        sourceString = BackendType.backendSourceName backendTypeMetadata

    let request =
          [interpolateYaml|
            type: #{backendType}_create_array_relationship
            args:
              source: #{sourceString}
              table: [Artist]
              name: Albums
              using:
                manual_configuration:
                  remote_table: [Album]
                  column_mapping:
                    ArtistId: ArtistId
          |]

    let expectedStatusCode = 400
    MockRequestResults {..} <- performMetadataRequest Mock.defaultMockRequestConfig expectedStatusCode request

    _mrrResponse
      `shouldBe` [interpolateYaml|
        path: $.args
        code: invalid-configuration
        error: "Inconsistent object: in table \"Artist\": in relationship \"Albums\": Local object and array relationships are not supported for '#{sourceString}'. Instead consider using remote relationships to join between tables on the same source."
        internal:
          - definition:
              source: #{sourceString}
              table: [Artist]
              name: Albums
              using:
                manual_configuration:
                  remote_table: [Album]
                  column_mapping:
                    ArtistId: ArtistId
                  insertion_order: null
              comment: null
            name: array_relation Albums in table Artist in source #{sourceString}
            reason: "Inconsistent object: in table \"Artist\": in relationship \"Albums\": Local object and array relationships are not supported for '#{sourceString}'. Instead consider using remote relationships to join between tables on the same source."
            type: array_relation
      |]
