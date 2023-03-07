{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.RemoteRelationshipsSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentGraphqlTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..))
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (HasCallStack, SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [ -- First set up the mock server source, which will be the remote relationship target
                  Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv),
                  -- Then set up the postgres source which will be the remote relationship source
                  Fixture.SetupAction (setupPostgres testEnv) (const (teardownPostgres testEnv))
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [Album]
        configuration: {}
      |]

postgresTables :: [Table]
postgresTables =
  [ (Schema.table "PgArtist")
      { tableColumns =
          [ Schema.column "ArtistId" Schema.TInt,
            Schema.column "Name" Schema.TStr
          ],
        tablePrimaryKey = ["ArtistId"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "AC/DC"],
            [Schema.VInt 2, Schema.VStr "Accept"]
          ]
      },
    (Schema.table "PgTrack")
      { tableColumns =
          [ Schema.column "TrackId" Schema.TInt,
            Schema.column "Name" Schema.TStr,
            Schema.column "AlbumId" Schema.TInt
          ],
        tablePrimaryKey = ["TrackId"],
        tableData =
          [ [Schema.VInt 6, Schema.VStr "Put The Finger On You", Schema.VInt 1],
            [Schema.VInt 16, Schema.VStr "Dog Eat Dog", Schema.VInt 4],
            [Schema.VInt 3, Schema.VStr "Fast As a Shark", Schema.VInt 3]
          ]
      }
  ]

setupPostgres :: HasCallStack => TestEnvironment -> IO ()
setupPostgres testEnv = do
  let pgSourceName :: String = "pg_source"
      mockAgentSourceName = BackendType.backendSourceName Mock.backendTypeMetadata
      sourceConfig = Postgres.defaultSourceConfiguration testEnv
      schemaName = Schema.getSchemaName testEnv

  Postgres.createDatabase testEnv

  GraphqlEngine.postMetadata_
    testEnv
    [yaml|
      type: pg_add_source
      args:
        name: *pgSourceName
        configuration: *sourceConfig
    |]

  for_ postgresTables $ \table -> do
    Postgres.createTable testEnv table
    Postgres.insertTable testEnv table

    GraphqlEngine.postMetadata_
      testEnv
      [interpolateYaml|
        type: pg_track_table
        args:
          source: #{pgSourceName}
          table:
            schema: #{schemaName}
            name: #{tableName table}
      |]

  -- Postgres.PgArtist -> MockAgent.Album array relationship
  GraphqlEngine.postMetadata_
    testEnv
    [yaml|
      type: pg_create_remote_relationship
      args:
        source: *pgSourceName
        table:
          schema: *schemaName
          name: PgArtist
        name: RemoteAlbums
        definition:
          to_source:
            source: *mockAgentSourceName
            table: [Album]
            relationship_type: array
            field_mapping:
              ArtistId: ArtistId
    |]

  -- Postgres.PgTrack -> MockAgent.Album object relationship
  GraphqlEngine.postMetadata_
    testEnv
    [yaml|
      type: pg_create_remote_relationship
      args:
        source: *pgSourceName
        table:
          schema: *schemaName
          name: PgTrack
        name: RemoteAlbum
        definition:
          to_source:
            source: *mockAgentSourceName
            table: [Album]
            relationship_type: object
            field_mapping:
              AlbumId: AlbumId
    |]

teardownPostgres :: TestEnvironment -> IO ()
teardownPostgres testEnv = do
  Postgres.dropDatabase testEnv

--------------------------------------------------------------------------------

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests _opts = describe "Remote Relationships Tests" $ do
  mockAgentGraphqlTest "can act as the target of a remote array relationship" $ \testEnv performGraphqlRequest -> do
    let pgSchemaName = Schema.getSchemaName testEnv
    let headers = []
    let graphqlRequest =
          [graphql|
            query getArtist {
              PgArtist: #{pgSchemaName}_PgArtist(order_by: {ArtistId: asc}) {
                ArtistId
                Name
                RemoteAlbums {
                  AlbumId
                  Title
                }
              }
            }
          |]
    let queryResponse =
          mkRowsResponse
            [ [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkRowsResponse
                      [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 1),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
                        ],
                        [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 4),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Let There Be Rock")
                        ]
                      ]
                )
              ],
              [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkRowsResponse
                      [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 2),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Balls to the Wall")
                        ],
                        [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 3),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Restless and Wild")
                        ]
                      ]
                )
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          PgArtist:
            - ArtistId: 1
              Name: AC/DC
              RemoteAlbums:
                - AlbumId: 1
                  Title: For Those About To Rock We Salute You
                - AlbumId: 4
                  Title: Let There Be Rock
            - ArtistId: 2
              Name: Accept
              RemoteAlbums:
                - AlbumId: 2
                  Title: Balls to the Wall
                - AlbumId: 3
                  Title: Restless and Wild
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
                            [ (API.FieldName "AlbumId", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                              (API.FieldName "Title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Nothing,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Nothing
                    },
                _qrForeach =
                  Just $
                    NonEmpty.fromList
                      [ HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (Aeson.Number 1) (API.ScalarType "number"))],
                        HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (Aeson.Number 2) (API.ScalarType "number"))]
                      ]
              }
        )

  mockAgentGraphqlTest "can act as the target of a remote object relationship" $ \testEnv performGraphqlRequest -> do
    let pgSchemaName = Schema.getSchemaName testEnv
    let headers = []
    let graphqlRequest =
          [graphql|
            query getTrack {
              PgTrack: #{pgSchemaName}_PgTrack(order_by: {TrackId: asc}) {
                TrackId
                Name
                RemoteAlbum {
                  AlbumId
                  Title
                }
              }
            }
          |]
    let queryResponse =
          mkRowsResponse
            [ [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkRowsResponse
                      [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 3),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Restless and Wild")
                        ]
                      ]
                )
              ],
              [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkRowsResponse
                      [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 1),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
                        ]
                      ]
                )
              ],
              [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkRowsResponse
                      [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ Aeson.Number 4),
                          (API.FieldName "Title", API.mkColumnFieldValue $ Aeson.String "Let There Be Rock")
                        ]
                      ]
                )
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          PgTrack:
            - TrackId: 3
              Name: Fast As a Shark
              RemoteAlbum:
                AlbumId: 3
                Title: Restless and Wild
            - TrackId: 6
              Name: Put The Finger On You
              RemoteAlbum:
                AlbumId: 1
                Title: For Those About To Rock We Salute You
            - TrackId: 16
              Name: Dog Eat Dog
              RemoteAlbum:
                AlbumId: 4
                Title: Let There Be Rock
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
                            [ (API.FieldName "AlbumId", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                              (API.FieldName "Title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                            ],
                      _qAggregates = Nothing,
                      _qLimit = Nothing,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Nothing
                    },
                _qrForeach =
                  Just $
                    NonEmpty.fromList
                      [ HashMap.fromList [(API.ColumnName "AlbumId", API.ScalarValue (Aeson.Number 3) (API.ScalarType "number"))],
                        HashMap.fromList [(API.ColumnName "AlbumId", API.ScalarValue (Aeson.Number 1) (API.ScalarType "number"))],
                        HashMap.fromList [(API.ColumnName "AlbumId", API.ScalarValue (Aeson.Number 4) (API.ScalarType "number"))]
                      ]
              }
        )

  mockAgentGraphqlTest "can act as the target of an aggregation over a remote array relationship" $ \testEnv performGraphqlRequest -> do
    let pgSchemaName = Schema.getSchemaName testEnv
    let headers = []
    let graphqlRequest =
          [graphql|
            query getArtist {
              PgArtist: #{pgSchemaName}_PgArtist(order_by: {ArtistId: asc}) {
                ArtistId
                Name
                RemoteAlbums_aggregate {
                  aggregate {
                    count
                  }
                  nodes {
                    AlbumId
                    Title
                  }
                }
              }
            }
          |]
    let queryResponse =
          mkRowsResponse
            [ [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkQueryResponse
                      [ [ (API.FieldName "nodes_AlbumId", API.mkColumnFieldValue $ Aeson.Number 1),
                          (API.FieldName "nodes_Title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
                        ],
                        [ (API.FieldName "nodes_AlbumId", API.mkColumnFieldValue $ Aeson.Number 4),
                          (API.FieldName "nodes_Title", API.mkColumnFieldValue $ Aeson.String "Let There Be Rock")
                        ]
                      ]
                      [ (API.FieldName "aggregate_count", Aeson.Number 2)
                      ]
                )
              ],
              [ ( API.FieldName "query",
                  API.mkRelationshipFieldValue $
                    mkQueryResponse
                      [ [ (API.FieldName "nodes_AlbumId", API.mkColumnFieldValue $ Aeson.Number 2),
                          (API.FieldName "nodes_Title", API.mkColumnFieldValue $ Aeson.String "Balls to the Wall")
                        ],
                        [ (API.FieldName "nodes_AlbumId", API.mkColumnFieldValue $ Aeson.Number 3),
                          (API.FieldName "nodes_Title", API.mkColumnFieldValue $ Aeson.String "Restless and Wild")
                        ]
                      ]
                      [ (API.FieldName "aggregate_count", Aeson.Number 2)
                      ]
                )
              ]
            ]
    let mockConfig = Mock.chinookMock & mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          PgArtist:
            - ArtistId: 1
              Name: AC/DC
              RemoteAlbums_aggregate:
                aggregate:
                  count: 2
                nodes:
                  - AlbumId: 1
                    Title: For Those About To Rock We Salute You
                  - AlbumId: 4
                    Title: Let There Be Rock
            - ArtistId: 2
              Name: Accept
              RemoteAlbums_aggregate:
                aggregate:
                  count: 2
                nodes:
                  - AlbumId: 2
                    Title: Balls to the Wall
                  - AlbumId: 3
                    Title: Restless and Wild
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
                            [ (API.FieldName "nodes_AlbumId", API.ColumnField (API.ColumnName "AlbumId") $ API.ScalarType "number"),
                              (API.FieldName "nodes_Title", API.ColumnField (API.ColumnName "Title") $ API.ScalarType "string")
                            ],
                      _qAggregates =
                        Just $
                          HashMap.fromList
                            [(API.FieldName "aggregate_count", API.StarCount)],
                      _qLimit = Nothing,
                      _qOffset = Nothing,
                      _qWhere = Nothing,
                      _qOrderBy = Nothing
                    },
                _qrForeach =
                  Just $
                    NonEmpty.fromList
                      [ HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (Aeson.Number 1) (API.ScalarType "number"))],
                        HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (Aeson.Number 2) (API.ScalarType "number"))]
                      ]
              }
        )

mkRowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
mkRowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing

mkQueryResponse :: [[(API.FieldName, API.FieldValue)]] -> [(API.FieldName, Aeson.Value)] -> API.QueryResponse
mkQueryResponse rows aggregates = API.QueryResponse (Just $ HashMap.fromList <$> rows) (Just $ HashMap.fromList aggregates)
