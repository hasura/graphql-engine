{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.RemoteRelationshipsSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((.~), (?~), _Just)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockConfig, MockRequestResults (..), mockAgentGraphqlTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..))
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec (HasCallStack, SpecWith, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec = describe "Remote Relationships Tests" $ do
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [ -- First set up the mock server source, which will be the remote relationship target
                  Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv),
                  -- Then set up the postgres source which will be the remote relationship source
                  -- including registering the remote relationships
                  Fixture.SetupAction (setupPostgres testEnv >> registerRemoteRelationships testEnv) (const (teardownPostgres testEnv))
                ]
            }
        ]
    )
    tests

  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment' chinookMockThatDoesNotSupportForeachQueries,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [ -- First set up the mock server source, which will be the remote relationship target
                  Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv),
                  -- Then set up the postgres source which will be the remote relationship source
                  Fixture.SetupAction (setupPostgres testEnv) (const (teardownPostgres testEnv))
                ]
            }
        ]
    )
    errorTests
  where
    chinookMockThatDoesNotSupportForeachQueries :: MockConfig
    chinookMockThatDoesNotSupportForeachQueries =
      Mock.chinookMock {Mock._capabilitiesResponse = Mock._capabilitiesResponse Mock.chinookMock & API.crCapabilities . API.cQueries . _Just . API.qcForeach .~ Nothing}

--------------------------------------------------------------------------------

sourceMetadata :: J.Value
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

pgSourceName :: String
pgSourceName = "pg_source"

setupPostgres :: (HasCallStack) => TestEnvironment -> IO ()
setupPostgres testEnv = do
  let sourceConfig = Postgres.defaultSourceConfiguration testEnv
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

registerRemoteRelationships :: (HasCallStack) => TestEnvironment -> IO ()
registerRemoteRelationships testEnv = do
  let mockAgentSourceName = BackendType.backendSourceName Mock.backendTypeMetadata
      schemaName = Schema.getSchemaName testEnv

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

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = do
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
          mkRowsQueryResponse
            [ [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 1),
                          ("Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
                        ],
                        [ ("AlbumId", API.mkColumnFieldValue $ J.Number 4),
                          ("Title", API.mkColumnFieldValue $ J.String "Let There Be Rock")
                        ]
                      ]
                )
              ],
              [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 2),
                          ("Title", API.mkColumnFieldValue $ J.String "Balls to the Wall")
                        ],
                        [ ("AlbumId", API.mkColumnFieldValue $ J.Number 3),
                          ("Title", API.mkColumnFieldValue $ J.String "Restless and Wild")
                        ]
                      ]
                )
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

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
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                      ("Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)
                    ]
              )
            & API.qrForeach
            ?~ NonEmpty.fromList
              [ HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (J.Number 1) (API.ScalarType "number"))],
                HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (J.Number 2) (API.ScalarType "number"))]
              ]
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
          mkRowsQueryResponse
            [ [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 3),
                          ("Title", API.mkColumnFieldValue $ J.String "Restless and Wild")
                        ]
                      ]
                )
              ],
              [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 1),
                          ("Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
                        ]
                      ]
                )
              ],
              [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkRowsQueryResponse
                      [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 4),
                          ("Title", API.mkColumnFieldValue $ J.String "Let There Be Rock")
                        ]
                      ]
                )
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

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
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                      ("Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)
                    ]
              )
            & API.qrForeach
            ?~ NonEmpty.fromList
              [ HashMap.fromList [(API.ColumnName "AlbumId", API.ScalarValue (J.Number 3) (API.ScalarType "number"))],
                HashMap.fromList [(API.ColumnName "AlbumId", API.ScalarValue (J.Number 1) (API.ScalarType "number"))],
                HashMap.fromList [(API.ColumnName "AlbumId", API.ScalarValue (J.Number 4) (API.ScalarType "number"))]
              ]
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
          mkRowsQueryResponse
            [ [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkQueryResponse
                      [ [ ("nodes_AlbumId", API.mkColumnFieldValue $ J.Number 1),
                          ("nodes_Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
                        ],
                        [ ("nodes_AlbumId", API.mkColumnFieldValue $ J.Number 4),
                          ("nodes_Title", API.mkColumnFieldValue $ J.String "Let There Be Rock")
                        ]
                      ]
                      [ ("aggregate_count", J.Number 2)
                      ]
                )
              ],
              [ ( "query",
                  API.mkRelationshipFieldValue
                    $ mkQueryResponse
                      [ [ ("nodes_AlbumId", API.mkColumnFieldValue $ J.Number 2),
                          ("nodes_Title", API.mkColumnFieldValue $ J.String "Balls to the Wall")
                        ],
                        [ ("nodes_AlbumId", API.mkColumnFieldValue $ J.Number 3),
                          ("nodes_Title", API.mkColumnFieldValue $ J.String "Restless and Wild")
                        ]
                      ]
                      [ ("aggregate_count", J.Number 2)
                      ]
                )
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

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
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("nodes_AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                      ("nodes_Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)
                    ]
                    & API.qAggregates
                  ?~ mkFieldsMap [("aggregate_count", API.StarCount)]
              )
            & API.qrForeach
            ?~ NonEmpty.fromList
              [ HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (J.Number 1) (API.ScalarType "number"))],
                HashMap.fromList [(API.ColumnName "ArtistId", API.ScalarValue (J.Number 2) (API.ScalarType "number"))]
              ]
        )

  mockAgentGraphqlTest "evaluates a Postgres remote-rel select-permission filter targeting a DC source (getColVals + valid IN cast)" $ \testEnv performGraphqlRequest -> do
    let pgSchemaName = Schema.getSchemaName testEnv
    -- Add a select permission on the Postgres PgTrack table whose row filter
    -- traverses the RemoteAlbum remote relationship into the mock Data Connector
    -- source. Evaluating this permission drives `getColVals @'DataConnector'` and
    -- rewrites the filter to `PgTrack.AlbumId IN (<fetched values>)` on the
    -- Postgres query. Before the B2 fix the outer IN literals were annotated with
    -- the RHS (Data Connector) scalar type name (e.g. `'1'::number`), which is not
    -- a valid Postgres type and made this query fail (Zendesk #15062); the fix
    -- annotates them with the LHS (Postgres) column's type instead.
    GraphqlEngine.postMetadata_
      testEnv
      [interpolateYaml|
        type: pg_create_select_permission
        args:
          source: #{pgSourceName}
          table:
            schema: #{pgSchemaName}
            name: PgTrack
          role: test-role
          permission:
            columns:
              - TrackId
              - Name
              - AlbumId
            filter:
              RemoteAlbum:
                AlbumId:
                  _eq: X-Hasura-AlbumId
      |]

    let headers =
          [ ("X-Hasura-Role", "test-role"),
            ("X-Hasura-AlbumId", "1")
          ]
    let graphqlRequest =
          [graphql|
            query getTracks {
              PgTrack: #{pgSchemaName}_PgTrack {
                TrackId
                AlbumId
              }
            }
          |]
    -- The mock serves only the getColVals fetch (the main query runs on Postgres);
    -- returning AlbumId = 1 makes the permission resolve to `PgTrack.AlbumId IN (1)`.
    let queryResponse =
          mkRowsQueryResponse
            [ [("AlbumId", API.mkColumnFieldValue $ J.Number 1)]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    -- (1) The Postgres query executes (no invalid `::<dc-scalar-type>` cast) and
    -- returns only the permitted rows (AlbumId = 1 -> TrackId 6).
    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          PgTrack:
            - TrackId: 6
              AlbumId: 1
      |]

    -- (2) The only agent request is the single-column, filtered getColVals fetch
    -- (no foreach): SELECT AlbumId FROM Album WHERE AlbumId = '1'.
    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                    ]
                    & API.qWhere
                  ?~ API.ApplyBinaryComparisonOperator
                    API.Equal
                    (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.ScalarType "number") Nothing)
                    (API.ScalarValueComparison $ API.ScalarValue (J.String "1") (API.ScalarType "number"))
              )
        )

  mockAgentGraphqlTest "returns no rows for a Postgres remote-rel select-permission filter targeting a DC source when getColVals finds no matches" $ \testEnv performGraphqlRequest -> do
    let pgSchemaName = Schema.getSchemaName testEnv
    -- Same shape of permission as the previous test, registered on a separate
    -- role so this test is self-contained and doesn't depend on execution
    -- order. Confirms the empty-match case (Zendesk #15062: "filter matches
    -- nothing" should return `[]`, not a 500).
    GraphqlEngine.postMetadata_
      testEnv
      [interpolateYaml|
        type: pg_create_select_permission
        args:
          source: #{pgSourceName}
          table:
            schema: #{pgSchemaName}
            name: PgTrack
          role: test-role-nomatch
          permission:
            columns:
              - TrackId
              - Name
              - AlbumId
            filter:
              RemoteAlbum:
                AlbumId:
                  _eq: X-Hasura-AlbumId
      |]

    let headers =
          [ ("X-Hasura-Role", "test-role-nomatch"),
            ("X-Hasura-AlbumId", "999")
          ]
    let graphqlRequest =
          [graphql|
            query getTracks {
              PgTrack: #{pgSchemaName}_PgTrack {
                TrackId
                AlbumId
              }
            }
          |]
    -- getColVals fetches no AlbumId values for "999", so the permission
    -- rewrites to an empty IN and the Postgres query returns nothing.
    let queryResponse = mkRowsQueryResponse []
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          PgTrack: []
      |]

  mockAgentGraphqlTest "admin (no role) bypasses the remote-rel permission filter entirely" $ \testEnv performGraphqlRequest -> do
    let pgSchemaName = Schema.getSchemaName testEnv
    let headers = []
    let graphqlRequest =
          [graphql|
            query getTracks {
              PgTrack: #{pgSchemaName}_PgTrack(order_by: {TrackId: asc}) {
                TrackId
                AlbumId
              }
            }
          |]
    -- Admin bypasses select permissions entirely, so no getColVals fetch is
    -- ever issued -- the query runs directly on Postgres and returns every
    -- row (Zendesk #15062: "Admin (no role header): all rows, unfiltered").
    let mockConfig = mockQueryResponse (mkRowsQueryResponse [])

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          PgTrack:
            - TrackId: 3
              AlbumId: 3
            - TrackId: 6
              AlbumId: 1
            - TrackId: 16
              AlbumId: 4
      |]

    -- No agent request is recorded: the DC mock is never called for admin.
    _mrrRecordedRequest `shouldBe` Nothing

errorTests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
errorTests = do
  it "creating a remote relationship returns an error when it is unsupported by the target" $ \(testEnv, _) -> do
    let mockAgentSourceName = BackendType.backendSourceName Mock.backendTypeMetadata
        schemaName = Schema.getSchemaName testEnv

    shouldReturnYaml
      testEnv
      ( GraphqlEngine.postMetadataWithStatus
          400
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
      )
      [interpolateYaml|
        code: invalid-configuration
        error: 'Inconsistent object: in table "#{schemaName}.PgArtist": in remote relationship "RemoteAlbums":
          source #{mockAgentSourceName} does not support being used as the target of a remote relationship'
        internal:
        - definition:
            definition:
              to_source:
                field_mapping:
                  ArtistId: ArtistId
                relationship_type: array
                source: #{mockAgentSourceName}
                table:
                - Album
            name: RemoteAlbums
            source: #{pgSourceName}
            table:
              name: PgArtist
              schema: #{schemaName}
          name: remote_relationship RemoteAlbums in table #{schemaName}.PgArtist in source #{pgSourceName}
          reason: 'Inconsistent object: in table "#{schemaName}.PgArtist": in remote relationship "RemoteAlbums":
            source #{mockAgentSourceName} does not support being used as the target of a remote relationship'
          type: remote_relationship
        path: $.args
      |]
