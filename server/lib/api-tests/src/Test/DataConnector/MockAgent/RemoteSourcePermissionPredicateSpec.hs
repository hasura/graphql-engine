{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Mock-agent test for evaluating a remote-relationship predicate in a SELECT
-- permission where the /target (RHS) of the relationship is a Data Connector
-- source. This exercises the @getColVals \@'DataConnector'@ implementation added
-- for Zendesk #15062 (see @Hasura.Backends.DataConnector.Adapter.ColumnValues@).
--
-- Topology: a single mock (Data Connector) source with an @Album@ table (LHS)
-- and an @Artist@ table (RHS), joined by a same-source @to_source@ remote
-- relationship @RemoteArtist@ (Album.ArtistId -> Artist.ArtistId). This is the
-- pattern HGE recommends for joins within a Data Connector source (see
-- @ifSupportsLocalRelationships@ in the DC adapter). A select permission on
-- @Album@ filters via the remote relationship:
--
-- >   filter: { RemoteArtist: { ArtistId: { _eq: X-Hasura-ArtistId } } }
--
-- When the @Album@ table is queried as that role, HGE evaluates the permission
-- by calling @getColVals \@'DataConnector'@, which issues a single-column,
-- filtered @QueryRequest@ (no @foreach@) to the mock agent to fetch the matching
-- @Artist.ArtistId@ values, then rewrites the permission into
-- @Album.ArtistId IN (<values>)@ on the main query.
--
-- Before the fix, @getColVals@ for the Data Connector backend was a
-- @throw500 "getColVals: not implemented for the Data Connector backend"@, so
-- this query failed at runtime. This test asserts (1) the query now succeeds and
-- returns the expected rows, and (2) the main query the agent receives carries
-- the @IN@ filter whose values were produced by @getColVals@.
--
-- NOTE: The DC-source LHS path is used deliberately: the outer @IN@ list is built
-- by the Data Connector query plan, which is valid. (The Postgres-LHS -> DC-RHS
-- path annotates the @IN@ literals with the RHS scalar type name via
-- 'textToPGScalarType', which is not a valid Postgres type for non-Postgres
-- scalar types like @number@ -- a separate gap tracked outside this test.)
module Test.DataConnector.MockAgent.RemoteSourcePermissionPredicateSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.ByteString (ByteString)
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

sourceMetadata :: J.Value
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
            remote_relationships:
              - name: RemoteArtist
                definition:
                  to_source:
                    source: *source
                    table: [Artist]
                    relationship_type: object
                    field_mapping:
                      ArtistId: ArtistId
            select_permissions:
              - role: *testRoleName
                permission:
                  columns:
                    - AlbumId
                    - Title
                    - ArtistId
                  filter:
                    RemoteArtist:
                      ArtistId:
                        _eq: X-Hasura-ArtistId
          - table: [Artist]
        configuration: {}
      |]

--------------------------------------------------------------------------------

-- | The @where@ expression of the recorded main query. The mock records the
-- /last/ request it receives; the @getColVals@ fetch happens first (during
-- planning) and the main @Album@ query second, so this returns the main query's
-- filter -- which should be the @IN@ built from the values @getColVals@ fetched.
recordedQueryWhere :: Maybe AgentRequest -> Maybe API.Expression
recordedQueryWhere (Just (Query queryRequest)) = API._qWhere (API._qrQuery queryRequest)
recordedQueryWhere _ = Nothing

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Remote source relationship permission predicate (DC RHS)" $ do
  mockAgentGraphqlTest "evaluates a remote-rel select-permission filter via getColVals" $ \_testEnv performGraphqlRequest -> do
    let headers =
          [ ("X-Hasura-Role", testRoleName),
            ("X-Hasura-ArtistId", "1")
          ]
    let graphqlRequest =
          [graphql|
            query getAlbums {
              albums {
                AlbumId
                Title
              }
            }
          |]
    -- A single canned response serves both agent requests (the mock ignores the
    -- WHERE and returns these rows for any query): the getColVals fetch reads the
    -- "ArtistId" column, the main Album query reads "AlbumId"/"Title".
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 1),
                ("Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You"),
                ("ArtistId", API.mkColumnFieldValue $ J.Number 1)
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    -- (1) The query succeeds (pre-fix this failed with
    -- "getColVals: not implemented for the Data Connector backend").
    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          albums:
            - AlbumId: 1
              Title: For Those About To Rock We Salute You
      |]

    -- (2) The main query the agent received carries the IN filter whose values
    -- were produced by getColVals (the resolved X-Hasura-ArtistId, as text).
    recordedQueryWhere _mrrRecordedRequest
      `shouldBe` Just
        ( API.ApplyBinaryArrayComparisonOperator
            API.In
            (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "ArtistId") (API.ScalarType "number") Nothing)
            [J.String "1"]
            (API.ScalarType "number")
        )

  mockAgentGraphqlTest "returns no rows when getColVals finds no matching remote keys" $ \_testEnv performGraphqlRequest -> do
    let headers =
          [ ("X-Hasura-Role", testRoleName),
            ("X-Hasura-ArtistId", "999")
          ]
    let graphqlRequest =
          [graphql|
            query getAlbums {
              albums {
                AlbumId
                Title
              }
            }
          |]
    -- Empty rows: getColVals fetches no ArtistId values, so the permission
    -- rewrites to an empty IN and the main query returns nothing.
    let queryResponse = mkRowsQueryResponse []
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          albums: []
      |]
