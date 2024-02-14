{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.OrderBySpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
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
import Language.GraphQL.Draft.Syntax.QQ qualified as G
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
            array_relationships:
              - name: Albums
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      ArtistId: ArtistId
        configuration: {}
      |]

--------------------------------------------------------------------------------

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Order By Tests" $ do
  mockAgentGraphqlTest "can order by column" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              Album(limit: 3, order_by: {AlbumId: asc}) {
                AlbumId
                Title
              }
            }
          |]
    let queryResponse =
          rowsResponse
            [ [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ J.Number 1),
                (API.FieldName "Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
              ],
              [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ J.Number 2),
                (API.FieldName "Title", API.mkColumnFieldValue $ J.String "Balls to the Wall")
              ],
              [ (API.FieldName "AlbumId", API.mkColumnFieldValue $ J.Number 3),
                (API.FieldName "Title", API.mkColumnFieldValue $ J.String "Restless and Wild")
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Album:
            - AlbumId: 1
              Title: For Those About To Rock We Salute You
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
                    & API.qLimit
                  ?~ 3
                    & API.qOrderBy
                  ?~ API.OrderBy mempty (API.OrderByElement [] (API.OrderByColumn (API.mkColumnSelector $ API.ColumnName "AlbumId") Nothing) API.Ascending :| [])
              )
        )

  mockAgentGraphqlTest "can order by aggregates" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getArtists {
              Artist(order_by: {Albums_aggregate: {count: asc, max: {AlbumId: asc}}}, limit: 2) {
                Name
              }
            }
          |]
    let queryResponse =
          rowsResponse
            [ [(API.FieldName "Name", API.mkColumnFieldValue $ J.String "Milton Nascimento & Bebeto")],
              [(API.FieldName "Name", API.mkColumnFieldValue $ J.String "Azymuth")]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Artist:
            - Name: Milton Nascimento & Bebeto
            - Name: Azymuth
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Artist")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap [("Name", API.ColumnField (API.ColumnName "Name") (API.ScalarType "string") Nothing)]
                    & API.qLimit
                  ?~ 2
                    & API.qOrderBy
                  ?~ API.OrderBy
                    ( HashMap.fromList
                        [ ( API.RelationshipName "Albums",
                            API.OrderByRelation Nothing mempty
                          )
                        ]
                    )
                    ( NE.fromList
                        [ API.OrderByElement [API.RelationshipName "Albums"] API.OrderByStarCountAggregate API.Ascending,
                          API.OrderByElement
                            [API.RelationshipName "Albums"]
                            ( API.OrderBySingleColumnAggregate
                                $ API.SingleColumnAggregate
                                  (API.SingleColumnAggregateFunction [G.name|max|])
                                  (API.ColumnName "AlbumId")
                                  Nothing
                                  (API.ScalarType "number")
                            )
                            API.Ascending
                        ]
                    )
              )
            & API.qrRelationships
            .~ Set.fromList
              [ API.RTable
                  API.TableRelationships
                    { _trelSourceTable = mkTableName "Artist",
                      _trelRelationships =
                        HashMap.fromList
                          [ ( API.RelationshipName "Albums",
                              API.Relationship
                                { _rTarget = mkTableTarget "Album",
                                  _rRelationshipType = API.ArrayRelationship,
                                  _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
                                }
                            )
                          ]
                    }
              ]
        )

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
