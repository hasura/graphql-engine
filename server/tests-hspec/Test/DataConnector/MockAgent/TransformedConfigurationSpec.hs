{-# LANGUAGE QuasiQuotes #-}

-- | Configuration Transformation Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.TransformedConfigurationSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector (TestCase (..))
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    ( NE.fromList
        [ Context.Context
            { name = Context.Backend Context.DataConnector,
              mkLocalTestEnvironment = DataConnector.mkLocalTestEnvironmentMock,
              setup = DataConnector.setupMock sourceMetadata DataConnector.mockBackendConfig,
              teardown = DataConnector.teardownMock,
              customOptions = Nothing
            }
        ]
    )
    tests

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [Album]
            configuration:
              custom_root_fields:
                select: albums
                select_by_pk: albums_by_pk
              column_config:
                AlbumId:
                  custom_name: id
                Title:
                  custom_name: title
                ArtistId:
                  custom_name: artist_id
            object_relationships:
              - name: artist
                using:
                  manual_configuration:
                    remote_table: [Artist]
                    column_mapping:
                      ArtistId: ArtistId
          - table: [Artist]
            configuration:
              custom_root_fields:
                select: artists
                select_by_pk: artists_by_pk
              column_config:
                ArtistId:
                  custom_name: id
                Name:
                  custom_name: name
            array_relationships:
              - name: albums
                using:
                  manual_configuration:
                    remote_table: [Album]
                    column_mapping:
                      ArtistId: ArtistId
        configuration:
          value: {}
          template: |
            {
              "DEBUG": {
                "session": {{ $session?.foo ?? "foo session default" }},
                "env":     {{ $env?.bar     ?? "bar env default"     }},
                "config":  {{ $config?.baz  ?? "baz config default"  }}
              }
            }
        |]

--------------------------------------------------------------------------------

tests :: Context.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = do
  describe "Basic Tests" $ do
    it "works with configuration transformation Kriti template" $
      DataConnector.runMockedTest opts $
        let required =
              DataConnector.TestCaseRequired
                { _givenRequired =
                    let albums =
                          [ [ ("id", API.mkColumnFieldValue $ Aeson.Number 1),
                              ("title", API.mkColumnFieldValue $ Aeson.String "For Those About To Rock We Salute You")
                            ]
                          ]
                     in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> rowsResponse albums},
                  _whenRequestRequired =
                    [graphql|
                      query getAlbum {
                        albums(limit: 1) {
                          id
                          title
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        albums:
                          - id: 1
                            title: For Those About To Rock We Salute You
                    |]
                }
         in (DataConnector.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("Album" :| []),
                          _qrTableRelationships = [],
                          _qrQuery =
                            API.Query
                              { _qFields =
                                  Just $
                                    KM.fromList
                                      [ ("id", API.ColumnField (API.ColumnName "AlbumId")),
                                        ("title", API.ColumnField (API.ColumnName "Title"))
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Just (API.And []),
                                _qOrderBy = Nothing
                              }
                        }
                    ),
                _whenConfig =
                  -- TODO: Create a QQ for this purpose.
                  let conf =
                        Aeson.fromJSON
                          [yaml|
                            DEBUG:
                              config: "baz config default"
                              env: "bar env default"
                              session: "foo session default"
                          |]
                   in case conf of
                        Aeson.Success r -> Just r
                        _ -> error "Should parse."
              }

rowsResponse :: [[(Aeson.Key, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ KM.fromList <$> rows) Nothing
