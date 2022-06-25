{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.QueryMockAgentSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended (ValueWrapper (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml, yamlObjects)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.API qualified as API
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.DataConnector,
          mkLocalTestEnvironment = DataConnector.mkLocalTestEnvironmentMock,
          setup = DataConnector.setupMock sourceMetadata DataConnector.mockBackendConfig,
          teardown = DataConnector.teardownMock,
          customOptions = Nothing
        }
    ]
    tests

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
   in [yaml|
name : *source
kind: *backendType
tables:
  - table: Album
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
            remote_table: Artist
            column_mapping:
              ArtistId: ArtistId
  - table: Artist
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
            remote_table: Album
            column_mapping:
              ArtistId: ArtistId
configuration: {}
|]

--------------------------------------------------------------------------------

tests :: Context.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = describe "Mock Agent Queries" $ do
  describe "Basic Tests" $ do
    it "works with simple object query" $
      DataConnector.runMockedTest opts $
        DataConnector.TestCase
          { _given =
              let albums =
                    [yamlObjects|
            - id: 1
              title: For Those About To Rock We Salute You
            |]
               in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> API.QueryResponse albums},
            _whenRequest =
              [graphql|
            query getAlbum {
              albums(limit: 1) {
                id
                title
              }
            }
          |],
            _whenQuery =
              Just
                ( API.QueryRequest
                    { _qrTable = API.TableName {unTableName = "Album"},
                      _qrTableRelationships = [],
                      _qrQuery =
                        API.Query
                          { _qFields =
                              KM.fromList
                                [ ("id", API.ColumnField (ValueWrapper {getValue = API.ColumnName {unColumnName = "AlbumId"}})),
                                  ("title", API.ColumnField (ValueWrapper {getValue = API.ColumnName {unColumnName = "Title"}}))
                                ],
                            _qLimit = Just 1,
                            _qOffset = Nothing,
                            _qWhere = Just (API.And (ValueWrapper {getValue = []})),
                            _qOrderBy = Nothing
                          }
                    }
                ),
            _then =
              [yaml|
              data:
                albums:
                  - id: 1
                    title: For Those About To Rock We Salute You
            |]
          }

    it "works with order_by id" $
      DataConnector.runMockedTest opts $
        DataConnector.TestCase
          { _given =
              let albums =
                    [yamlObjects|
              - id: 1
                title: For Those About To Rock We Salute You
              - id: 2
                title: Balls to the Wall
              - id: 3
                title: Restless and Wild
            |]
               in DataConnector.chinookMock {DataConnector._queryResponse = \_ -> API.QueryResponse albums},
            _whenRequest =
              [graphql|
              query getAlbum {
                albums(limit: 3, order_by: {id: asc}) {
                  id
                  title
                }
              }
              |],
            _whenQuery =
              Just
                ( API.QueryRequest
                    { _qrTable = API.TableName {unTableName = "Album"},
                      _qrTableRelationships = [],
                      _qrQuery =
                        API.Query
                          { _qFields =
                              KM.fromList
                                [ ("id", API.ColumnField (ValueWrapper {getValue = API.ColumnName {unColumnName = "AlbumId"}})),
                                  ("title", API.ColumnField (ValueWrapper {getValue = API.ColumnName {unColumnName = "Title"}}))
                                ],
                            _qLimit = Just 3,
                            _qOffset = Nothing,
                            _qWhere = Just (API.And (ValueWrapper {getValue = []})),
                            _qOrderBy = Just (API.OrderBy {column = API.ColumnName {unColumnName = "AlbumId"}, ordering = API.Ascending} NE.:| [])
                          }
                    }
                ),
            _then =
              [yaml|
              data:
                albums:
                  - id: 1
                    title: For Those About To Rock We Salute You
                  - id: 2
                    title: Balls to the Wall
                  - id: 3
                    title: Restless and Wild
            |]
          }
