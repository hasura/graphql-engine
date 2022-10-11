{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Error Conditions in Data Connector Backends
module Test.DataConnector.MockAgent.ErrorSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector (TestCase (..))
import Harness.Backend.DataConnector qualified as DataConnector
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (..), defaultBackendTypeString, defaultSource)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------

spec :: SpecWith TestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.DataConnectorMock)
            { Fixture.mkLocalTestEnvironment = DataConnector.mkLocalTestEnvironmentMock,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [DataConnector.setupMockAction sourceMetadata DataConnector.mockBackendConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnectorMock
      backendType = defaultBackendTypeString DataConnectorMock
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
        configuration: {}
      |]

tests :: Fixture.Options -> SpecWith (TestEnvironment, DataConnector.MockAgentEnvironment)
tests opts = do
  describe "Error Protocol Tests" $ do
    it "handles returned errors correctly" $
      DataConnector.runMockedTest opts $
        let errorResponse = API.ErrorResponse API.UncaughtError "Hello World!" [yaml| { foo: "bar" } |]
            required =
              DataConnector.TestCaseRequired
                { _givenRequired = DataConnector.chinookMock {DataConnector._queryResponse = \_ -> Left errorResponse},
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
                      errors:
                        -
                          extensions:
                            code: "data-connector-error"
                            path: "$"
                            internal:
                              foo: "bar"
                          message: "UncaughtError: Hello World!"
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
                                    HashMap.fromList
                                      [ (API.FieldName "id", API.ColumnField (API.ColumnName "AlbumId")),
                                        (API.FieldName "title", API.ColumnField (API.ColumnName "Title"))
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Nothing,
                                _qOrderBy = Nothing
                              }
                        }
                    )
              }
