module Harness.Backend.DataConnector.MockAgent
  ( MockConfig (..),
    chinookMock,
    mockAgentPort,
    runMockServer,
  )
where

import Data.HashMap.Strict.InsOrd qualified as HMap
import Data.IORef qualified as I
import Data.OpenApi qualified as OpenApi
import Data.Proxy
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Network.Wai.Handler.Warp qualified as Warp
import Servant

--------------------------------------------------------------------------------

data MockConfig = MockConfig
  { _capabilitiesResponse :: API.CapabilitiesResponse,
    _schemaResponse :: API.SchemaResponse,
    _queryResponse :: API.QueryRequest -> API.QueryResponse
  }

-- | Stock Capabilities for a Chinook Agent
capabilities :: API.CapabilitiesResponse
capabilities =
  API.CapabilitiesResponse
    { crCapabilities =
        API.Capabilities
          { API.cQueries = Just API.QueryCapabilities {API.qcSupportsPrimaryKeys = True},
            API.cMutations = Nothing,
            API.cSubscriptions = Nothing,
            API.cFiltering = Nothing,
            API.cRelationships = Just API.RelationshipCapabilities {}
          },
      crConfigSchemaResponse =
        API.ConfigSchemaResponse
          { _csrConfigSchema =
              mempty
                { OpenApi._schemaType = Just OpenApi.OpenApiObject,
                  OpenApi._schemaNullable = Just False,
                  OpenApi._schemaProperties =
                    HMap.singleton
                      "DEBUG"
                      ( OpenApi.Inline
                          mempty
                            { OpenApi._schemaType = Just OpenApi.OpenApiObject,
                              OpenApi._schemaNullable = Just True,
                              OpenApi._schemaAdditionalProperties = Just (OpenApi.AdditionalPropertiesAllowed True)
                            }
                      )
                },
            _csrOtherSchemas = mempty
          }
    }

-- | Stock Schema for a Chinook Agent
schema :: API.SchemaResponse
schema =
  API.SchemaResponse
    { API.srTables =
        [ API.TableInfo
            { API.dtiName = API.TableName "Artist",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "ArtistId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Artist primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Name",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The name of the artist"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "ArtistId"],
              API.dtiDescription = Just "Collection of artists of music"
            },
          API.TableInfo
            { API.dtiName = API.TableName "Album",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "AlbumId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Album primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Title",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The title of the album"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "ArtistId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The ID of the artist that created the album"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "AlbumId"],
              API.dtiDescription = Just "Collection of music albums created by artists"
            },
          API.TableInfo
            { API.dtiName = API.TableName "Genre",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "GenreId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Genre primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Name",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The name of the genre"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "GenreId"],
              API.dtiDescription = Just "Genres of music"
            },
          API.TableInfo
            { API.dtiName = API.TableName "MediaType",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "MediaTypeId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Media Type primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Name",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The name of the media type format"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "MediaTypeId"],
              API.dtiDescription = Just "Collection of media types that tracks can be encoded in"
            },
          API.TableInfo
            { API.dtiName = API.TableName "Track",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "TrackId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The ID of the track"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Name",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The name of the track"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "AlbumId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The ID of the album the track belongs to"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "MediaTypeId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The ID of the media type the track is encoded with"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "GenreId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The ID of the genre of the track"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Composer",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The name of the composer of the track"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Milliseconds",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The length of the track in milliseconds"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Bytes",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The size of the track in bytes"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "UnitPrice",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The price of the track"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "TrackId"],
              API.dtiDescription = Just "Collection of music tracks"
            }
        ]
    }

-- | Stock 'MockConfig' for a Chinook Agent.
chinookMock :: MockConfig
chinookMock =
  MockConfig
    { _capabilitiesResponse = capabilities,
      _schemaResponse = schema,
      _queryResponse = \_ -> API.QueryResponse []
    }

--------------------------------------------------------------------------------

mockCapabilitiesHandler :: I.IORef MockConfig -> Handler API.CapabilitiesResponse
mockCapabilitiesHandler mcfg = liftIO $ do
  cfg <- I.readIORef mcfg
  pure $ _capabilitiesResponse cfg

mockSchemaHandler :: I.IORef MockConfig -> I.IORef (Maybe API.Config) -> API.SourceName -> API.Config -> Handler API.SchemaResponse
mockSchemaHandler mcfg mQueryConfig _sourceName queryConfig = liftIO $ do
  cfg <- I.readIORef mcfg
  I.writeIORef mQueryConfig (Just queryConfig)
  pure $ _schemaResponse cfg

mockQueryHandler :: I.IORef MockConfig -> I.IORef (Maybe API.QueryRequest) -> I.IORef (Maybe API.Config) -> API.SourceName -> API.Config -> API.QueryRequest -> Handler API.QueryResponse
mockQueryHandler mcfg mquery mQueryCfg _sourceName queryConfig query = liftIO $ do
  handler <- fmap _queryResponse $ I.readIORef mcfg
  I.writeIORef mquery (Just query)
  I.writeIORef mQueryCfg (Just queryConfig)
  pure $ handler query

healthcheckHandler :: Maybe API.SourceName -> Maybe API.Config -> Handler NoContent
healthcheckHandler _sourceName _config = pure NoContent

dcMockableServer :: I.IORef MockConfig -> I.IORef (Maybe API.QueryRequest) -> I.IORef (Maybe API.Config) -> Server API.Api
dcMockableServer mcfg mquery mQueryConfig =
  mockCapabilitiesHandler mcfg
    :<|> mockSchemaHandler mcfg mQueryConfig
    :<|> mockQueryHandler mcfg mquery mQueryConfig
    :<|> healthcheckHandler

mockAgentPort :: Warp.Port
mockAgentPort = 65006

runMockServer :: I.IORef MockConfig -> I.IORef (Maybe API.QueryRequest) -> I.IORef (Maybe API.Config) -> IO ()
runMockServer mcfg mquery mQueryConfig = do
  let app = serve (Proxy :: Proxy API.Api) $ dcMockableServer mcfg mquery mQueryConfig
  Warp.run mockAgentPort app
