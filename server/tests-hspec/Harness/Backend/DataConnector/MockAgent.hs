module Harness.Backend.DataConnector.MockAgent
  ( MockConfig (..),
    chinookMock,
    mockAgentPort,
    runMockServer,
  )
where

import Data.HashMap.Strict qualified as HashMap
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

mkTableName :: Text -> API.TableName
mkTableName = API.TableName . (:| [])

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
            API.cRelationships = Just API.RelationshipCapabilities {},
            API.cMetrics = Just API.MetricsCapabilities {},
            API.cExplain = Just API.ExplainCapabilities {}
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
            { API.dtiName = mkTableName "Artist",
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
                      API.dciNullable = True,
                      API.dciDescription = Just "The name of the artist"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "ArtistId"],
              API.dtiDescription = Just "Collection of artists of music",
              API.dtiForeignKeys = Nothing
            },
          API.TableInfo
            { API.dtiName = mkTableName "Album",
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
              API.dtiDescription = Just "Collection of music albums created by artists",
              API.dtiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "Artist") (API.Constraint (mkTableName "Artist") (HashMap.singleton "ArtistId" "ArtistId"))
            },
          API.TableInfo
            { API.dtiName = mkTableName "Customer",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "CustomerId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Customer primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "FirstName",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The customer's first name"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "LastName",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The customer's last name"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Company",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's company name"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Address",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's address line (street number, street)"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "City",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's address city"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "State",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's address state"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Country",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's address country"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "PostalCode",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's address postal code"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Phone",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's phone number"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Fax",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The customer's fax number"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Email",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The customer's email address"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "SupportRepId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The ID of the Employee who is this customer's support representative"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "CustomerId"],
              API.dtiDescription = Just "Collection of customers who can buy tracks",
              API.dtiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "CustomerSupportRep") (API.Constraint (mkTableName "Employee") (HashMap.singleton "SupportRepId" "EmployeeId"))
            },
          API.TableInfo
            { API.dtiName = mkTableName "Employee",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "EmployeeId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Employee primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "LastName",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The employee's last name"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "FirstName",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The employee's first name"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Title",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's job title"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "ReportsTo",
                      API.dciType = API.NumberTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's report"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "BirthDate",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's birth date"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "HireDate",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's hire date"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Address",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's address line (street number, street)"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "City",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's address city"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "State",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's address state"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Country",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's address country"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "PostalCode",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's address postal code"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Phone",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's phone number"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Fax",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's fax number"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Email",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The employee's email address"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "EmployeeId"],
              API.dtiDescription = Just "Collection of employees who work for the business",
              API.dtiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "EmployeeReportsTo") (API.Constraint (mkTableName "Employee") (HashMap.singleton "ReportsTo" "EmployeeId"))
            },
          API.TableInfo
            { API.dtiName = mkTableName "Genre",
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
              API.dtiDescription = Just "Genres of music",
              API.dtiForeignKeys = Nothing
            },
          API.TableInfo
            { API.dtiName = mkTableName "Invoice",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "InvoiceId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Invoice primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "CustomerId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "ID of the customer who bought the music"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "InvoiceDate",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Date of the invoice"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "BillingAddress",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The invoice's billing address line (street number, street)"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "BillingCity",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The invoice's billing address city"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "BillingState",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The invoice's billing address state"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "BillingCountry",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The invoice's billing address country"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "BillingPostalCode",
                      API.dciType = API.StringTy,
                      API.dciNullable = True,
                      API.dciDescription = Just "The invoice's billing address postal code"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Total",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The total amount due on the invoice"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "InvoiceId"],
              API.dtiDescription = Just "Collection of invoices of music purchases by a customer",
              API.dtiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "InvoiceCustomer") $
                      API.Constraint (mkTableName "Customer") (HashMap.singleton "CustomerId" "CustomerId")
            },
          API.TableInfo
            { API.dtiName = mkTableName "InvoiceLine",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "InvoiceLineId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Invoice Line primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "InvoiceId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "ID of the invoice the line belongs to"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "TrackId",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "ID of the music track being purchased"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "UnitPrice",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Price of each individual track unit"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "Quantity",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Quantity of the track purchased"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "InvoiceLineId"],
              API.dtiDescription = Just "Collection of track purchasing line items of invoices",
              API.dtiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.fromList
                      [ (API.ConstraintName "Invoice", API.Constraint (mkTableName "Invoice") (HashMap.singleton "InvoiceId" "InvoiceId")),
                        (API.ConstraintName "Track", API.Constraint (mkTableName "Track") (HashMap.singleton "TrackId" "TrackId"))
                      ]
            },
          API.TableInfo
            { API.dtiName = mkTableName "MediaType",
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
              API.dtiDescription = Just "Collection of media types that tracks can be encoded in",
              API.dtiForeignKeys = Nothing
            },
          API.TableInfo
            { API.dtiName = mkTableName "Track",
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
                      API.dciNullable = False,
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
                      API.dciNullable = False,
                      API.dciDescription = Just "The price of the track"
                    }
                ],
              API.dtiPrimaryKey = Just [API.ColumnName "TrackId"],
              API.dtiDescription = Just "Collection of music tracks",
              API.dtiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.fromList
                      [ (API.ConstraintName "Album", API.Constraint (mkTableName "Album") (HashMap.singleton "AlbumId" "AlbumId")),
                        (API.ConstraintName "Genre", API.Constraint (mkTableName "Genre") (HashMap.singleton "GenreId" "GenreId")),
                        (API.ConstraintName "MediaType", API.Constraint (mkTableName "MediaType") (HashMap.singleton "MediaTypeId" "MediaTypeId"))
                      ]
            }
        ]
    }

-- | Stock 'MockConfig' for a Chinook Agent.
chinookMock :: MockConfig
chinookMock =
  MockConfig
    { _capabilitiesResponse = capabilities,
      _schemaResponse = schema,
      _queryResponse = \_ -> API.QueryResponse (Just []) Nothing
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

-- Returns an empty explain response for now
explainHandler :: API.SourceName -> API.Config -> API.QueryRequest -> Handler API.ExplainResponse
explainHandler _sourceName _queryConfig _query = pure $ API.ExplainResponse [] ""

healthcheckHandler :: Maybe API.SourceName -> Maybe API.Config -> Handler NoContent
healthcheckHandler _sourceName _config = pure NoContent

metricsHandler :: Handler Text
metricsHandler = pure "# NOTE: Metrics would go here."

dcMockableServer :: I.IORef MockConfig -> I.IORef (Maybe API.QueryRequest) -> I.IORef (Maybe API.Config) -> Server API.Api
dcMockableServer mcfg mquery mQueryConfig =
  mockCapabilitiesHandler mcfg
    :<|> mockSchemaHandler mcfg mQueryConfig
    :<|> mockQueryHandler mcfg mquery mQueryConfig
    :<|> explainHandler
    :<|> healthcheckHandler
    :<|> metricsHandler

mockAgentPort :: Warp.Port
mockAgentPort = 65006

runMockServer :: I.IORef MockConfig -> I.IORef (Maybe API.QueryRequest) -> I.IORef (Maybe API.Config) -> IO ()
runMockServer mcfg mquery mQueryConfig = do
  let app = serve (Proxy :: Proxy API.Api) $ dcMockableServer mcfg mquery mQueryConfig
  Warp.run mockAgentPort app
