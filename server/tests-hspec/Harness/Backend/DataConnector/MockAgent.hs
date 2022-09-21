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
    { _crCapabilities =
        API.Capabilities
          { API._cQueries = Just API.QueryCapabilities {API._qcSupportsPrimaryKeys = True},
            API._cMutations = Nothing,
            API._cSubscriptions = Nothing,
            API._cScalarTypes = Nothing,
            API._cGraphQLTypeDefinitions = Nothing,
            API._cRelationships = Just API.RelationshipCapabilities {},
            API._cComparisons =
              Just
                API.ComparisonCapabilities
                  { API._ccSubqueryComparisonCapabilities = Just API.SubqueryComparisonCapabilities {API._ctccSupportsRelations = True}
                  },
            API._cMetrics = Just API.MetricsCapabilities {},
            API._cExplain = Just API.ExplainCapabilities {}
          },
      _crConfigSchemaResponse =
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
    { API._srTables =
        [ API.TableInfo
            { API._tiName = mkTableName "Artist",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "ArtistId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Artist primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the artist"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "ArtistId"],
              API._tiDescription = Just "Collection of artists of music",
              API._tiForeignKeys = Nothing
            },
          API.TableInfo
            { API._tiName = mkTableName "Album",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "AlbumId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Album primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Title",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The title of the album"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "ArtistId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The ID of the artist that created the album"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "AlbumId"],
              API._tiDescription = Just "Collection of music albums created by artists",
              API._tiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "Artist") (API.Constraint (mkTableName "Artist") (HashMap.singleton "ArtistId" "ArtistId"))
            },
          API.TableInfo
            { API._tiName = mkTableName "Customer",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "CustomerId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Customer primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "FirstName",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The customer's first name"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "LastName",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The customer's last name"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Company",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's company name"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Address",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address line (street number, street)"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "City",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address city"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "State",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address state"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Country",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address country"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "PostalCode",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address postal code"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Phone",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's phone number"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Fax",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's fax number"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Email",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The customer's email address"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "SupportRepId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The ID of the Employee who is this customer's support representative"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "CustomerId"],
              API._tiDescription = Just "Collection of customers who can buy tracks",
              API._tiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "CustomerSupportRep") (API.Constraint (mkTableName "Employee") (HashMap.singleton "SupportRepId" "EmployeeId"))
            },
          API.TableInfo
            { API._tiName = mkTableName "Employee",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "EmployeeId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Employee primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "LastName",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The employee's last name"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "FirstName",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The employee's first name"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Title",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's job title"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "ReportsTo",
                      API._ciType = API.NumberTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's report"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BirthDate",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's birth date"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "HireDate",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's hire date"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Address",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address line (street number, street)"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "City",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address city"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "State",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address state"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Country",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address country"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "PostalCode",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address postal code"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Phone",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's phone number"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Fax",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's fax number"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Email",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's email address"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "EmployeeId"],
              API._tiDescription = Just "Collection of employees who work for the business",
              API._tiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "EmployeeReportsTo") (API.Constraint (mkTableName "Employee") (HashMap.singleton "ReportsTo" "EmployeeId"))
            },
          API.TableInfo
            { API._tiName = mkTableName "Genre",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "GenreId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Genre primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the genre"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "GenreId"],
              API._tiDescription = Just "Genres of music",
              API._tiForeignKeys = Nothing
            },
          API.TableInfo
            { API._tiName = mkTableName "Invoice",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Invoice primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "CustomerId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "ID of the customer who bought the music"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceDate",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Date of the invoice"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingAddress",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address line (street number, street)"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingCity",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address city"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingState",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address state"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingCountry",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address country"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingPostalCode",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address postal code"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Total",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The total amount due on the invoice"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "InvoiceId"],
              API._tiDescription = Just "Collection of invoices of music purchases by a customer",
              API._tiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.singleton (API.ConstraintName "InvoiceCustomer") $
                      API.Constraint (mkTableName "Customer") (HashMap.singleton "CustomerId" "CustomerId")
            },
          API.TableInfo
            { API._tiName = mkTableName "InvoiceLine",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceLineId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Invoice Line primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "ID of the invoice the line belongs to"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "TrackId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "ID of the music track being purchased"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "UnitPrice",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Price of each individual track unit"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Quantity",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Quantity of the track purchased"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "InvoiceLineId"],
              API._tiDescription = Just "Collection of track purchasing line items of invoices",
              API._tiForeignKeys =
                Just $
                  API.ForeignKeys $
                    HashMap.fromList
                      [ (API.ConstraintName "Invoice", API.Constraint (mkTableName "Invoice") (HashMap.singleton "InvoiceId" "InvoiceId")),
                        (API.ConstraintName "Track", API.Constraint (mkTableName "Track") (HashMap.singleton "TrackId" "TrackId"))
                      ]
            },
          API.TableInfo
            { API._tiName = mkTableName "MediaType",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "MediaTypeId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "Media Type primary key identifier"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the media type format"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "MediaTypeId"],
              API._tiDescription = Just "Collection of media types that tracks can be encoded in",
              API._tiForeignKeys = Nothing
            },
          API.TableInfo
            { API._tiName = mkTableName "Track",
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "TrackId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The ID of the track"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = API.StringTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The name of the track"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "AlbumId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The ID of the album the track belongs to"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "MediaTypeId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The ID of the media type the track is encoded with"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "GenreId",
                      API._ciType = API.NumberTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The ID of the genre of the track"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Composer",
                      API._ciType = API.StringTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the composer of the track"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Milliseconds",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The length of the track in milliseconds"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Bytes",
                      API._ciType = API.NumberTy,
                      API._ciNullable = True,
                      API._ciDescription = Just "The size of the track in bytes"
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "UnitPrice",
                      API._ciType = API.NumberTy,
                      API._ciNullable = False,
                      API._ciDescription = Just "The price of the track"
                    }
                ],
              API._tiPrimaryKey = Just [API.ColumnName "TrackId"],
              API._tiDescription = Just "Collection of music tracks",
              API._tiForeignKeys =
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
