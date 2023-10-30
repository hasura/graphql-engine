{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Mock Agent Warp server backend
module Harness.Backend.DataConnector.Mock.Server
  ( AgentRequest (..),
    MockConfig (..),
    chinookMock,
    MockRequestConfig (..),
    defaultMockRequestConfig,
    mockAgentPort,
    runMockServer,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.IORef qualified as I
import Data.OpenApi qualified as OpenApi
import Data.Proxy
import Data.SOP.BasicFunctors qualified as SOP
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Network.Wai.Handler.Warp qualified as Warp
import Servant

--------------------------------------------------------------------------------

data AgentRequest
  = Schema
  | Query API.QueryRequest
  | Mutation API.MutationRequest
  deriving stock (Eq, Show)

data MockConfig = MockConfig
  { _capabilitiesResponse :: API.CapabilitiesResponse,
    _schemaResponse :: API.SchemaResponse,
    _requestConfig :: MockRequestConfig
  }

data MockRequestConfig = MockRequestConfig
  { _queryResponse :: API.QueryRequest -> Either API.ErrorResponse API.QueryResponse,
    _mutationResponse :: API.MutationRequest -> Either API.ErrorResponse API.MutationResponse
  }

mkTableName :: Text -> API.TableName
mkTableName = API.TableName . (:| [])

mkScalar :: Text -> API.ColumnType
mkScalar = API.ColumnTypeScalar . API.ScalarType

-- | Stock Capabilities for a Chinook Agent
capabilities :: API.CapabilitiesResponse
capabilities =
  API.CapabilitiesResponse
    { _crCapabilities =
        API.Capabilities
          { API._cDataSchema = API.defaultDataSchemaCapabilities,
            API._cPostSchema = Just API.defaultPostSchemaCapabilities,
            API._cQueries =
              Just
                API.QueryCapabilities
                  { _qcForeach = Just API.ForeachCapabilities,
                    _qcRedaction = Just API.RedactionCapabilities
                  },
            API._cMutations =
              Just
                $ API.MutationCapabilities
                  { API._mcInsertCapabilities = Just API.InsertCapabilities {API._icSupportsNestedInserts = False},
                    API._mcUpdateCapabilities = Just API.UpdateCapabilities,
                    API._mcDeleteCapabilities = Just API.DeleteCapabilities,
                    API._mcAtomicitySupportLevel = Just API.HeterogeneousOperationsAtomicity,
                    API._mcReturningCapabilities = Just API.ReturningCapabilities
                  },
            API._cSubscriptions = Nothing,
            API._cScalarTypes = scalarTypesCapabilities,
            API._cRelationships = Just API.RelationshipCapabilities {},
            API._cInterpolatedQueries = Just API.InterpolatedQueryCapabilities {},
            API._cComparisons =
              Just
                API.ComparisonCapabilities
                  { API._ccSubqueryComparisonCapabilities = Just API.SubqueryComparisonCapabilities {API._ctccSupportsRelations = True}
                  },
            API._cMetrics = Just API.MetricsCapabilities {},
            API._cExplain = Just API.ExplainCapabilities {},
            API._cRaw = Just API.RawCapabilities {},
            API._cDatasets = Just API.DatasetCapabilities {},
            API._cUserDefinedFunctions = Just API.UserDefinedFunctionCapabilities {},
            API._cLicensing = Nothing
          },
      _crConfigSchemaResponse =
        API.ConfigSchemaResponse
          { _csrConfigSchema =
              mempty
                { OpenApi._schemaType = Just OpenApi.OpenApiObject,
                  OpenApi._schemaNullable = Just False,
                  OpenApi._schemaProperties =
                    InsOrdHashMap.singleton
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
          },
      _crDisplayName = Nothing,
      _crReleaseName = Nothing
    }
  where
    scalarTypesCapabilities =
      API.ScalarTypesCapabilities
        $ HashMap.fromList
          [ mkScalarTypeCapability "number" minMaxFunctions numericUpdateOperators $ Just API.GraphQLFloat,
            mkScalarTypeCapability "string" minMaxFunctions mempty $ Just API.GraphQLString,
            mkScalarTypeCapability "MyInt" mempty numericUpdateOperators $ Just API.GraphQLInt,
            mkScalarTypeCapability "MyFloat" mempty numericUpdateOperators $ Just API.GraphQLFloat,
            mkScalarTypeCapability "MyString" mempty mempty $ Just API.GraphQLString,
            mkScalarTypeCapability "MyBoolean" mempty mempty $ Just API.GraphQLBoolean,
            mkScalarTypeCapability "MyID" mempty mempty $ Just API.GraphQLID,
            mkScalarTypeCapability "MyAnything" mempty mempty Nothing
          ]
    mkScalarTypeCapability ::
      Text ->
      (API.ScalarType -> API.AggregateFunctions) ->
      (API.ScalarType -> API.UpdateColumnOperators) ->
      Maybe API.GraphQLType ->
      (API.ScalarType, API.ScalarTypeCapabilities)
    mkScalarTypeCapability name aggregateFunctions updateColumnOperators gqlType =
      (scalarType, API.ScalarTypeCapabilities mempty (aggregateFunctions scalarType) (updateColumnOperators scalarType) gqlType)
      where
        scalarType = API.ScalarType name

    minMaxFunctions :: API.ScalarType -> API.AggregateFunctions
    minMaxFunctions resultType =
      API.AggregateFunctions
        $ HashMap.fromList
        $ (,resultType)
        <$> [[G.name|min|], [G.name|max|]]

    numericUpdateOperators :: API.ScalarType -> API.UpdateColumnOperators
    numericUpdateOperators scalarType =
      API.UpdateColumnOperators
        $ HashMap.fromList
        $ [(API.UpdateColumnOperatorName [G.name|inc|], API.UpdateColumnOperatorDefinition scalarType)]

-- | Stock Schema for a Chinook Agent
schema :: API.SchemaResponse
schema =
  API.SchemaResponse
    { API._srFunctions = [],
      API._srTables =
        [ API.TableInfo
            { API._tiName = mkTableName "Artist",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "ArtistId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Artist primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the artist",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "ArtistId" :| [],
              API._tiDescription = Just "Collection of artists of music",
              API._tiForeignKeys = API.ForeignKeys mempty,
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "Album",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "AlbumId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Album primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Title",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The title of the album",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "ArtistId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "The ID of the artist that created the album",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "AlbumId" :| [],
              API._tiDescription = Just "Collection of music albums created by artists",
              API._tiForeignKeys =
                API.ForeignKeys
                  $ HashMap.singleton
                    (API.ConstraintName "Artist")
                    ( API.Constraint
                        (mkTableName "Artist")
                        (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "ArtistId") (API.mkColumnSelector $ API.ColumnName "ArtistId"))
                    ),
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "Customer",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "CustomerId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Customer primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "FirstName",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The customer's first name",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "LastName",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The customer's last name",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Company",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's company name",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Address",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address line (street number, street)",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "City",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address city",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "State",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address state",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Country",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address country",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "PostalCode",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's address postal code",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Phone",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's phone number",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Fax",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The customer's fax number",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Email",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The customer's email address",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "SupportRepId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = True,
                      API._ciDescription = Just "The ID of the Employee who is this customer's support representative",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "CustomerId" :| [],
              API._tiDescription = Just "Collection of customers who can buy tracks",
              API._tiForeignKeys =
                API.ForeignKeys
                  $ HashMap.singleton
                    (API.ConstraintName "CustomerSupportRep")
                    ( API.Constraint
                        (mkTableName "Employee")
                        (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "SupportRepId") (API.mkColumnSelector $ API.ColumnName "EmployeeId"))
                    ),
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "Employee",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "EmployeeId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Employee primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "LastName",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The employee's last name",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "FirstName",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The employee's first name",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Title",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's job title",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "ReportsTo",
                      API._ciType = mkScalar "number",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's report",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BirthDate",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's birth date",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "HireDate",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's hire date",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Address",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address line (street number, street)",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "City",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address city",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "State",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address state",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Country",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address country",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "PostalCode",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's address postal code",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Phone",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's phone number",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Fax",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's fax number",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Email",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The employee's email address",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "EmployeeId" :| [],
              API._tiDescription = Just "Collection of employees who work for the business",
              API._tiForeignKeys =
                API.ForeignKeys
                  $ HashMap.singleton
                    (API.ConstraintName "EmployeeReportsTo")
                    ( API.Constraint
                        (mkTableName "Employee")
                        (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "ReportsTo") (API.mkColumnSelector $ API.ColumnName "EmployeeId"))
                    ),
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "Genre",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "GenreId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Genre primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the genre",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "GenreId" :| [],
              API._tiDescription = Just "Genres of music",
              API._tiForeignKeys = API.ForeignKeys mempty,
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "Invoice",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Invoice primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "CustomerId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "ID of the customer who bought the music",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceDate",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "Date of the invoice",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingAddress",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address line (street number, street)",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingCity",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address city",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingState",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address state",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingCountry",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address country",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "BillingPostalCode",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The invoice's billing address postal code",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Total",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "The total amount due on the invoice",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "InvoiceId" :| [],
              API._tiDescription = Just "Collection of invoices of music purchases by a customer",
              API._tiForeignKeys =
                API.ForeignKeys
                  $ HashMap.singleton (API.ConstraintName "InvoiceCustomer")
                  $ API.Constraint
                    (mkTableName "Customer")
                    (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "CustomerId") (API.mkColumnSelector $ API.ColumnName "CustomerId")),
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "InvoiceLine",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceLineId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Invoice Line primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "InvoiceId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "ID of the invoice the line belongs to",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "TrackId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "ID of the music track being purchased",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "UnitPrice",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Price of each individual track unit",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Quantity",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Quantity of the track purchased",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "InvoiceLineId" :| [],
              API._tiDescription = Just "Collection of track purchasing line items of invoices",
              API._tiForeignKeys =
                API.ForeignKeys
                  $ HashMap.fromList
                    [ ( API.ConstraintName "Invoice",
                        API.Constraint
                          (mkTableName "Invoice")
                          (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "InvoiceId") (API.mkColumnSelector $ API.ColumnName "InvoiceId"))
                      ),
                      ( API.ConstraintName "Track",
                        API.Constraint
                          (mkTableName "Track")
                          (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "TrackId") (API.mkColumnSelector $ API.ColumnName "TrackId"))
                      )
                    ],
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "MediaType",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "MediaTypeId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "Media Type primary key identifier",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the media type format",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "MediaTypeId" :| [],
              API._tiDescription = Just "Collection of media types that tracks can be encoded in",
              API._tiForeignKeys = API.ForeignKeys mempty,
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "Track",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo
                    { API._ciName = API.ColumnName "TrackId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "The ID of the track",
                      API._ciInsertable = True,
                      API._ciUpdatable = False,
                      API._ciValueGenerated = Just API.AutoIncrement
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Name",
                      API._ciType = mkScalar "string",
                      API._ciNullable = False,
                      API._ciDescription = Just "The name of the track",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "AlbumId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = True,
                      API._ciDescription = Just "The ID of the album the track belongs to",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "MediaTypeId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "The ID of the media type the track is encoded with",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "GenreId",
                      API._ciType = mkScalar "number",
                      API._ciNullable = True,
                      API._ciDescription = Just "The ID of the genre of the track",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Composer",
                      API._ciType = mkScalar "string",
                      API._ciNullable = True,
                      API._ciDescription = Just "The name of the composer of the track",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Milliseconds",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "The length of the track in milliseconds",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "Bytes",
                      API._ciType = mkScalar "number",
                      API._ciNullable = True,
                      API._ciDescription = Just "The size of the track in bytes",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    },
                  API.ColumnInfo
                    { API._ciName = API.ColumnName "UnitPrice",
                      API._ciType = mkScalar "number",
                      API._ciNullable = False,
                      API._ciDescription = Just "The price of the track",
                      API._ciInsertable = True,
                      API._ciUpdatable = True,
                      API._ciValueGenerated = Nothing
                    }
                ],
              API._tiPrimaryKey = Just $ API.ColumnName "TrackId" :| [],
              API._tiDescription = Just "Collection of music tracks",
              API._tiForeignKeys =
                API.ForeignKeys
                  $ HashMap.fromList
                    [ ( API.ConstraintName "Album",
                        API.Constraint
                          (mkTableName "Album")
                          (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "AlbumId") (API.mkColumnSelector $ API.ColumnName "AlbumId"))
                      ),
                      ( API.ConstraintName "Genre",
                        API.Constraint
                          (mkTableName "Genre")
                          (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "GenreId") (API.mkColumnSelector $ API.ColumnName "GenreId"))
                      ),
                      ( API.ConstraintName "MediaType",
                        API.Constraint
                          (mkTableName "MediaType")
                          (API.ColumnPathMapping $ HashMap.singleton (API.mkColumnSelector $ API.ColumnName "MediaTypeId") (API.mkColumnSelector $ API.ColumnName "MediaTypeId"))
                      )
                    ],
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            },
          API.TableInfo
            { API._tiName = mkTableName "MyCustomScalarsTable",
              API._tiType = API.Table,
              API._tiColumns =
                [ API.ColumnInfo (API.ColumnName "MyIntColumn") (mkScalar "MyInt") False Nothing True True Nothing,
                  API.ColumnInfo (API.ColumnName "MyFloatColumn") (mkScalar "MyFloat") False Nothing True True Nothing,
                  API.ColumnInfo (API.ColumnName "MyStringColumn") (mkScalar "MyString") False Nothing True True Nothing,
                  API.ColumnInfo (API.ColumnName "MyBooleanColumn") (mkScalar "MyBoolean") False Nothing True True Nothing,
                  API.ColumnInfo (API.ColumnName "MyIDColumn") (mkScalar "MyID") False Nothing True True Nothing,
                  API.ColumnInfo (API.ColumnName "MyAnythingColumn") (mkScalar "MyAnything") False Nothing True True Nothing
                ],
              API._tiPrimaryKey = Nothing,
              API._tiDescription = Nothing,
              API._tiForeignKeys = API.ForeignKeys mempty,
              API._tiInsertable = True,
              API._tiUpdatable = True,
              API._tiDeletable = True
            }
        ],
      _srObjectTypes = Nothing
    }

chinookMock :: MockConfig
chinookMock =
  MockConfig
    { _capabilitiesResponse = capabilities,
      _schemaResponse = schema,
      _requestConfig = defaultMockRequestConfig
    }

-- | Stock 'MockConfig' for a Chinook Agent.
defaultMockRequestConfig :: MockRequestConfig
defaultMockRequestConfig =
  MockRequestConfig
    { _queryResponse = \_request -> Right $ API.QueryResponse (Just []) Nothing,
      _mutationResponse = \_request -> Right $ API.MutationResponse []
    }

--------------------------------------------------------------------------------

mockCapabilitiesHandler :: I.IORef MockConfig -> Handler (Union API.CapabilitiesResponses)
mockCapabilitiesHandler mcfg = liftIO $ do
  cfg <- I.readIORef mcfg
  pure $ inject $ SOP.I $ _capabilitiesResponse cfg

mockSchemaGetHandler :: I.IORef MockConfig -> I.IORef (Maybe AgentRequest) -> I.IORef (Maybe API.Config) -> API.SourceName -> API.Config -> Handler (Union API.SchemaResponses)
mockSchemaGetHandler mcfg mRecordedRequest mRecordedRequestConfig _sourceName requestConfig = liftIO $ do
  cfg <- I.readIORef mcfg
  I.writeIORef mRecordedRequest (Just Schema)
  I.writeIORef mRecordedRequestConfig (Just requestConfig)
  pure $ inject $ SOP.I $ _schemaResponse cfg

mockSchemaPostHandler :: I.IORef MockConfig -> I.IORef (Maybe AgentRequest) -> I.IORef (Maybe API.Config) -> API.SourceName -> API.Config -> API.SchemaRequest -> Handler (Union API.SchemaResponses)
mockSchemaPostHandler mcfg mRecordedRequest mRecordedRequestConfig _sourceName requestConfig _schemaRequest = liftIO $ do
  cfg <- I.readIORef mcfg
  I.writeIORef mRecordedRequest (Just Schema)
  I.writeIORef mRecordedRequestConfig (Just requestConfig)
  pure $ inject $ SOP.I $ _schemaResponse cfg

mockQueryHandler :: I.IORef MockConfig -> I.IORef (Maybe AgentRequest) -> I.IORef (Maybe API.Config) -> API.SourceName -> API.Config -> API.QueryRequest -> Handler (Union API.QueryResponses)
mockQueryHandler mcfg mRecordedRequest mRecordedRequestConfig _sourceName requestConfig query = liftIO $ do
  handler <- fmap (_queryResponse . _requestConfig) $ I.readIORef mcfg
  I.writeIORef mRecordedRequest (Just $ Query query)
  I.writeIORef mRecordedRequestConfig (Just requestConfig)
  case handler query of
    Left err -> pure $ inject $ SOP.I err
    Right response -> pure $ inject $ SOP.I response

mockMutationHandler :: I.IORef MockConfig -> I.IORef (Maybe AgentRequest) -> I.IORef (Maybe API.Config) -> API.SourceName -> API.Config -> API.MutationRequest -> Handler (Union API.MutationResponses)
mockMutationHandler mcfg mRecordedRequest mRecordedRequestConfig _sourceName requestConfig mutation = liftIO $ do
  handler <- fmap (_mutationResponse . _requestConfig) $ I.readIORef mcfg
  I.writeIORef mRecordedRequest (Just $ Mutation mutation)
  I.writeIORef mRecordedRequestConfig (Just requestConfig)
  case handler mutation of
    Left err -> pure $ inject $ SOP.I err
    Right response -> pure $ inject $ SOP.I response

-- Returns an empty explain response for now
explainHandler :: API.SourceName -> API.Config -> API.QueryRequest -> Handler API.ExplainResponse
explainHandler _sourceName _queryConfig _query = pure $ API.ExplainResponse [] ""

healthcheckHandler :: Maybe API.SourceName -> Maybe API.Config -> Handler NoContent
healthcheckHandler _sourceName _config = pure NoContent

metricsHandler :: Handler Text
metricsHandler = pure "# NOTE: Metrics would go here."

rawHandler :: API.SourceName -> API.Config -> API.RawRequest -> Handler API.RawResponse
rawHandler _ _ _ = pure $ API.RawResponse [] -- NOTE: Raw query response would go here.

datasetHandler :: (API.DatasetTemplateName -> Handler API.DatasetGetTemplateResponse) :<|> ((API.DatasetCloneName -> API.DatasetCreateCloneRequest -> Handler API.DatasetCreateCloneResponse) :<|> (API.DatasetCloneName -> Handler API.DatasetDeleteCloneResponse))
datasetHandler = datasetGetHandler :<|> datasetPostHandler :<|> datasetDeleteHandler
  where
    datasetGetHandler _ = pure $ API.datasetGetTemplateSuccess
    datasetPostHandler _ _ = pure $ API.DatasetCreateCloneResponse API.emptyConfig
    datasetDeleteHandler _ = pure $ API.datasetDeleteCloneSuccess

dcMockableServer :: I.IORef MockConfig -> I.IORef (Maybe AgentRequest) -> I.IORef (Maybe API.Config) -> Server API.Api
dcMockableServer mcfg mRecordedRequest mRecordedRequestConfig =
  mockCapabilitiesHandler mcfg
    :<|> mockSchemaGetHandler mcfg mRecordedRequest mRecordedRequestConfig
    :<|> mockSchemaPostHandler mcfg mRecordedRequest mRecordedRequestConfig
    :<|> mockQueryHandler mcfg mRecordedRequest mRecordedRequestConfig
    :<|> explainHandler
    :<|> mockMutationHandler mcfg mRecordedRequest mRecordedRequestConfig
    :<|> healthcheckHandler
    :<|> metricsHandler
    :<|> rawHandler
    :<|> datasetHandler

mockAgentPort :: Warp.Port
mockAgentPort = 65006

runMockServer :: I.IORef MockConfig -> I.IORef (Maybe AgentRequest) -> I.IORef (Maybe API.Config) -> IO ()
runMockServer mcfg mRecordedRequest mRecordedRequestConfig = do
  let app = serve (Proxy :: Proxy API.Api) $ dcMockableServer mcfg mRecordedRequest mRecordedRequestConfig
  Warp.run mockAgentPort app
