-- | This module exports an OpenAPI specification for the GraphQL Engine
-- metadata API.
--
-- The OpenAPI specification for metadata is experimental and incomplete. Please
-- do not incorporate it into essential workflows at this time.
module Hasura.Server.MetadataOpenAPI (metadataOpenAPI) where

import Autodocodec (HasCodec, JSONCodec)
import Autodocodec.OpenAPI (declareNamedSchemaVia, declareNamedSchemaViaCodec)
import Control.Lens ((.~), (^.))
import Data.Data (Proxy)
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.OpenApi
  ( HasComponents (components),
    HasName (name),
    HasSchema (schema),
    HasSchemas (schemas),
    OpenApi,
  )
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (undeclare)
import Data.Proxy (Proxy (..))
import Hasura.Backends.BigQuery.Source (BigQueryConnSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DataConnector
import Hasura.Backends.MSSQL.Connection (MSSQLConnConfiguration)
import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.Backends.Postgres.Connection.Settings (PostgresConnConfiguration)
import Hasura.Metadata.DTO.Metadata (MetadataDTO)
import Hasura.Metadata.DTO.MetadataV1 (MetadataV1)
import Hasura.Metadata.DTO.MetadataV2 (MetadataV2)
import Hasura.Metadata.DTO.MetadataV3 (MetadataV3)
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Common
  ( BackendSourceMetadata,
    FunctionMetadata,
    SourceMetadata,
    TableMetadata,
    backendSourceMetadataCodec,
  )
import Hasura.RQL.Types.SourceCustomization (SourceCustomization)
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))

-- | An OpenApi document includes \"schemas\" that describe the data that may be
-- produced or consumed by an API. It can also include \"paths\" which describe
-- REST endpoints, and the document can include other API metadata. This example
-- only includes schemas.
--
-- Throws an error if any schema listed in 'openApiSchemas' does not have
-- a name.
--
-- The OpenAPI specification for metadata is experimental and incomplete. Please
-- do not incorporate it into essential workflows at this time.
metadataOpenAPI :: OpenApi
metadataOpenAPI =
  (mempty :: OpenApi)
    & components . schemas .~ HM.fromList (applySchemaName <$> openApiSchemas)

-- | All metadata DTOs should be listed here. Schemas in this list must be
-- named! Some autodocodec combinators apply names for you, like 'object'.
-- Otherwise you can use the 'named' combinator to apply a name.
--
-- As far as I can tell it is necessary to explicitly list all of the data
-- types that should be included in the OpenApi document with their names. It
-- would be nice to provide only a top-level type ('Metadata' in this case), and
-- have all of the types referenced by that type included automatically; but
-- I haven't seen a way to do that.
openApiSchemas :: [OpenApi.NamedSchema]
openApiSchemas =
  [ toNamedSchema (Proxy @MetadataDTO),
    toNamedSchema (Proxy @MetadataV1),
    toNamedSchema (Proxy @MetadataV2),
    toNamedSchema (Proxy @MetadataV3),
    toNamedSchemaVia backendSourceMetadataCodec (Proxy @BackendSourceMetadata),
    toNamedSchema (Proxy @SourceCustomization),
    -- SourceMetadata
    toNamedSchema (Proxy @(SourceMetadata ('Postgres 'Vanilla))),
    toNamedSchema (Proxy @(SourceMetadata ('Postgres 'Citus))),
    toNamedSchema (Proxy @(SourceMetadata ('Postgres 'Cockroach))),
    toNamedSchema (Proxy @(SourceMetadata ('MSSQL))),
    toNamedSchema (Proxy @(SourceMetadata ('BigQuery))),
    toNamedSchema (Proxy @(SourceMetadata ('MySQL))),
    toNamedSchema (Proxy @(SourceMetadata ('DataConnector))),
    -- FunctionMetadata
    toNamedSchema (Proxy @(FunctionMetadata ('Postgres 'Vanilla))),
    toNamedSchema (Proxy @(FunctionMetadata ('Postgres 'Citus))),
    toNamedSchema (Proxy @(FunctionMetadata ('Postgres 'Cockroach))),
    toNamedSchema (Proxy @(FunctionMetadata ('MSSQL))),
    toNamedSchema (Proxy @(FunctionMetadata ('BigQuery))),
    toNamedSchema (Proxy @(FunctionMetadata ('MySQL))),
    toNamedSchema (Proxy @(FunctionMetadata ('DataConnector))),
    -- TableMetadata
    toNamedSchema (Proxy @(TableMetadata ('Postgres 'Vanilla))),
    toNamedSchema (Proxy @(TableMetadata ('Postgres 'Citus))),
    toNamedSchema (Proxy @(TableMetadata ('Postgres 'Cockroach))),
    toNamedSchema (Proxy @(TableMetadata ('MSSQL))),
    toNamedSchema (Proxy @(TableMetadata ('BigQuery))),
    toNamedSchema (Proxy @(TableMetadata ('MySQL))),
    toNamedSchema (Proxy @(TableMetadata ('DataConnector))),
    -- Postgres-specific types
    toNamedSchema (Proxy @PostgresConnConfiguration),
    -- MSSQL-specific types
    toNamedSchema (Proxy @MSSQLConnConfiguration),
    -- BigQuery-specific types
    toNamedSchema (Proxy @BigQueryConnSourceConfig),
    -- MySQL-specific types
    toNamedSchema (Proxy @MySQL.ConnSourceConfig),
    -- DataConnector-specific types
    toNamedSchema (Proxy @DataConnector.ConnSourceConfig)
  ]

-- | Introspect a given 'OpenApi.NamedSchema' to get its name, and return the
-- name with the unwrapped schema. (NamedSchema wraps a pair of an
-- 'OpenApi.Schema' and an optional name.)
--
-- Throws an exception if the named schema has no name. If this happens to you
-- then use autodocodec's 'named' combinator to apply a name to your codec.
applySchemaName :: OpenApi.NamedSchema -> (Text, OpenApi.Schema)
applySchemaName givenSchema = (schemaName, givenSchema ^. schema)
  where
    schemaName = case givenSchema ^. name of
      Just n -> n
      Nothing ->
        error $
          "a codec listed in 'openApiSchemas' does not have a name; "
            <> "use the 'named' combinator from autodocodec to apply a name "
            <> "to any codec in that list that does not already have one"

toNamedSchema :: HasCodec a => Proxy a -> OpenApi.NamedSchema
toNamedSchema proxy = undeclare $ declareNamedSchemaViaCodec proxy

toNamedSchemaVia :: JSONCodec a -> Proxy a -> OpenApi.NamedSchema
toNamedSchemaVia codec proxy = undeclare $ declareNamedSchemaVia codec proxy
