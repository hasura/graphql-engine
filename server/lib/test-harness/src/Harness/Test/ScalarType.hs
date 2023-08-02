-- | Common interface for setup/teardown for all backends - schema and data
module Harness.Test.ScalarType
  ( quotedValue,
    unquotedValue,
    WKT (..),
    ScalarType (..),
    defaultSerialType,
    ScalarValue (..),
    BackendScalarType (..),
    BackendScalarValue (..),
    BackendScalarValueType (..),
    backendScalarValue,
    defaultBackendScalarType,
    getBackendScalarType,
    defaultBackendScalarValue,
    formatBackendScalarValueType,
  )
where

import Data.Time (UTCTime)
import Hasura.Prelude

-- | Generic type to represent ScalarType for multiple backends. This
-- type can be used to encapsulate the column types for different
-- backends by providing explicit name of the datatype. This provides
-- flexibility and scalability which is difficult to achieve by just
-- extending ScalarType.
--
-- To give a concrete usecase, right now we have 'ScalarType' with
-- value 'TUTCTime'. This is treated as TIMESTAMP for Citus and
-- DATETIME for MSSQL server. There might be usecases where you want
-- your table column to treat it as TIMESTAMP for Citus and
-- <https://docs.microsoft.com/en-us/sql/t-sql/data-types/datetime2-transact-sql?redirectedfrom=MSDN&view=sql-server-ver15
-- DATETIME2> for MSSQL server. BackendScalarType makes such use case
-- very simple to achive instead of making you define a new sum type
-- and handling it.
data BackendScalarType = BackendScalarType
  { bstCitus :: Maybe Text,
    bstCockroach :: Maybe Text,
    bstPostgres :: Maybe Text,
    bstBigQuery :: Maybe Text,
    bstMssql :: Maybe Text,
    bstSqlite :: Maybe Text
  }
  deriving (Show, Eq)

-- | Default value for 'BackendScalarType' initialized with 'Nothing'
-- for all the fields.
defaultBackendScalarType :: BackendScalarType
defaultBackendScalarType =
  BackendScalarType
    { bstCitus = Nothing,
      bstCockroach = Nothing,
      bstMssql = Nothing,
      bstPostgres = Nothing,
      bstBigQuery = Nothing,
      bstSqlite = Nothing
    }

-- | Access specific backend scalar type out of 'BackendScalarType'
getBackendScalarType :: BackendScalarType -> (BackendScalarType -> Maybe Text) -> Text
getBackendScalarType bst fn =
  case fn bst of
    Just scalarType -> scalarType
    Nothing -> error $ "getBackendScalarType: BackendScalarType is Nothing, passed " <> show bst

-- | This type represents how the serialization of a value should
-- happen for a particular item. 'Quoted' text indicates that the text
-- will be enclosed with double quotes whereas 'Unqouted' text will have
-- none.
--
-- Usually, texts (or strings) should be represented as quoted and
-- numbers might not require any quotes. Although, consult the
-- particular database backend for the exact behavior. This type has
-- been introduced to allow flexibility while construting values for
-- the columns.
data BackendScalarValueType = Quoted Text | Unquoted Text deriving (Show, Eq)

quotedValue :: Text -> Maybe BackendScalarValueType
quotedValue = Just . Quoted

unquotedValue :: Text -> Maybe BackendScalarValueType
unquotedValue = Just . Unquoted

formatBackendScalarValueType :: BackendScalarValueType -> Text
formatBackendScalarValueType (Quoted text) = "'" <> text <> "'"
formatBackendScalarValueType (Unquoted text) = text

-- | Generic type to represent ScalarValue for multiple backends. This
-- type can be used to encapsulate the column values for different
-- backends by providing explicit data for individual backend. This provides
-- flexibility and scalability which is difficult to achieve by just
-- extending ScalarValue.
--
-- To give a concrete usecase, right now we have timestamp column for
-- out database. Depending on the database, the value can be
-- different. For postgres backend, we use 2017-09-21T09:39:44 to
-- represent timestamp. But we would want to use 2017-09-21T09:39:44Z
-- for Microsoft's SQL server backend. This type provides flexibility
-- to provide such options.
data BackendScalarValue = BackendScalarValue
  { bsvCitus :: Maybe BackendScalarValueType,
    bsvCockroach :: Maybe BackendScalarValueType,
    bsvPostgres :: Maybe BackendScalarValueType,
    bsvBigQuery :: Maybe BackendScalarValueType,
    bsvMssql :: Maybe BackendScalarValueType,
    bsvSqlite :: Maybe BackendScalarValueType
  }
  deriving (Show, Eq)

-- | Default value for 'BackendScalarValue' initialized with 'Nothing'
-- for all the fields.
defaultBackendScalarValue :: BackendScalarValue
defaultBackendScalarValue =
  BackendScalarValue
    { bsvCitus = Nothing,
      bsvCockroach = Nothing,
      bsvPostgres = Nothing,
      bsvBigQuery = Nothing,
      bsvMssql = Nothing,
      bsvSqlite = Nothing
    }

-- | Generic scalar type for all backends, for simplicity.
-- Ideally, we would be wiring in @'Backend@ specific scalar types here to make
-- sure all backend-specific scalar types are also covered by tests, perhaps in
-- a future iteration.
data ScalarType
  = TInt
  | TDouble
  | TStr
  | TUTCTime
  | TBool
  | TGeography
  | TCustomType BackendScalarType
  deriving (Show, Eq)

-- | Generic scalar value type for all backends, that should directly correspond
-- to 'ScalarType'
data ScalarValue
  = VInt Int
  | VDouble Double
  | VStr Text
  | VUTCTime UTCTime
  | VBool Bool
  | VGeography WKT
  | VNull
  | VCustomValue BackendScalarValue
  deriving (Show, Eq)

-- | Describe Geography values using the WKT representation
-- https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry
-- https://cloud.google.com/bigquery/docs/geospatial-data#loading_wkt_or_wkb_data
newtype WKT = WKT Text
  deriving (Eq, Show, IsString)

backendScalarValue :: BackendScalarValue -> (BackendScalarValue -> Maybe BackendScalarValueType) -> BackendScalarValueType
backendScalarValue bsv fn = case fn bsv of
  Nothing -> error $ "backendScalarValue: Retrieved value is Nothing, passed " <> show bsv
  Just scalarValue -> scalarValue

defaultSerialType :: ScalarType
defaultSerialType =
  TCustomType
    $ defaultBackendScalarType
      { bstMssql = Just "INT IDENTITY(1,1)",
        bstCitus = Just "SERIAL",
        -- cockroachdb's serial behaves differently than postgresql's serial:
        -- https://www.cockroachlabs.com/docs/v22.1/serial
        bstCockroach = Just "INT4 GENERATED BY DEFAULT AS IDENTITY",
        bstPostgres = Just "SERIAL",
        bstBigQuery = Nothing
      }
