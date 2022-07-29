{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.MySQL.Types.Internal
  ( Aliased (..),
    ConnSourceConfig (..),
    SourceConfig (..),
    Column (..),
    JoinType (..),
    ScalarValue (..),
    Expression (..),
    Top (..),
    Op (..),
    ConnPoolSettings (..),
    FieldName (..),
    FieldOrigin (..),
    EntityAlias (..),
    Countable (..),
    Aggregate (..),
    Projection (..),
    TableName (..),
    From (..),
    Reselect (..),
    JoinAlias (..),
    Join (..),
    Where (..),
    Order (..),
    NullsOrder (..),
    ScalarType,
    OrderBy (..),
    Select (..),
    defaultConnPoolSettings,
    ConstraintName (..),
    FunctionName (..),
    parseMySQLScalarType,
    parseScalarValue,
    mkMySQLScalarTypeName,
  )
where

import Data.Aeson qualified as J
import Data.ByteString
import Data.Data
import Data.HashSet.InsOrd (InsOrdHashSet)
import Data.Hashable
import Data.Int
import Data.Pool
import Data.Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Extended (ToTxt (..))
import Data.Word (Word16)
import Database.MySQL.Base (Connection)
import Database.MySQL.Base.Types qualified as MySQLTypes (Type (..))
import Hasura.Base.Error
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Incremental.Internal.Dependency (Cacheable (..))
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

data Aliased a = Aliased
  { aliasedThing :: a,
    aliasedAlias :: Text
  }

-- | Partial of Database.MySQL.Simple.ConnectInfo
data ConnSourceConfig = ConnSourceConfig
  { -- | Works with @127.0.0.1@ but not with @localhost@: https://mariadb.com/kb/en/troubleshooting-connection-issues/#localhost-and
    _cscHost :: Text,
    _cscPort :: Word16,
    _cscUser :: Text,
    _cscPassword :: Text,
    _cscDatabase :: Text,
    _cscPoolSettings :: ConnPoolSettings
  }
  deriving (Eq, Show, NFData, Generic, Hashable)

data SourceConfig = SourceConfig
  { scConfig :: ConnSourceConfig,
    scConnectionPool :: Pool Connection
  }

newtype ConstraintName = ConstraintName {unConstraintName :: Text}
  deriving newtype (Show, Eq, ToTxt, J.FromJSON, J.ToJSON, Hashable, NFData, Cacheable)

instance ToErrorValue ConstraintName where
  toErrorValue = ErrorValue.squote . unConstraintName

newtype FunctionName = FunctionName {unFunctionName :: Text}
  deriving newtype (Show, Eq, Ord, ToTxt, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)

instance ToErrorValue FunctionName where
  toErrorValue = ErrorValue.squote . unFunctionName

newtype Column = Column {unColumn :: Text}
  deriving newtype (Show, Eq, Ord, ToTxt, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)
  deriving (Generic)

instance ToErrorValue Column where
  toErrorValue = ErrorValue.squote . unColumn

data ScalarValue
  = BigValue Int32 -- Not Int64 due to scalar-representation
  | BinaryValue ByteString
  | BitValue Bool
  | BlobValue ByteString
  | CharValue Text
  | DatetimeValue Text
  | DateValue Text
  | DecimalValue Double -- Not Decimal due to scalar-representation
  | DoubleValue Double
  | EnumValue Text
  | FloatValue Double -- Not Float due to scalar-representation
  | GeometrycollectionValue Text -- TODO
  | GeometryValue Text -- TODO
  | IntValue Int32
  | JsonValue J.Value
  | LinestringValue Text -- TODO
  | MediumValue Int32 -- (actually, 3-bytes)
  | MultilinestringValue Text -- TODO
  | MultipointValue Text -- TODO
  | MultipolygonValue Text -- TODO
  | NullValue
  | NumericValue Double -- Not Decimal due to scalar-representation -- TODO: Double check
  | PointValue Text -- TODO
  | PolygonValue Text -- TODO
  | SetValue (Set Text)
  | SmallValue Int32 -- Not Int16 due to scalar-representation
  | TextValue Text
  | TimestampValue Text
  | TimeValue Text
  | TinyValue Int32 -- Not Int8 due to scalar-representation
  | UnknownValue Text
  | VarbinaryValue ByteString
  | VarcharValue Text
  | YearValue Text
  deriving (Show, Read, Eq, Ord, Generic, J.ToJSON, J.ToJSONKey, J.FromJSON, Data, NFData, Cacheable)

instance Hashable ScalarValue where
  hashWithSalt i = hashWithSalt i . tshow

instance ToTxt ScalarValue where
  toTxt = tshow

instance J.ToJSON ByteString where
  toJSON = J.String . decodeUtf8With lenientDecode

instance J.FromJSON ByteString where
  parseJSON = J.withText "ByteString" (pure . encodeUtf8)

data Expression
  = ValueExpression ScalarValue
  | AndExpression [Expression]
  | OrExpression [Expression]
  | NotExpression Expression
  | ExistsExpression Select
  | InExpression Expression [Expression]
  | OpExpression Op Expression Expression
  | ColumnExpression FieldName
  | -- expression.text(e1, e2, ..)
    MethodExpression Expression Text [Expression]

data Top
  = NoTop
  | Top Int

data Op
  = LT
  | LTE
  | GT
  | GTE
  | IN
  | LIKE
  | NLIKE
  | NIN
  | EQ'
  | NEQ'

data ConnPoolSettings = ConnPoolSettings
  { _cscIdleTimeout :: Word,
    _cscMaxConnections :: Word
  }
  deriving (Eq, Show, NFData, Generic, Hashable)

data FieldName = FieldName
  { fName :: Text,
    fNameEntity :: Text
  }

data FieldOrigin
  = NoOrigin
  | AggregateOrigin [Aliased Aggregate]

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  }

data Countable name
  = StarCountable
  | NonNullFieldCountable (NonEmpty name)
  | DistinctCountable (NonEmpty name)

data Aggregate
  = CountAggregate (Countable FieldName)
  | OpAggregate Text [Expression]
  | TextAggregate Text

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjections (Aliased (NonEmpty (Aliased Aggregate)))
  | AggregateProjection (Aliased Aggregate)
  | StarProjection
  | EntityProjection (Aliased [(FieldName, FieldOrigin)])
  | ArrayEntityProjection EntityAlias (Aliased [FieldName])

data TableName = TableName
  { name :: Text,
    schema :: Maybe Text
  }

instance ToErrorValue TableName where
  toErrorValue TableName {name, schema} =
    ErrorValue.squote $ maybe name (<> "." <> name) schema

data From
  = FromQualifiedTable (Aliased TableName)
  | FromSelect (Aliased Select)

data Reselect = Reselect
  { reselectProjections :: [Projection],
    reselectWhere :: Where
  }

data JoinAlias = JoinAlias
  { joinAliasEntity :: Text,
    joinAliasField :: Maybe Text
  }

data Join = Join
  { -- | For display/debug purposes.
    joinRightTable :: EntityAlias,
    -- | Where to pull the data from.
    joinSelect :: Select,
    -- | Type of join to perform in-Haskell.
    joinType :: JoinType,
    -- | Wrap the output in this field name.
    joinFieldName :: Text,
    joinTop :: Top,
    joinOffset :: Maybe Int
  }

data JoinType
  = -- | A join without any 'ON x=y' construct. We're querying from a
    -- table and doing our own WHERE clauses.
    OnlessJoin
  | -- | An array join on the given fields.
    ArrayJoin [(FieldName, FieldName)]
  | -- | An array aggregate join.
    ArrayAggregateJoin [(FieldName, FieldName)]
  | -- | Simple object join on the fields.
    ObjectJoin [(FieldName, FieldName)]

newtype Where
  = Where [Expression]

data Order
  = Asc
  | Desc

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder

type ScalarType = MySQLTypes.Type

data OrderBy = OrderBy
  { orderByFieldName :: FieldName,
    orderByOrder :: Order,
    orderByNullsOrder :: NullsOrder,
    orderByType :: Maybe ScalarType
  }

data Select = Select
  { selectProjections :: InsOrdHashSet Projection,
    selectFrom :: From,
    selectJoins :: [Join],
    selectWhere :: Where,
    selectOrderBy :: Maybe (NonEmpty OrderBy),
    selectSqlOffset :: Maybe Int,
    selectSqlTop :: Top,
    selectGroupBy :: [FieldName],
    selectFinalWantedFields :: Maybe [Text]
  }

mkMySQLScalarTypeName :: MonadError QErr m => ScalarType -> m G.Name
mkMySQLScalarTypeName = \case
  scalarType ->
    G.mkName (scalarTypeDBName scalarType)
      `onNothing` throw400
        ValidationFailed
        ( "cannot use SQL type " <> scalarTypeDBName scalarType <> " in the GraphQL schema because its name is not a "
            <> "valid GraphQL identifier"
        )

scalarTypeDBName :: ScalarType -> Text
scalarTypeDBName = error "scalarTypeDBName: not implemented"

defaultConnPoolSettings :: ConnPoolSettings
defaultConnPoolSettings =
  ConnPoolSettings
    { _cscIdleTimeout = 5,
      _cscMaxConnections = 50
    }

-- | ref: https://dev.mysql.com/doc/c-api/8.0/en/c-api-data-structures.html
--
-- DB has CHAR, BINARY, VARCHAR and VARBINARY
-- C API only has STRING and VARSTRING
-- Database.MySQL.Base.Types.Type has String, VarString and VarChar for some reason
parseMySQLScalarType :: Text -> ScalarType
parseMySQLScalarType scalarType =
  case (T.toUpper scalarType) of
    "BIGINT" -> MySQLTypes.LongLong
    "BINARY" -> MySQLTypes.String
    "BIT" -> MySQLTypes.Bit
    "BLOB" -> MySQLTypes.Blob -- TinyBlob, MediumBlob, LongBlob
    "CHAR" -> MySQLTypes.String
    "DATE" -> MySQLTypes.Date -- Or NewDate. REVIEW: When to use NewDate :: Database.MySQL.Base.Types.Type then?
    "DATETIME" -> MySQLTypes.DateTime
    "DECIMAL" -> MySQLTypes.Decimal -- Or NewDecimal
    "DOUBLE" -> MySQLTypes.Double
    "ENUM" -> MySQLTypes.Enum
    "FLOAT" -> MySQLTypes.Float
    "GEOMETRYCOLLECTION" -> MySQLTypes.Geometry
    "GEOMETRY" -> MySQLTypes.Geometry -- For all Geometry types. TODO: Check how to distinguish between these types when it becomes necessary
    "INT" -> MySQLTypes.Long
    "JSON" -> MySQLTypes.Json
    "LINESTRING" -> MySQLTypes.Geometry -- For now Geometry could be considered as Text
    "MEDIUMINT" -> MySQLTypes.Int24
    "MULTILINESTRING" -> MySQLTypes.Geometry
    "MULTIPOINT" -> MySQLTypes.Geometry
    "MULTIPOLYGON" -> MySQLTypes.Geometry
    "NULL" -> MySQLTypes.Null -- Not a column type, but we retain it as part of this definition to enumerate all possible types
    "NUMERIC" -> MySQLTypes.Decimal -- Or NewDecimal
    "POINT" -> MySQLTypes.Geometry
    "POLYGON" -> MySQLTypes.Geometry
    "SET" -> MySQLTypes.Set
    "SMALLINT" -> MySQLTypes.Short
    "TEXT" -> MySQLTypes.Blob
    "TIME" -> MySQLTypes.Time
    "TIMESTAMP" -> MySQLTypes.Timestamp
    "TINYINT" -> MySQLTypes.Tiny
    "VARBINARY" -> MySQLTypes.VarString
    "VARCHAR" -> MySQLTypes.VarChar
    "YEAR" -> MySQLTypes.Year
    "TINYTEXT" -> MySQLTypes.String
    "VARCHAR(45)" -> MySQLTypes.VarChar
    "VARCHAR(450)" -> MySQLTypes.VarChar
    "INT UNSIGNED" -> MySQLTypes.Long
    "BIT(1)" -> MySQLTypes.Bit
    -- _ -> MySQLTypes.Null

    txt | "INT" `T.isPrefixOf` txt -> MySQLTypes.Long
    txt -> error $ "parseMySQLScalartype: " <> show txt

parseScalarValue :: ScalarType -> J.Value -> Either QErr (ScalarValue)
parseScalarValue = error "parseScalarValue is yet to be implemented."
