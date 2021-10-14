{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Types.Internal
  ( Aliased (..),
    ConnSourceConfig (..),
    SourceConfig (..),
    Column (..),
    ScalarValue (..),
    Expression (..),
    Top (..),
    Op (..),
    ConnPoolSettings (..),
    FieldName (..),
    EntityAlias (..),
    Countable (..),
    Aggregate (..),
    Projection (..),
    TableName (..),
    OpenJson (..),
    JsonPath (..),
    JsonFieldSpec (..),
    From (..),
    JoinSource (..),
    Reselect (..),
    JoinAlias (..),
    Join (..),
    Where (..),
    ForJson (..),
    JsonCardinality (..),
    Root (..),
    For (..),
    Order (..),
    NullsOrder (..),
    ScalarType,
    OrderBy (..),
    Select (..),
    defaultConnPoolSettings,
    FunctionName,
    ConstraintName (..),
    parseMySQLScalarType,
    parseScalarValue,
    mkMySQLScalarTypeName,
    JoinType (..),
    -- These stand as API stubs to be implemented in a follow-up PR. The
    -- changes to FromIr are substantial, and we don't want them in the
    -- PR presenting this commit.

    selectFinalWantedFields,
    joinType,
    joinTop,
    joinOffset,
    joinSelect,
    joinFieldName,
    joinRightTable,
  )
where

import Data.Aeson qualified as J
import Data.ByteString
import Data.Data
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
import Hasura.Incremental.Internal.Dependency (Cacheable (..))
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

type FunctionName = Text

data Aliased a = Aliased
  { aliasedThing :: !a,
    aliasedAlias :: !Text
  }

-- | Partial of Database.MySQL.Simple.ConnectInfo
data ConnSourceConfig = ConnSourceConfig
  { -- | Works with @127.0.0.1@ but not with @localhost@: https://mariadb.com/kb/en/troubleshooting-connection-issues/#localhost-and
    _cscHost :: !Text,
    _cscPort :: !Word16,
    _cscUser :: !Text,
    _cscPassword :: !Text,
    _cscDatabase :: !Text,
    _cscPoolSettings :: !ConnPoolSettings
  }
  deriving (Eq, Show, NFData, Generic, Hashable)

data SourceConfig = SourceConfig
  { scConfig :: !ConnSourceConfig,
    scConnectionPool :: !(Pool Connection)
  }

newtype ConstraintName = ConstraintName
  {unConstraintName :: Text}
  deriving newtype (Show, Eq, ToTxt, J.FromJSON, J.ToJSON, Hashable, NFData, Cacheable)

newtype Column = Column
  {unColumn :: Text}
  deriving newtype (Show, Eq, Ord, ToTxt, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)
  deriving (Generic)

data ScalarValue
  = BigValue !Int32 -- Not (!Int64) due to scalar-representation
  | BinaryValue !ByteString
  | BitValue !Bool
  | BlobValue !ByteString
  | CharValue !Text
  | DatetimeValue !Text
  | DateValue !Text
  | DecimalValue !Double -- Not (!Decimal) due to scalar-representation
  | DoubleValue !Double
  | EnumValue !Text
  | FloatValue !Double -- Not (!Float) due to scalar-representation
  | GeometrycollectionValue !Text -- TODO
  | GeometryValue !Text -- TODO
  | IntValue !Int32
  | JsonValue !J.Value
  | LinestringValue !Text -- TODO
  | MediumValue !Int32 -- (actually, 3-bytes)
  | MultilinestringValue !Text -- TODO
  | MultipointValue !Text -- TODO
  | MultipolygonValue !Text -- TODO
  | NullValue
  | NumericValue !Double -- Not (!Decimal) due to scalar-representation -- TODO: Double check
  | PointValue !Text -- TODO
  | PolygonValue !Text -- TODO
  | SetValue !(Set Text)
  | SmallValue !Int32 -- Not (!Int16) due to scalar-representation
  | TextValue !Text
  | TimestampValue !Text
  | TimeValue !Text
  | TinyValue !Int32 -- Not (!Int8) due to scalar-representation
  | UnknownValue !Text
  | VarbinaryValue !ByteString
  | VarcharValue !Text
  | YearValue !Text
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
  | OpExpression Op Expression Expression
  | ColumnExpression FieldName
  | -- expression.text(e1, e2, ..)
    MethodExpression !Expression !Text ![Expression]

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
  { _cscIdleTimeout :: !Word,
    _cscMaxConnections :: !Word
  }
  deriving (Eq, Show, NFData, Generic, Hashable)

data FieldName = FieldName
  { fName :: !Text,
    fNameEntity :: !Text
  }

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  }

data Countable name
  = StarCountable
  | NonNullFieldCountable (NonEmpty name)
  | DistinctCountable (NonEmpty name)

data Aggregate
  = CountAggregate (Countable FieldName)
  | OpAggregate !Text [Expression]
  | TextAggregate !Text

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  | StarProjection

data TableName = TableName
  { name :: !Text,
    schema :: !Text
  }

data OpenJson = OpenJson
  { openJsonExpression :: Expression,
    openJsonWith :: NonEmpty JsonFieldSpec
  }

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer

data JsonFieldSpec
  = IntField Text (Maybe JsonPath)
  | JsonField Text (Maybe JsonPath)
  | StringField Text (Maybe JsonPath)
  | UuidField Text (Maybe JsonPath)

data From
  = FromQualifiedTable (Aliased TableName)
  | FromOpenJson (Aliased OpenJson)

data JoinSource
  = JoinSelect Select
  | JoinReselect Reselect

data Reselect = Reselect
  { reselectProjections :: ![Projection],
    reselectFor :: !For,
    reselectWhere :: !Where
  }

data JoinAlias = JoinAlias
  { joinAliasEntity :: Text,
    joinAliasField :: Maybe Text
  }

data Join = Join
  { joinSource :: !JoinSource,
    joinJoinAlias :: !JoinAlias
  }

-- TODO: TO be implemented in next PR by adding a field to Join.
joinType :: Join -> JoinType
joinType = undefined

-- TODO: TO be implemented in next PR by adding a field to Join.
joinSelect :: Join -> Select
joinSelect = undefined

-- TODO: TO be implemented in next PR by adding a field to Join.
joinOffset :: Join -> Maybe Int
joinOffset = undefined

-- TODO: TO be implemented in next PR by adding a field to Join.
joinFieldName :: Join -> Text
joinFieldName = undefined

-- TODO: TO be implemented in next PR by adding a field to Join.
joinTop :: Join -> Top
joinTop = undefined

-- TODO: TO be implemented in next PR by adding a field to Join.
joinRightTable :: Join -> EntityAlias
joinRightTable = undefined

-- | This type is for FromIr to communicate with DataLoader.Execute
-- the specific type of join to be performed and related information
-- to achieve it.
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

data ForJson = ForJson
  { jsonCardinality :: JsonCardinality,
    jsonRoot :: Root
  }

data JsonCardinality
  = JsonArray
  | JsonSingleton

data Root
  = NoRoot
  | Root Text

data For
  = JsonFor ForJson
  | NoFor

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
  { selectProjections :: ![Projection],
    selectFrom :: !(Maybe From),
    selectJoins :: ![Join],
    selectWhere :: !Where,
    selectFor :: !For,
    selectOrderBy :: !(Maybe (NonEmpty OrderBy)),
    selectOffset :: !(Maybe Expression),
    selectTop :: !Top
  }

-- TODO: To be implemented in the next PR. Needed by the planner/executor.
selectFinalWantedFields :: Select -> (Maybe [Text])
selectFinalWantedFields = undefined

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
    txt -> error $ "parseMySQLScalartype: " <> show txt

parseScalarValue :: ScalarType -> J.Value -> Either QErr (ScalarValue)
parseScalarValue = error "parseScalarValue is yet to be implemented."
