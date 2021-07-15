{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hasura.Backends.MySQL.Types where


import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import           Data.ByteString
import           Data.Data
import           Data.Hashable
import           Data.Int
import           Data.Pool
import           Data.Set
import           Data.Text.Encoding                     (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error               (lenientDecode)
import           Data.Text.Extended                     (ToTxt (..))
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Word                              (Word16)
import           Database.MySQL.Base
import qualified Database.MySQL.Base.Types              as MySQLTypes (Type (..))
import           Hasura.Base.Error                      (QErr)
import           Hasura.Incremental.Internal.Dependency (Cacheable (..))
import           Hasura.Prelude
import           Hasura.SQL.Types                       (ToSQL (..))
import           System.IO.Unsafe                       (unsafePerformIO)
import qualified Text.Builder                           as TB


data ConnPoolSettings
  = ConnPoolSettings
    { _cscIdleTimeout    :: !Word
    , _cscMaxConnections :: !Word
    } deriving (Eq, Show, NFData, Generic, Hashable, Cacheable)
instance Arbitrary ConnPoolSettings where
  arbitrary = genericArbitrary
instance Cacheable Word where
  unchanged _ = (==)
defaultConnPoolSettings :: ConnPoolSettings
defaultConnPoolSettings =
  ConnPoolSettings
    { _cscIdleTimeout = 5
    , _cscMaxConnections = 50
    }
instance J.FromJSON ConnPoolSettings where
  parseJSON = J.withObject "MySQL pool settings" $ \o ->
    ConnPoolSettings
      <$> o J..:? "max_connections" J..!= _cscMaxConnections defaultConnPoolSettings
      <*> o J..:? "idle_timeout"    J..!= _cscIdleTimeout    defaultConnPoolSettings
$(J.deriveToJSON hasuraJSON ''ConnPoolSettings)


-- | Partial of Database.MySQL.Simple.ConnectInfo
data ConnSourceConfig
  = ConnSourceConfig
    { _cscHost         :: !Text -- ^ Works with @127.0.0.1@ but not with @localhost@ for some reason
    , _cscPort         :: !Word16
    , _cscUser         :: !Text
    , _cscPassword     :: !Text
    , _cscDatabase     :: !Text
    , _cscPoolSettings :: !ConnPoolSettings
    } deriving (Eq, Show, NFData, Generic, Hashable)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = False} ''ConnSourceConfig)
instance Arbitrary ConnSourceConfig where
  arbitrary = genericArbitrary
instance Cacheable Word16 where
  unchanged _ = (==)
deriving instance Cacheable ConnSourceConfig


data SourceConfig
  = SourceConfig
    { scConfig         :: !ConnSourceConfig
    , scConnectionPool :: !(Pool Connection)
    } deriving (Eq, Generic, J.ToJSON)
instance Arbitrary SourceConfig where
  arbitrary = genericArbitrary
instance J.ToJSON (Pool Connection) where
  toJSON = const (J.String "_REDACTED_")
instance Eq (Pool Connection) where
  _ == _ = True
instance Arbitrary (Pool Connection) where
  arbitrary = pure . unsafePerformIO $
    createPool (connect defaultConnectInfo) close 1 (60 {-seconds-} * 60 {-minutes-}) 1
instance Cacheable SourceConfig where
  unchanged _ = (==)


data TableName
  = TableName
    { name   :: !Text
    , schema :: !Text
    } deriving (Show, Eq, Ord, Generic, J.ToJSONKey, J.ToJSON, J.FromJSON, Data, Hashable, Cacheable, NFData)
instance Arbitrary TableName where
  arbitrary = genericArbitrary
instance ToTxt TableName where
  toTxt TableName{..} = name


data FieldName
  = FieldName
    { fName       :: !Text
    , fNameEntity :: !Text
    }


newtype ConstraintName
  = ConstraintName
    { unConstraintName :: Text }
    deriving newtype (Show, Eq, ToTxt, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)


newtype Column
  = Column
    { unColumn :: Text }
    deriving newtype (Show, Eq, Ord, ToTxt, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)
    deriving (Generic)
instance Arbitrary Column where
  arbitrary = genericArbitrary


type ScalarType = MySQLTypes.Type
deriving instance Ord MySQLTypes.Type
deriving instance Generic MySQLTypes.Type
deriving instance J.ToJSON MySQLTypes.Type
deriving instance J.ToJSONKey MySQLTypes.Type
deriving instance J.FromJSON MySQLTypes.Type
deriving instance J.FromJSONKey MySQLTypes.Type
deriving instance Data MySQLTypes.Type
deriving instance NFData MySQLTypes.Type
deriving instance Hashable MySQLTypes.Type
deriving instance Cacheable MySQLTypes.Type
instance ToTxt MySQLTypes.Type where
  toTxt = tshow


-- | ref: https://dev.mysql.com/doc/c-api/8.0/en/c-api-data-structures.html
--
-- DB has CHAR, BINARY, VARCHAR and VARBINARY
-- C API only has STRING and VARSTRING
-- Database.MySQL.Base.Types.Type has String, VarString and VarChar for some reason
--
parseMySQLScalarType :: Text -> ScalarType
parseMySQLScalarType = \case
  "BIGINT"             -> MySQLTypes.LongLong
  "BINARY"             -> MySQLTypes.String
  "BIT"                -> MySQLTypes.Bit
  "BLOB"               -> MySQLTypes.Blob -- TinyBlob, MediumBlob, LongBlob
  "CHAR"               -> MySQLTypes.String
  "DATE"               -> MySQLTypes.Date
  -- ^ 'NewDate' is obsolete as per: https://dev.mysql.com/doc/dev/connector-net/6.10/html/T_MySql_Data_MySqlClient_MySqlDbType.htm
  "DATETIME"           -> MySQLTypes.DateTime
  "DECIMAL"            -> MySQLTypes.Decimal
  -- ^ Sticking with 'Decimal' here, until we unearth what 'NewDecimal' is.
  "DOUBLE"             -> MySQLTypes.Double
  "ENUM"               -> MySQLTypes.Enum
  "FLOAT"              -> MySQLTypes.Float
  "GEOMETRYCOLLECTION" -> MySQLTypes.Geometry
  "GEOMETRY"           -> MySQLTypes.Geometry -- For all Geometry types. TODO: Check how to distinguish between these types when it becomes necessary
  "INT"                -> MySQLTypes.Long
  "JSON"               -> MySQLTypes.Json
  "LINESTRING"         -> MySQLTypes.Geometry -- For now Geometry could be considered as Text
  "MEDIUMINT"          -> MySQLTypes.Int24
  "MULTILINESTRING"    -> MySQLTypes.Geometry
  "MULTIPOINT"         -> MySQLTypes.Geometry
  "MULTIPOLYGON"       -> MySQLTypes.Geometry
  "NULL"               -> MySQLTypes.Null -- Not a column type, but we retain it as part of this definition to enumerate all possible types
  "NUMERIC"            -> MySQLTypes.Decimal -- Or NewDecimal
  "POINT"              -> MySQLTypes.Geometry
  "POLYGON"            -> MySQLTypes.Geometry
  "SET"                -> MySQLTypes.Set
  "SMALLINT"           -> MySQLTypes.Short
  "TEXT"               -> MySQLTypes.Blob
  "TIME"               -> MySQLTypes.Time
  "TIMESTAMP"          -> MySQLTypes.Timestamp
  "TINYINT"            -> MySQLTypes.Tiny
  "VARBINARY"          -> MySQLTypes.VarString
  "VARCHAR"            -> MySQLTypes.VarChar
  "YEAR"               -> MySQLTypes.Year
  _                    -> MySQLTypes.Null


data ScalarValue
  = BigValue !Int32 -- Not (!Int64) due to scalar-representation
  | BinaryValue !ByteString
  | BitValue !Bool
  | BlobValue !ByteString
  | CharValue !Text
  | DatetimeValue !UTCTime
  | DateValue !Day
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
  | TimestampValue !UTCTime
  | TimeValue !TimeOfDay
  | TinyValue !Int32 -- Not (!Int8) due to scalar-representation
  | UnknownValue !Text
  | VarbinaryValue !ByteString
  | VarcharValue !Text
  | YearValue !Word16 -- (4-digit year)
  deriving (Show, Read, Eq, Ord, Generic, J.ToJSON, J.ToJSONKey, J.FromJSON, Data, NFData, Cacheable)
instance Hashable ScalarValue where
  hashWithSalt i = hashWithSalt i . tshow
instance ToTxt ScalarValue where
  toTxt = tshow
instance J.ToJSON ByteString where
  toJSON = J.String . decodeUtf8With lenientDecode
instance J.FromJSON ByteString where
  parseJSON = J.withText "ByteString" (pure . encodeUtf8)


parseScalarValue :: ScalarType -> Text -> Either QErr (ScalarValue)
parseScalarValue = error "parseScalarValue is yet to be implemented."


data Order
  = Asc
  | Desc
  deriving (Show, Eq, Ord, Generic, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)


data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder
  deriving (Show, Eq, Ord, Generic, J.FromJSON, J.ToJSON, Hashable, Cacheable, NFData)


data OrderBy = OrderBy
  { orderByFieldName  :: FieldName
  , orderByOrder      :: Order
  , orderByNullsOrder :: NullsOrder
  , orderByType       :: Maybe ScalarType
  }


data Expression
  = ValueExpression ScalarValue
  deriving (Show, Eq, Generic, Data, Hashable, Cacheable, NFData)

instance J.ToJSON Expression where
  toJSON (ValueExpression scalarValue) = J.toJSON scalarValue
instance J.FromJSON Expression where
  parseJSON value = ValueExpression <$> J.parseJSON value

instance ToSQL Expression where
  toSQL = TB.text . tshow
