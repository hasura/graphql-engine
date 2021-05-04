{-# LANGUAGE DuplicateRecordFields #-}

-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.Backends.MSSQL.Types.Internal where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Database.ODBC.SQLServer       as ODBC
import qualified Hasura.SQL.GeoJSON            as Geo
import qualified Hasura.SQL.WKT                as WKT
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Text.Encoding            (encodeUtf8)

import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Phantom pretend-generic types that are actually specific

type Column (b :: BackendType) = ColumnName

type ColumnType (b :: BackendType) = ScalarType

type Value = ODBC.Value

--------------------------------------------------------------------------------

data UnifiedColumn = UnifiedColumn
  { name  :: !Text
  , type' :: !ScalarType
  }

data UnifiedTableName = UnifiedTableName
  { schema :: !Text
  , name   :: !Text
  }

data UnifiedObjectRelationship = UnifiedObjectRelationship
  { using :: !UnifiedUsing
  , name  :: !Text
  }

data UnifiedArrayRelationship = UnifiedArrayRelationship
  { using :: !UnifiedUsing
  , name  :: !Text
  }

data UnifiedUsing = UnifiedUsing
  { foreign_key_constraint_on :: !UnifiedOn
  }

data UnifiedOn = UnifiedOn
  { table  :: !UnifiedTableName
  , column :: !Text
  }

-------------------------------------------------------------------------------
-- AST types

data BooleanOperators a
  = ASTContains   !a
  | ASTCrosses    !a
  | ASTEquals     !a
  | ASTIntersects !a
  | ASTOverlaps   !a
  | ASTTouches    !a
  | ASTWithin     !a

data Select = Select
  { selectTop         :: !Top
  , selectProjections :: ![Projection]
  , selectFrom        :: !From
  , selectJoins       :: ![Join]
  , selectWhere       :: !Where
  , selectFor         :: !For
  , selectOrderBy     :: !(Maybe (NonEmpty OrderBy))
  , selectOffset      :: !(Maybe Expression)
  }

data Delete = Delete
  { deleteTable :: !(Aliased TableName)
  , deleteWhere :: !Where
  }

data Reselect = Reselect
  { reselectProjections :: ![Projection]
  , reselectFor         :: !For
  , reselectWhere       :: !Where
  }

data OrderBy = OrderBy
  { orderByFieldName  :: FieldName
  , orderByOrder      :: Order
  , orderByNullsOrder :: NullsOrder
  }

data Order
  = AscOrder
  | DescOrder

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder

data For
  = JsonFor ForJson
  | NoFor

data ForJson = ForJson
  { jsonCardinality :: JsonCardinality
  , jsonRoot        :: Root
  }

data Root
  = NoRoot
  | Root Text

data JsonCardinality
  = JsonArray
  | JsonSingleton

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  | StarProjection

data Join = Join
  { joinSource    :: !JoinSource
  , joinJoinAlias :: !JoinAlias
  }

data JoinSource
  = JoinSelect Select
  | JoinReselect Reselect


data JoinAlias = JoinAlias
  { joinAliasEntity :: Text
  , joinAliasField  :: Maybe Text
  }

newtype Where =
  Where [Expression]

data Top
  = NoTop
  | Top Int

data Expression
  = ValueExpression ODBC.Value
  | AndExpression [Expression]
  | OrExpression [Expression]
  | NotExpression Expression
  | ExistsExpression Select
  | SelectExpression Select
  | IsNullExpression Expression
  | IsNotNullExpression Expression
  | ColumnExpression FieldName
  | EqualExpression Expression Expression
  | NotEqualExpression Expression Expression
  | JsonQueryExpression Expression
    -- ^ This one acts like a "cast to JSON" and makes SQL Server
    -- behave like it knows your field is JSON and not double-encode
    -- it.
  | ToStringExpression Expression
  -- expression.text(e1, e2, ..)
  | MethodExpression !Expression !Text ![Expression]
  | JsonValueExpression Expression JsonPath
    -- ^ This is for getting actual atomic values out of a JSON
    -- string.
  | OpExpression Op Expression Expression
  | ListExpression [Expression]
  | STOpExpression SpatialOp Expression Expression

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer

data Aggregate
  = CountAggregate (Countable FieldName)
  | OpAggregate !Text [Expression]
  | TextAggregate !Text

data Countable name
  = StarCountable
  | NonNullFieldCountable (NonEmpty name)
  | DistinctCountable (NonEmpty name)

data From
  = FromQualifiedTable (Aliased TableName)
  | FromOpenJson (Aliased OpenJson)

data OpenJson = OpenJson
  { openJsonExpression :: Expression
  , openJsonWith       :: NonEmpty JsonFieldSpec
  }

data JsonFieldSpec
  = IntField Text (Maybe JsonPath)
  | JsonField Text (Maybe JsonPath)
  | StringField Text (Maybe JsonPath)
  | UuidField Text (Maybe JsonPath)

data Aliased a = Aliased
  { aliasedThing :: !a
  , aliasedAlias :: !Text
  }

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  }

data TableName = TableName
  { tableName   :: !Text
  , tableSchema :: !Text
  }

type FunctionName = Text -- TODO: Improve this type when SQL function support added to MSSQL

data FieldName = FieldName
  { fieldName       :: Text
  , fieldNameEntity :: !Text
  }

data Comment = DueToPermission | RequestedSingleObject

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  }

data Op
  = LT
  | LTE
  | GT
  | GTE
  | IN
  | LIKE
  | NLIKE
  | NIN

-- | Supported operations for spatial data types
data SpatialOp
  = STEquals
  | STContains
  | STCrosses
  | STIntersects
  | STOverlaps
  | STWithin
  | STTouches

-- | Column name of some database table -- this differs to FieldName
-- that is used for referring to things within a query.
newtype ColumnName = ColumnName { columnNameText :: Text }

-- | Derived from the odbc package.
data ScalarType
  = CharType
  | NumericType
  | DecimalType
  | IntegerType
  | SmallintType
  | FloatType
  | RealType
  | DateType
  | Ss_time2Type
  | VarcharType
  | WcharType
  | WvarcharType
  | WtextType
  | TimestampType
  | TextType
  | BinaryType
  | VarbinaryType
  | BigintType
  | TinyintType
  | BitType
  | GuidType
  | GeographyType
  | GeometryType
  | UnknownType !Text

scalarTypeDBName :: ScalarType -> Text
scalarTypeDBName = \case
  CharType      -> "char"
  NumericType   -> "numeric"
  DecimalType   -> "decimal"
  IntegerType   -> "int"
  SmallintType  -> "smallint"
  FloatType     -> "float"
  RealType      -> "real"
  DateType      -> "date"
  Ss_time2Type  -> "time"
  VarcharType   -> "varchar"
  WcharType     -> "nchar"
  WvarcharType  -> "nvarchar"
  WtextType     -> "ntext"
  TextType      -> "text"
  TimestampType -> "timestamp"
  BinaryType    -> "binary"
  VarbinaryType -> "varbinary"
  BigintType    -> "bigint"
  TinyintType   -> "tinyint"
  BitType       -> "bit"
  GuidType      -> "uniqueidentifier"
  GeographyType -> "geography"
  GeometryType  -> "geometry"
  -- the input form for types that aren't explicitly supported is a string
  UnknownType t -> t

parseScalarValue :: ScalarType -> J.Value -> Either QErr Value
parseScalarValue scalarType jValue = case scalarType of
  -- bytestring
  CharType      -> ODBC.ByteStringValue . encodeUtf8 <$> parseJValue jValue
  VarcharType   -> ODBC.ByteStringValue . encodeUtf8 <$> parseJValue jValue

  -- text
  TextType      -> ODBC.TextValue <$> parseJValue jValue
  WcharType     -> ODBC.TextValue <$> parseJValue jValue
  WvarcharType  -> ODBC.TextValue <$> parseJValue jValue
  WtextType     -> ODBC.TextValue <$> parseJValue jValue

  -- integer
  IntegerType   -> ODBC.IntValue <$> parseJValue jValue
  SmallintType  -> ODBC.IntValue <$> parseJValue jValue
  BigintType    -> ODBC.IntValue <$> parseJValue jValue
  TinyintType   -> ODBC.IntValue <$> parseJValue jValue

  -- float
  NumericType   -> ODBC.FloatValue <$> parseJValue jValue
  DecimalType   -> ODBC.FloatValue <$> parseJValue jValue
  FloatType     -> ODBC.FloatValue <$> parseJValue jValue
  RealType      -> ODBC.FloatValue <$> parseJValue jValue

  -- boolean
  BitType       -> ODBC.ByteValue <$> parseJValue jValue

  -- geo
  GeographyType -> ODBC.TextValue <$> parseGeoTypes jValue
  GeometryType  -> ODBC.TextValue <$> parseGeoTypes jValue

  -- misc
  BinaryType    -> ODBC.BinaryValue . ODBC.Binary . txtToBs <$> parseJValue jValue
  VarbinaryType -> ODBC.BinaryValue . ODBC.Binary . txtToBs <$> parseJValue jValue
  Ss_time2Type  -> ODBC.TimeOfDayValue <$> parseJValue jValue
  TimestampType -> ODBC.LocalTimeValue <$> parseJValue jValue
  DateType      -> ODBC.DayValue <$> parseJValue jValue
  GuidType      -> ODBC.TextValue <$> parseJValue jValue

  -- the input format for types that aren't explicitly supported is a string
  UnknownType _ -> ODBC.TextValue <$> parseJValue jValue
  where
    parseJValue :: (J.FromJSON a) => J.Value -> Either QErr a
    parseJValue = runAesonParser J.parseJSON

    parseGeoTypes :: J.Value -> Either QErr Text
    parseGeoTypes jv =
      runAesonParser (J.parseJSON @Text) jv <> parseGeoJSONAsWKT jValue

    parseGeoJSONAsWKT :: J.Value -> Either QErr Text
    parseGeoJSONAsWKT jv =
      runAesonParser (J.parseJSON @Geo.GeometryWithCRS) jv
        >>= fmap WKT.getWKT . WKT.toWKT

isComparableType, isNumType :: ScalarType -> Bool
isComparableType = \case
  BinaryType    -> False
  VarbinaryType -> False
  BitType       -> False
  GuidType      -> False
  _             -> True
isNumType = \case
  NumericType  -> True
  DecimalType  -> True
  IntegerType  -> True
  SmallintType -> True
  FloatType    -> True
  RealType     -> True
  BigintType   -> True
  TinyintType  -> True
  _            -> False

getGQLTableName :: TableName -> Either QErr G.Name
getGQLTableName tn = do
  let textName = snakeCaseTableName tn
  onNothing (G.mkName $ snakeCaseTableName tn) $ throw400 ValidationFailed $
    "cannot include " <> textName <> " in the GraphQL schema because it is not a valid GraphQL identifier"

snakeCaseTableName :: TableName -> Text
snakeCaseTableName TableName { tableName, tableSchema } =
  if tableSchema == "dbo"
     then tableName
     else tableSchema <> "_" <> tableName

stringTypes :: [ScalarType]
stringTypes =
  [ CharType
  , VarcharType
  , TextType
  , WcharType
  , WvarcharType
  , WtextType
  ]

geoTypes :: [ScalarType]
geoTypes = [GeometryType, GeographyType]
