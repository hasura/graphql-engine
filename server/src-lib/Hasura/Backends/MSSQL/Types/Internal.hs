{-# LANGUAGE DuplicateRecordFields #-}

-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.
--
-- In this module we define various MS SQL Server specific data types used for T-SQL generation.
--
-- These types are also used as underlying types in the @Backend 'MSSQL@ instance
-- which is defined in "Hasura.Backends.MSSQL.Instances.Types".
--
-- We convert RQL IR ASTs to types defined here in the "Hasura.Backends.MSSQL.FromIr" module,
-- and we implement pretty-printing for these types in the "Hasura.Backends.MSSQL.ToQuery" module.
--
-- NOTE: Various type class instances (including simple once such as Eq and Show) are implemented
-- in the "Hasura.Backends.MSSQL.Types.Instances" module.
module Hasura.Backends.MSSQL.Types.Internal
  ( Aggregate (..),
    Aliased (..),
    BooleanOperators (..),
    Column,
    ColumnName (..),
    ColumnType,
    Comment (..),
    Countable (..),
    Delete (..),
    DeleteOutput,
    EntityAlias (..),
    Expression (..),
    FieldName (..),
    For (..),
    ForJson (..),
    From (..),
    FunctionName,
    MergeUsing (..),
    MergeOn (..),
    MergeWhenMatched (..),
    MergeWhenNotMatched (..),
    Merge (..),
    Insert (..),
    InsertOutput,
    Join (..),
    JoinAlias (..),
    JoinSource (..),
    JsonCardinality (..),
    JsonFieldSpec (..),
    JsonPath (..),
    NullsOrder (..),
    Op (..),
    OpenJson (..),
    Order (..),
    OrderBy (..),
    OutputColumn (..),
    Inserted (..),
    Deleted (..),
    Output (..),
    Projection (..),
    Reselect (..),
    Root (..),
    ScalarType (..),
    SchemaName (..),
    Select (..),
    SetIdentityInsert (..),
    TempTableName (..),
    SomeTableName (..),
    TempTable (..),
    SetValue (..),
    SelectIntoTempTable (..),
    SITTConstraints (..),
    InsertValuesIntoTempTable (..),
    SpatialOp (..),
    TableName (..),
    Top (..),
    UnifiedArrayRelationship (..),
    UnifiedColumn (..),
    UnifiedObjectRelationship (..),
    UnifiedOn (..),
    UnifiedTableName (..),
    UnifiedUsing (..),
    Value,
    Values (..),
    Where (..),
    With (..),
    emptySelect,
    geoTypes,
    getGQLTableName,
    isComparableType,
    isNumType,
    mkMSSQLScalarTypeName,
    parseScalarValue,
    scalarTypeDBName,
    snakeCaseTableName,
    stringTypes,
    tempTableNameInserted,
    tempTableNameValues,
    tempTableNameDeleted,
    tempTableNameUpdated,
  )
where

import Data.Aeson qualified as J
import Data.Text.Encoding (encodeUtf8)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.SQL.Backend
import Hasura.SQL.GeoJSON qualified as Geo
import Hasura.SQL.WKT qualified as WKT
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Phantom pretend-generic types that are actually specific

type Column (b :: BackendType) = ColumnName

type ColumnType (b :: BackendType) = ScalarType

type Value = ODBC.Value

--------------------------------------------------------------------------------

data UnifiedColumn = UnifiedColumn
  { name :: !Text,
    type' :: !ScalarType
  }

data UnifiedTableName = UnifiedTableName
  { schema :: !Text,
    name :: !Text
  }

data UnifiedObjectRelationship = UnifiedObjectRelationship
  { using :: !UnifiedUsing,
    name :: !Text
  }

data UnifiedArrayRelationship = UnifiedArrayRelationship
  { using :: !UnifiedUsing,
    name :: !Text
  }

data UnifiedUsing = UnifiedUsing
  { foreign_key_constraint_on :: !UnifiedOn
  }

data UnifiedOn = UnifiedOn
  { table :: !UnifiedTableName,
    column :: !Text
  }

-------------------------------------------------------------------------------
-- AST types

data BooleanOperators a
  = ASTContains !a
  | ASTCrosses !a
  | ASTEquals !a
  | ASTIntersects !a
  | ASTOverlaps !a
  | ASTTouches !a
  | ASTWithin !a

data Select = Select
  { selectWith :: !(Maybe With),
    selectTop :: !Top,
    selectProjections :: ![Projection],
    selectFrom :: !(Maybe From),
    selectJoins :: ![Join],
    selectWhere :: !Where,
    selectFor :: !For,
    selectOrderBy :: !(Maybe (NonEmpty OrderBy)),
    selectOffset :: !(Maybe Expression)
  }

emptySelect :: Select
emptySelect =
  Select
    { selectWith = Nothing,
      selectFrom = Nothing,
      selectTop = NoTop,
      selectProjections = [],
      selectJoins = [],
      selectWhere = Where [],
      selectOrderBy = Nothing,
      selectFor = NoFor,
      selectOffset = Nothing
    }

newtype OutputColumn = OutputColumn {unOutputColumn :: ColumnName}

data Inserted = Inserted

data Deleted = Deleted

data Output t = Output
  { outputType :: !t,
    outputColumns :: ![OutputColumn]
  }

type InsertOutput = Output Inserted

newtype Values = Values [Expression]

data Insert = Insert
  { insertTable :: TableName,
    insertColumns :: [ColumnName],
    insertOutput :: InsertOutput,
    insertTempTable :: TempTable,
    insertValues :: [Values]
  }

data SetValue
  = SetON
  | SetOFF

data SetIdentityInsert = SetIdentityInsert
  { setTable :: SomeTableName,
    setValue :: !SetValue
  }

type DeleteOutput = Output Deleted

data Delete = Delete
  { deleteTable :: !(Aliased TableName),
    deleteOutput :: !DeleteOutput,
    deleteTempTable :: !TempTable,
    deleteWhere :: !Where
  }

-- | MERGE statement.
-- Used for upserts and is responsible for actually inserting or updating the data in the table.
data Merge = Merge
  { mergeTargetTable :: TableName,
    mergeUsing :: MergeUsing,
    mergeOn :: MergeOn,
    mergeWhenMatched :: MergeWhenMatched,
    mergeWhenNotMatched :: MergeWhenNotMatched,
    mergeInsertOutput :: InsertOutput,
    mergeOutputTempTable :: TempTable
  }

-- | The @USING@ section of a @MERGE@ statement.
--   Specifies the temp table schema where the input values are.
data MergeUsing = MergeUsing
  { mergeUsingTempTable :: TempTableName,
    mergeUsingColumns :: [ColumnName]
  }

-- | The @ON@ section of a @MERGE@ statement.
--   Which columns to match on?
data MergeOn = MergeOn
  { mergeOnColumns :: [ColumnName]
  }

-- | The @WHEN MATCHED@ section of a @MERGE@ statement.
--   Which columns to update when @match_columns@ match (including presets),
--   and on which condition to actually update the values.
data MergeWhenMatched = MergeWhenMatched
  { mwmUpdateColumns :: [ColumnName],
    mwmCondition :: Expression,
    mwmUpdatePreset :: HashMap ColumnName Expression
  }

-- | The @WHEN MATCHED@ section of a @MERGE@ statement.
--   Which columns to insert?
newtype MergeWhenNotMatched = MergeWhenNotMatched
  { mergeWhenNotMatchedInsertColumns :: [ColumnName]
  }

-- | SELECT INTO temporary table statement without values.
--   Used to create a temporary table with the same schema as an existing table.
data SelectIntoTempTable = SelectIntoTempTable
  { sittTempTableName :: TempTableName,
    sittColumns :: [UnifiedColumn],
    sittFromTableName :: TableName,
    sittConstraints :: SITTConstraints
  }

-- | When creating a temporary table from an existing table schema,
--   what should we do with the constraints (such as @IDENTITY@?)
data SITTConstraints
  = KeepConstraints
  | RemoveConstraints

-- | Simple insert into a temporary table.
data InsertValuesIntoTempTable = InsertValuesIntoTempTable
  { ivittTempTableName :: TempTableName,
    ivittColumns :: [ColumnName],
    ivittValues :: [Values]
  }

-- | A temporary table name is prepended by a hash-sign
newtype TempTableName = TempTableName Text

tempTableNameInserted :: TempTableName
tempTableNameInserted = TempTableName "inserted"

tempTableNameValues :: TempTableName
tempTableNameValues = TempTableName "values"

tempTableNameDeleted :: TempTableName
tempTableNameDeleted = TempTableName "deleted"

tempTableNameUpdated :: TempTableName
tempTableNameUpdated = TempTableName "updated"

-- | A name of a regular table or temporary table
data SomeTableName
  = RegularTableName TableName
  | TemporaryTableName TempTableName

data TempTable = TempTable
  { ttName :: !TempTableName,
    ttColumns :: ![ColumnName]
  }

data Reselect = Reselect
  { reselectProjections :: ![Projection],
    reselectFor :: !For,
    reselectWhere :: !Where
  }

data OrderBy = OrderBy
  { orderByFieldName :: FieldName,
    orderByOrder :: Order,
    orderByNullsOrder :: NullsOrder,
    orderByType :: Maybe ScalarType
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
  { jsonCardinality :: JsonCardinality,
    jsonRoot :: Root
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
  { joinSource :: !JoinSource,
    joinJoinAlias :: !JoinAlias
  }

data JoinSource
  = JoinSelect Select
  | JoinReselect Reselect

data JoinAlias = JoinAlias
  { joinAliasEntity :: Text,
    joinAliasField :: Maybe Text
  }

newtype Where
  = Where [Expression]

newtype With
  = With (NonEmpty (Aliased Select))

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
  | -- | This one acts like a "cast to JSON" and makes SQL Server
    -- behave like it knows your field is JSON and not double-encode
    -- it.
    JsonQueryExpression Expression
  | ToStringExpression Expression
  | -- expression.text(e1, e2, ..)
    MethodExpression !Expression !Text ![Expression]
  | -- | This is for getting actual atomic values out of a JSON
    -- string.
    JsonValueExpression Expression JsonPath
  | -- | This is for evaluating SQL functions, text(e1, e2, ..).
    FunctionExpression Text [Expression]
  | OpExpression Op Expression Expression
  | ListExpression [Expression]
  | STOpExpression SpatialOp Expression Expression
  | CastExpression Expression Text
  | -- | "CASE WHEN (expression) THEN (expression) ELSE (expression) END"
    ConditionalExpression Expression Expression Expression
  | -- | The 'DEFAULT' value. TODO: Make this as a part of @'ODBC.Value'.
    DefaultExpression

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

deriving instance Functor Countable

data From
  = FromQualifiedTable (Aliased TableName)
  | FromOpenJson (Aliased OpenJson)
  | FromSelect (Aliased Select)
  | FromIdentifier Text
  | FromTempTable (Aliased TempTableName)

data OpenJson = OpenJson
  { openJsonExpression :: Expression,
    openJsonWith :: Maybe (NonEmpty JsonFieldSpec)
  }

data JsonFieldSpec
  = IntField Text (Maybe JsonPath)
  | JsonField Text (Maybe JsonPath)
  | StringField Text (Maybe JsonPath)
  | UuidField Text (Maybe JsonPath)

data Aliased a = Aliased
  { aliasedThing :: !a,
    aliasedAlias :: !Text
  }

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  }

data TableName = TableName
  { tableName :: !Text,
    tableSchema :: !Text
  }

type FunctionName = Text -- TODO: Improve this type when SQL function support added to MSSQL

data FieldName = FieldName
  { fieldName :: Text,
    fieldNameEntity :: !Text
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
  | EQ'
  | NEQ'

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
newtype ColumnName = ColumnName {columnNameText :: Text}

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
  CharType -> "char"
  NumericType -> "numeric"
  DecimalType -> "decimal"
  IntegerType -> "int"
  SmallintType -> "smallint"
  FloatType -> "float"
  RealType -> "real"
  DateType -> "date"
  Ss_time2Type -> "time"
  VarcharType -> "varchar"
  WcharType -> "nchar"
  WvarcharType -> "nvarchar"
  WtextType -> "ntext"
  TextType -> "text"
  TimestampType -> "timestamp"
  BinaryType -> "binary"
  VarbinaryType -> "varbinary"
  BigintType -> "bigint"
  TinyintType -> "tinyint"
  BitType -> "bit"
  GuidType -> "uniqueidentifier"
  GeographyType -> "geography"
  GeometryType -> "geometry"
  -- the input form for types that aren't explicitly supported is a string
  UnknownType t -> t

mkMSSQLScalarTypeName :: MonadError QErr m => ScalarType -> m G.Name
mkMSSQLScalarTypeName = \case
  CharType -> pure RQL.stringScalar
  WcharType -> pure RQL.stringScalar
  WvarcharType -> pure RQL.stringScalar
  VarcharType -> pure RQL.stringScalar
  WtextType -> pure RQL.stringScalar
  TextType -> pure RQL.stringScalar
  FloatType -> pure RQL.floatScalar
  -- integer types
  IntegerType -> pure RQL.intScalar
  -- boolean type
  BitType -> pure RQL.boolScalar
  scalarType ->
    G.mkName (scalarTypeDBName scalarType)
      `onNothing` throw400
        ValidationFailed
        ( "cannot use SQL type " <> scalarTypeDBName scalarType <> " in the GraphQL schema because its name is not a "
            <> "valid GraphQL identifier"
        )

parseScalarValue :: ScalarType -> J.Value -> Either QErr Value
parseScalarValue scalarType jValue = case scalarType of
  -- bytestring
  CharType -> ODBC.ByteStringValue . encodeUtf8 <$> parseJValue jValue
  VarcharType -> ODBC.ByteStringValue . encodeUtf8 <$> parseJValue jValue
  -- text
  TextType -> ODBC.TextValue <$> parseJValue jValue
  WcharType -> ODBC.TextValue <$> parseJValue jValue
  WvarcharType -> ODBC.TextValue <$> parseJValue jValue
  WtextType -> ODBC.TextValue <$> parseJValue jValue
  -- integer
  IntegerType -> ODBC.IntValue <$> parseJValue jValue
  SmallintType -> ODBC.IntValue <$> parseJValue jValue
  BigintType -> ODBC.IntValue <$> parseJValue jValue
  TinyintType -> ODBC.IntValue <$> parseJValue jValue
  -- float
  NumericType -> ODBC.FloatValue <$> parseJValue jValue
  DecimalType -> ODBC.FloatValue <$> parseJValue jValue
  FloatType -> ODBC.FloatValue <$> parseJValue jValue
  RealType -> ODBC.FloatValue <$> parseJValue jValue
  -- boolean
  BitType -> ODBC.ByteValue <$> parseJValue jValue
  -- geo
  GeographyType -> ODBC.TextValue <$> parseGeoTypes jValue
  GeometryType -> ODBC.TextValue <$> parseGeoTypes jValue
  -- misc
  BinaryType -> ODBC.BinaryValue . ODBC.Binary . txtToBs <$> parseJValue jValue
  VarbinaryType -> ODBC.BinaryValue . ODBC.Binary . txtToBs <$> parseJValue jValue
  Ss_time2Type -> ODBC.TimeOfDayValue <$> parseJValue jValue
  TimestampType -> ODBC.LocalTimeValue <$> parseJValue jValue
  DateType -> ODBC.DayValue <$> parseJValue jValue
  GuidType -> ODBC.TextValue <$> parseJValue jValue
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
  BinaryType -> False
  VarbinaryType -> False
  BitType -> False
  GuidType -> False
  _ -> True
isNumType = \case
  NumericType -> True
  DecimalType -> True
  IntegerType -> True
  SmallintType -> True
  FloatType -> True
  RealType -> True
  BigintType -> True
  TinyintType -> True
  _ -> False

getGQLTableName :: TableName -> Either QErr G.Name
getGQLTableName tn = do
  let textName = snakeCaseTableName tn
  onNothing (G.mkName $ snakeCaseTableName tn) $
    throw400 ValidationFailed $
      "cannot include " <> textName <> " in the GraphQL schema because it is not a valid GraphQL identifier"

snakeCaseTableName :: TableName -> Text
snakeCaseTableName TableName {tableName, tableSchema} =
  if tableSchema == "dbo"
    then tableName
    else tableSchema <> "_" <> tableName

stringTypes :: [ScalarType]
stringTypes =
  [ CharType,
    VarcharType,
    TextType,
    WcharType,
    WvarcharType,
    WtextType
  ]

geoTypes :: [ScalarType]
geoTypes = [GeometryType, GeographyType]
