{-# LANGUAGE DuplicateRecordFields #-}

-- | MSSQL Types Internal
--
-- Types for Transact-SQL aka T-SQL; the language of SQL Server.
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
    Declare (..),
    ColumnName (..),
    columnNameToFieldName,
    ColumnType,
    Comment (..),
    ConstraintName (..),
    Countable (..),
    CountType (..),
    DataLength (..),
    Delete (..),
    DeleteOutput,
    EntityAlias (..),
    fromAlias,
    Expression (..),
    FieldName (..),
    For (..),
    ForJson (..),
    From (..),
    FunctionApplicationExpression (..),
    FunctionName (..),
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
    MethodApplicationExpression (..),
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
    QueryWithDDL (..),
    Reselect (..),
    Root (..),
    ScalarType (..),
    SchemaName (..),
    Select (..),
    SetIdentityInsert (..),
    TempTableDDL (..),
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
    CTEBody (..),
    emptySelect,
    geoTypes,
    getGQLTableName,
    getGQLFunctionName,
    getTableIdentifier,
    isComparableType,
    isNumType,
    mkMSSQLScalarTypeName,
    parseScalarValue,
    parseScalarType,
    scalarTypeDBName,
    snakeCaseName,
    stringTypes,
    namingConventionSupport,
  )
where

import Data.Aeson qualified as J
import Data.Text qualified as T
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.NativeQuery.Metadata (InterpolatedQuery)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnRedactionExp)
import Hasura.RQL.Types.Backend (SupportedNamingCase (..))
import Hasura.RQL.Types.BackendType
import Hasura.SQL.GeoJSON qualified as Geo
import Hasura.SQL.WKT qualified as WKT
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------
-- Phantom pretend-generic types that are actually specific

type Column (b :: BackendType) = ColumnName

type ColumnType (b :: BackendType) = ScalarType

type Value = ODBC.Value

--------------------------------------------------------------------------------

data UnifiedColumn = UnifiedColumn
  { name :: ColumnName,
    type' :: ScalarType
  }

data UnifiedTableName = UnifiedTableName
  { schema :: Text,
    name :: Text
  }

data UnifiedObjectRelationship = UnifiedObjectRelationship
  { using :: UnifiedUsing,
    name :: Text
  }

data UnifiedArrayRelationship = UnifiedArrayRelationship
  { using :: UnifiedUsing,
    name :: Text
  }

newtype UnifiedUsing = UnifiedUsing
  { foreign_key_constraint_on :: UnifiedOn
  }

data UnifiedOn = UnifiedOn
  { table :: UnifiedTableName,
    column :: Text
  }

-------------------------------------------------------------------------------
-- AST types

data BooleanOperators a
  = ASTContains a
  | ASTCrosses a
  | ASTEquals a
  | ASTIntersects a
  | ASTOverlaps a
  | ASTTouches a
  | ASTWithin a

data Select = Select
  { selectWith :: (Maybe With),
    selectTop :: Top,
    selectProjections :: [Projection],
    selectFrom :: (Maybe From),
    selectJoins :: [Join],
    selectWhere :: Where,
    selectFor :: For,
    selectOrderBy :: (Maybe (NonEmpty OrderBy)),
    selectOffset :: (Maybe Expression)
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
  { outputType :: t,
    outputColumns :: [OutputColumn]
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
    setValue :: SetValue
  }

type DeleteOutput = Output Deleted

data Delete = Delete
  { deleteTable :: (Aliased TableName),
    deleteOutput :: DeleteOutput,
    deleteTempTable :: TempTable,
    deleteWhere :: Where
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

-- | A name of a regular table or temporary table
data SomeTableName
  = RegularTableName TableName
  | TemporaryTableName TempTableName

data TempTable = TempTable
  { ttName :: TempTableName,
    ttColumns :: [ColumnName]
  }

-- | A version of `Select` without a `FROM` clause. This means it can only project expressions already selected in adjacent join clauses, hence the name @reselect@.
data Reselect = Reselect
  { reselectProjections :: [Projection],
    reselectFor :: For,
    reselectWhere :: Where
  }

data OrderBy = OrderBy
  { orderByExpression :: Expression,
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
  { joinSource :: JoinSource,
    joinJoinAlias :: JoinAlias,
    joinWhere :: Where
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
  = With (NonEmpty (Aliased CTEBody))
  deriving (Semigroup)

-- | Something that can appear in a CTE body.
data CTEBody
  = CTESelect Select
  | CTEUnsafeRawSQL (InterpolatedQuery Expression)

-- | Extra query steps that can be emitted from the main
-- query to do things like setup temp tables
data TempTableDDL
  = -- | create a temp table
    TempTableCreate TempTableName [UnifiedColumn]
  | -- | insert output of a statement into a temp table
    TempTableInsert TempTableName [Declare] (InterpolatedQuery Expression)
  | -- | Drop a temp table
    TempTableDrop TempTableName

data Declare = Declare
  { dName :: Text,
    dType :: ScalarType,
    dValue :: Expression
  }

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
  | MethodApplicationExpression Expression MethodApplicationExpression
  | FunctionApplicationExpression FunctionApplicationExpression
  | -- | This is for getting actual atomic values out of a JSON
    -- string.
    JsonValueExpression Expression JsonPath
  | OpExpression Op Expression Expression
  | ListExpression [Expression]
  | STOpExpression SpatialOp Expression Expression
  | CastExpression Expression ScalarType DataLength
  | -- | "CASE WHEN (expression) THEN (expression) ELSE (expression) END"
    ConditionalExpression Expression Expression Expression
  | -- | The 'DEFAULT' value. TODO: Make this as a part of @'ODBC.Value'.
    DefaultExpression

-- | Data type describing the length of a datatype. Used in 'CastExpression's.
data DataLength = DataLengthUnspecified | DataLengthInt Int | DataLengthMax

-- | SQL functions application: @some_function(e1, e2, ..)@.
data FunctionApplicationExpression
  = FunExpISNULL Expression Expression -- ISNULL

-- | Object expression method application: @(expression).text(e1, e2, ..)@
data MethodApplicationExpression
  = MethExpSTAsText -- STAsText

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer

data Aggregate
  = CountAggregate (Countable Expression)
  | OpAggregate Text [Expression]
  | TextAggregate Text

newtype CountType field = CountType {getCountType :: Countable (ColumnName, AnnRedactionExp 'MSSQL field)}

data Countable name
  = StarCountable
  | NonNullFieldCountable name
  | DistinctCountable name

deriving instance Functor Countable

data From
  = FromQualifiedTable (Aliased TableName)
  | FromOpenJson (Aliased OpenJson)
  | FromSelect (Aliased Select)
  | FromIdentifier Text
  | FromTempTable (Aliased TempTableName)

-- | Extract the name bound in a 'From' clause as an 'EntityAlias'.
fromAlias :: From -> EntityAlias
fromAlias (FromQualifiedTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromOpenJson Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromSelect Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromIdentifier identifier) = EntityAlias identifier
fromAlias (FromTempTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias

data OpenJson = OpenJson
  { openJsonExpression :: Expression,
    openJsonWith :: Maybe (NonEmpty JsonFieldSpec)
  }

data JsonFieldSpec
  = ScalarField ScalarType DataLength Text (Maybe JsonPath)
  | JsonField Text (Maybe JsonPath)
  | StringField Text (Maybe JsonPath)

data Aliased a = Aliased
  { aliasedThing :: a,
    aliasedAlias :: Text
  }

newtype SchemaName = SchemaName {_unSchemaName :: Text}
  deriving (Show, Eq, Ord, Data, J.ToJSON, J.FromJSON, NFData, Generic, IsString, Hashable, Lift)

data TableName = TableName
  { tableName :: Text,
    tableSchema :: SchemaName
  }

data FieldName = FieldName
  { fieldName :: Text,
    fieldNameEntity :: Text
  }

data Comment = DueToPermission | RequestedSingleObject

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  }

columnNameToFieldName :: ColumnName -> EntityAlias -> FieldName
columnNameToFieldName (ColumnName fieldName) EntityAlias {entityAliasText = fieldNameEntity} =
  FieldName {fieldName, fieldNameEntity}

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

newtype ConstraintName = ConstraintName {constraintNameText :: Text}
  deriving newtype (J.FromJSONKey, J.ToJSONKey)

data FunctionName = FunctionName
  { functionName :: Text,
    functionSchema :: SchemaName
  }

-- | type for a query generated from IR along with any DDL actions
data QueryWithDDL a = QueryWithDDL
  { qwdBeforeSteps :: [TempTableDDL],
    qwdQuery :: a,
    qwdAfterSteps :: [TempTableDDL]
  }

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
  | UnknownType Text

scalarTypeDBName :: DataLength -> ScalarType -> Text
scalarTypeDBName dataLength = \case
  CharType -> "char"
  NumericType -> "numeric"
  DecimalType -> "decimal"
  IntegerType -> "int"
  SmallintType -> "smallint"
  FloatType -> "float"
  RealType -> "real"
  DateType -> "date"
  Ss_time2Type -> "time"
  VarcharType -> "varchar" <> fromDataLength dataLength
  WcharType -> "nchar"
  WvarcharType -> "nvarchar" <> fromDataLength dataLength
  WtextType -> "ntext"
  TextType -> "text"
  TimestampType -> "timestamp"
  BinaryType -> "binary"
  VarbinaryType -> "varbinary" <> fromDataLength dataLength
  BigintType -> "bigint"
  TinyintType -> "tinyint"
  BitType -> "bit"
  GuidType -> "uniqueidentifier"
  GeographyType -> "geography"
  GeometryType -> "geometry"
  -- the input form for types that aren't explicitly supported is a string
  UnknownType t -> t

fromDataLength :: DataLength -> Text
fromDataLength = \case
  DataLengthUnspecified -> ""
  DataLengthInt len -> "(" <> tshow len <> ")"
  DataLengthMax -> "(max)"

mkMSSQLScalarTypeName :: (MonadError QErr m) => ScalarType -> m G.Name
mkMSSQLScalarTypeName = \case
  CharType -> pure GName._String
  WcharType -> pure GName._String
  WvarcharType -> pure GName._String
  VarcharType -> pure GName._String
  WtextType -> pure GName._String
  TextType -> pure GName._String
  FloatType -> pure GName._Float
  -- integer types
  IntegerType -> pure GName._Int
  -- boolean type
  BitType -> pure GName._Boolean
  scalarType ->
    G.mkName (scalarTypeDBName DataLengthUnspecified scalarType)
      `onNothing` throw400
        ValidationFailed
        ( "cannot use SQL type "
            <> scalarTypeDBName DataLengthUnspecified scalarType
            <> " in the GraphQL schema because its name is not a "
            <> "valid GraphQL identifier"
        )

parseScalarType :: Text -> ScalarType
parseScalarType = \case
  "char" -> CharType
  "numeric" -> NumericType
  "decimal" -> DecimalType
  "money" -> DecimalType
  "smallmoney" -> DecimalType
  "int" -> IntegerType
  "smallint" -> SmallintType
  "float" -> FloatType
  "real" -> RealType
  "date" -> DateType
  "time" -> Ss_time2Type
  "varchar" -> VarcharType
  "nchar" -> WcharType
  "nvarchar" -> WvarcharType
  "ntext" -> WtextType
  "timestamp" -> TimestampType
  "text" -> TextType
  "binary" -> BinaryType
  "bigint" -> BigintType
  "tinyint" -> TinyintType
  "varbinary" -> VarbinaryType
  "bit" -> BitType
  "uniqueidentifier" -> GuidType
  "geography" -> GeographyType
  "geometry" -> GeometryType
  t ->
    -- if the type is something like `varchar(127)`, try stripping off the data length
    if T.isInfixOf "(" t
      then parseScalarType (T.takeWhile (\c -> c /= '(') t)
      else UnknownType t

parseScalarValue :: ScalarType -> J.Value -> Either QErr Value
parseScalarValue scalarType jValue = case scalarType of
  -- text
  CharType -> ODBC.TextValue <$> parseJValue jValue
  VarcharType -> ODBC.TextValue <$> parseJValue jValue
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
        >>= fmap WKT.getWKT
        . WKT.toWKT

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
  let textName = snakeCaseName (tableName tn) (tableSchema tn)
  onNothing (G.mkName textName)
    $ throw400 ValidationFailed
    $ "cannot include "
    <> textName
    <> " in the GraphQL schema because it is not a valid GraphQL identifier"

getGQLFunctionName :: FunctionName -> Either QErr G.Name
getGQLFunctionName fn = do
  let textName = snakeCaseName (functionName fn) (functionSchema fn)
  onNothing (G.mkName textName)
    $ throw400 ValidationFailed
    $ "cannot include "
    <> textName
    <> " in the GraphQL schema because it is not a valid GraphQL identifier"

snakeCaseName :: Text -> SchemaName -> Text
snakeCaseName tableName (SchemaName tableSchema) =
  if tableSchema == "dbo"
    then tableName
    else tableSchema <> "_" <> tableName

getTableIdentifier :: TableName -> Either QErr GQLNameIdentifier
getTableIdentifier tName = do
  gqlTableName <- getGQLTableName tName
  pure $ C.fromAutogeneratedName gqlTableName

namingConventionSupport :: SupportedNamingCase
namingConventionSupport = OnlyHasuraCase

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
