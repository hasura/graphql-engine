{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types for BigQuery
module Hasura.Backends.BigQuery.Types
  ( Aggregate (..),
    Aliased (..),
    ArrayAgg (..),
    Base64,
    BigDecimal (..),
    BooleanOperators (..),
    Cardinality (..),
    ColumnName (ColumnName),
    Countable (..),
    CountType (..),
    Date (..),
    Datetime (..),
    Decimal (..),
    EntityAlias (..),
    ExecutionStatistics (..),
    Expression (..),
    FieldName (..),
    FieldOrigin (..),
    Float64,
    From (..),
    SelectFromFunction (..),
    Geography (Geography),
    Int64 (Int64),
    Job (..),
    Join (..),
    JoinProvenance (ArrayAggregateJoinProvenance, ArrayJoinProvenance, ObjectJoinProvenance, OrderByJoinProvenance),
    JoinSource (..),
    JoinType (..),
    JsonPath (..),
    NullsOrder (..),
    Op (..),
    Order (..),
    OrderBy (..),
    Projection (..),
    Reselect (..),
    ScalarType (..),
    Select (..),
    AsStruct (..),
    PartitionableSelect (..),
    noExtraPartitionFields,
    withExtraPartitionFields,
    simpleSelect,
    SelectJson (..),
    TableName (..),
    Time (..),
    Timestamp (..),
    Top (..),
    TypedValue (..),
    Value (..),
    Where (..),
    With (..),
    WindowFunction (..),
    aggregateProjectionsFieldOrigin,
    doubleToBigDecimal,
    doubleToFloat64,
    getGQLTableName,
    intToInt64,
    int64Expr,
    isComparableType,
    isNumType,
    parseScalarValue,
    projectionAlias,
    scientificToText,
    columnToFieldName,
    FunctionName (..),
    ComputedFieldDefinition (..),
    ArgumentExp (..),
    ComputedFieldImplicitArguments,
    ComputedFieldReturn (..),
    FunctionArgument (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec, object, optionalField', requiredField', (.=))
import Autodocodec qualified as AC
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Extended qualified as J
import Data.Aeson.Types qualified as J
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as L
import Data.Coerce
import Data.Int qualified as Int
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Extended
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Data.Vector (Vector)
import Data.Vector.Instances ()
import Hasura.Base.Error
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Function.Cache (FunctionArgName)
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.NativeQuery.Metadata (InterpolatedQuery, NativeQueryName)
import Hasura.Prelude hiding (state)
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend qualified as Backend
import Hasura.RQL.Types.BackendType (BackendType (BigQuery))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax hiding (location)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

data Select = Select
  { selectWith :: Maybe With,
    selectTop :: Top,
    selectAsStruct :: AsStruct,
    selectProjections :: NonEmpty Projection,
    selectFrom :: From,
    selectJoins :: [Join],
    selectWhere :: Where,
    selectOrderBy :: Maybe (NonEmpty OrderBy),
    selectOffset :: Maybe Expression,
    selectGroupBy :: [FieldName],
    selectFinalWantedFields :: Maybe [Text],
    selectCardinality :: Cardinality
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

-- | Helper type allowing addition of extra fields used
-- in PARTITION BY.
--
-- The main purpose of this type is sumulation of DISTINCT ON
-- implemented in Hasura.Backends.BigQuery.FromIr.simulateDistinctOn
data PartitionableSelect = PartitionableSelect
  { pselectFinalize :: Maybe [FieldName] -> Select,
    pselectFrom :: From
  }

simpleSelect :: Select -> PartitionableSelect
simpleSelect select =
  PartitionableSelect
    { pselectFinalize = const select,
      pselectFrom = selectFrom select
    }

noExtraPartitionFields :: PartitionableSelect -> Select
noExtraPartitionFields PartitionableSelect {..} = pselectFinalize Nothing

withExtraPartitionFields :: PartitionableSelect -> [FieldName] -> Select
withExtraPartitionFields PartitionableSelect {..} = pselectFinalize . Just

data ArrayAgg = ArrayAgg
  { arrayAggProjections :: NonEmpty Projection,
    arrayAggOrderBy :: Maybe (NonEmpty OrderBy),
    arrayAggTop :: Top
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data Reselect = Reselect
  { reselectProjections :: NonEmpty Projection,
    reselectWhere :: Where
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data OrderBy = OrderBy
  { orderByExpression :: Expression,
    orderByFieldName :: FieldName,
    orderByOrder :: Order,
    orderByNullsOrder :: NullsOrder
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data Order
  = AscOrder
  | DescOrder
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data FieldOrigin
  = NoOrigin
  | AggregateOrigin [Aliased Aggregate]
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

aggregateProjectionsFieldOrigin :: Projection -> FieldOrigin
aggregateProjectionsFieldOrigin = \case
  AggregateProjections a -> AggregateOrigin . toList . aliasedThing $ a
  AggregateProjection a -> AggregateOrigin [a]
  _ -> NoOrigin

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjections (Aliased (NonEmpty (Aliased Aggregate)))
  | AggregateProjection (Aliased Aggregate)
  | StarProjection
  | ArrayAggProjection (Aliased ArrayAgg)
  | EntityProjection (Aliased [(FieldName, FieldOrigin)])
  | ArrayEntityProjection EntityAlias (Aliased [FieldName])
  | WindowProjection (Aliased WindowFunction)
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (Hashable, NFData)

data WindowFunction
  = -- | ROW_NUMBER() OVER(PARTITION BY field)
    RowNumberOverPartitionBy (NonEmpty FieldName) (Maybe (NonEmpty OrderBy))
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (Hashable, NFData)

data JoinType = LeftOuter | Inner
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data Join = Join
  { joinSource :: JoinSource,
    joinAlias :: EntityAlias,
    joinOn :: [(FieldName, FieldName)],
    joinProvenance :: JoinProvenance,
    joinFieldName :: Text,
    joinExtractPath :: Maybe Text,
    joinRightTable :: EntityAlias,
    joinType :: JoinType
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data JoinProvenance
  = OrderByJoinProvenance
  | ObjectJoinProvenance [Text]
  | ArrayAggregateJoinProvenance [(Text, FieldOrigin)]
  | ArrayJoinProvenance [Text]
  | MultiplexProvenance
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data JoinSource
  = JoinSelect Select
  -- We're not using existingJoins at the moment, which was used to
  -- avoid re-joining on the same table twice.
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

newtype Where
  = Where [Expression]
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving newtype (Hashable, Monoid, NFData, Semigroup)

data Cardinality
  = Many
  | One
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data AsStruct
  = NoAsStruct
  | AsStruct
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

-- | A Common Table Expression clause.
newtype With = With (NonEmpty (Aliased (InterpolatedQuery Expression)))
  deriving stock (Data, Generic, Lift)
  deriving newtype (Eq, Hashable, NFData, Ord, Semigroup, Show)

data Top
  = NoTop
  | Top Int.Int64
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x = x
  (<>) x NoTop = x
  (<>) (Top x) (Top y) = Top (min x y)

data Expression
  = ValueExpression TypedValue
  | InExpression Expression TypedValue
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
  | -- | This one acts like a "cast to JSON" and makes SQL Server
    -- behave like it knows your field is JSON and not double-encode
    -- it.
    JsonQueryExpression Expression
  | ToStringExpression Expression
  | -- | This is for getting actual atomic values out of a JSON
    -- string.
    JsonValueExpression Expression JsonPath
  | OpExpression Op Expression Expression
  | ListExpression [Expression]
  | CastExpression Expression ScalarType
  | FunctionExpression FunctionName [Expression]
  | ConditionalProjection Expression FieldName
  | -- | A function input argument expression with argument name
    -- `argument_name` => 'argument_value'
    FunctionNamedArgument Text Expression
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data Aggregate
  = CountAggregate (Countable Expression)
  | OpAggregates Text (NonEmpty (Text, Expression))
  | OpAggregate Text Expression
  | TextAggregate Text
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

newtype CountType field = CountType {getCountType :: Countable (ColumnName, AnnRedactionExp 'BigQuery field)}

deriving stock instance (Backend.Backend 'BigQuery) => Foldable CountType

deriving stock instance (Backend.Backend 'BigQuery) => Functor CountType

deriving stock instance (Backend.Backend 'BigQuery) => Traversable CountType

deriving stock instance
  ( Backend.Backend 'BigQuery,
    Eq field,
    Eq (Backend.AggregationPredicates 'BigQuery field),
    Eq (Backend.BooleanOperators 'BigQuery field),
    Eq (Backend.FunctionArgumentExp 'BigQuery field)
  ) =>
  Eq (CountType field)

deriving stock instance
  ( Backend.Backend 'BigQuery,
    Show field,
    Show (Backend.AggregationPredicates 'BigQuery field),
    Show (Backend.BooleanOperators 'BigQuery field),
    Show (Backend.FunctionArgumentExp 'BigQuery field)
  ) =>
  Show (CountType field)

data Countable fieldname
  = StarCountable
  | NonNullFieldCountable (NonEmpty fieldname)
  | DistinctCountable (NonEmpty fieldname)
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving stock (Foldable, Functor, Traversable)

deriving anyclass instance (FromJSON a) => FromJSON (Countable a)

deriving anyclass instance (Hashable a) => Hashable (Countable a)

deriving anyclass instance (ToJSON a) => ToJSON (Countable a)

deriving anyclass instance (NFData a) => NFData (Countable a)

data From
  = FromQualifiedTable (Aliased TableName)
  | FromSelect (Aliased Select)
  | FromSelectJson (Aliased SelectJson)
  | FromFunction (Aliased SelectFromFunction)
  | FromNativeQuery (Aliased NativeQueryName)
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (Hashable, NFData)

data SelectJson = SelectJson
  { selectJsonBody :: Expression,
    selectJsonFields :: [(ColumnName, ScalarType)]
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data SelectFromFunction = SelectFromFunction
  { sffFunctionName :: FunctionName,
    sffArguments :: [Expression]
  }
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (Hashable, NFData)

data OpenJson = OpenJson
  { openJsonExpression :: Expression,
    openJsonWith :: NonEmpty JsonFieldSpec
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

data JsonFieldSpec
  = IntField Text
  | JsonField Text
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data Aliased a = Aliased
  { aliasedThing :: a,
    aliasedAlias :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift, Functor)

deriving anyclass instance (FromJSON a) => FromJSON (Aliased a)

deriving anyclass instance (Hashable a) => Hashable (Aliased a)

deriving anyclass instance (ToJSON a) => ToJSON (Aliased a)

deriving anyclass instance (NFData a) => NFData (Aliased a)

data TableName = TableName
  { tableName :: Text,
    tableNameSchema :: Text
  }
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (Hashable, NFData, ToJSONKey)

instance HasCodec TableName where
  codec =
    object "BigQueryTableName"
      $ TableName
      <$> requiredField' "name"
      .= tableName
        <*> requiredField' "dataset"
      .= tableNameSchema

instance FromJSON TableName where
  parseJSON =
    J.withObject
      "TableName"
      (\o -> TableName <$> o J..: "name" <*> o J..: "dataset")

instance ToJSON TableName where
  toJSON TableName {..} = J.object ["name" J..= tableName, "dataset" J..= tableNameSchema]

instance ToTxt TableName where
  toTxt TableName {..} = tableNameSchema <> "." <> tableName

instance ToErrorValue TableName where
  toErrorValue = ErrorValue.squote . toTxt

data FieldName = FieldName
  { fieldName :: Text,
    fieldNameEntity :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

newtype ColumnName = ColumnName
  { columnName :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving newtype (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable, NFData, ToTxt)

instance HasCodec ColumnName where
  codec = dimapCodec ColumnName columnName codec

instance ToErrorValue ColumnName where
  toErrorValue = ErrorValue.squote . columnName

data Comment = DueToPermission | RequestedSingleObject
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving newtype (FromJSON, Hashable, NFData, ToJSON)

columnToFieldName :: EntityAlias -> ColumnName -> FieldName
columnToFieldName EntityAlias {..} ColumnName {..} =
  FieldName columnName entityAliasText

data Op
  = LessOp
  | LessOrEqualOp
  | MoreOp
  | MoreOrEqualOp
  | InOp
  | NotInOp
  | LikeOp
  | NotLikeOp
  --  | SNE
  --  | SILIKE
  --  | SNILIKE
  --  | SSIMILAR
  --  | SNSIMILAR
  --  | SGTE
  --  | SLTE
  --  | SContains
  --  | SContainedIn
  --  | SHasKey
  --  | SHasKeysAny
  --  | SHasKeysAll
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

-- | Source for this represenation type:
--
-- https://developers.google.com/protocol-buffers/docs/reference/google.protobuf#google.protobuf.Value
--
-- BigQuery results come in via the REST API as one of these simply types.
--
-- TODO: This omits StructValue -- do we need it?
data Value
  = NullValue
  | -- | 64-bit <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#integer_type>
    IntegerValue Int64
  | -- | Fixed precision <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#decimal_types>
    DecimalValue Decimal
  | -- | Fixed precision <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#decimal_types>
    BigDecimalValue BigDecimal
  | -- | Floating point <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#floating_point_types>
    FloatValue Float64
  | GeographyValue Geography
  | StringValue Text
  | BytesValue Base64
  | BoolValue Bool
  | ArrayValue (Vector Value)
  | TimestampValue Timestamp
  | DateValue Date
  | TimeValue Time
  | JsonValue J.Value
  | DatetimeValue Datetime
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

-- | BigQuery's conception of a timestamp.
newtype Timestamp = Timestamp Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (ToJSON, FromJSON, NFData, Hashable)

-- | BigQuery's conception of a date.
newtype Date = Date Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (ToJSON, FromJSON, NFData, Hashable)

-- | BigQuery's conception of a time.
newtype Time = Time Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (ToJSON, FromJSON, NFData, Hashable)

-- | BigQuery's conception of a datetime.
newtype Datetime = Datetime Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (ToJSON, FromJSON, NFData, Hashable)

-- | BigQuery's conception of an INTEGER/INT64 (they are the same).
newtype Int64 = Int64 Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (NFData, Hashable)

instance FromJSON Int64 where parseJSON = liberalInt64Parser Int64

instance ToJSON Int64 where toJSON = liberalIntegralPrinter

data TypedValue = TypedValue
  { tvType :: ScalarType,
    tvValue :: Value
  }
  deriving stock (Eq, Ord, Show, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData)

intToInt64 :: Int.Int64 -> Int64
intToInt64 = Int64 . tshow

int64Expr :: Int.Int64 -> Expression
int64Expr i = ValueExpression (TypedValue IntegerScalarType (IntegerValue (intToInt64 i)))

-- | BigQuery's conception of a fixed precision decimal.
newtype Decimal = Decimal Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (NFData, Hashable)

instance FromJSON Decimal where
  parseJSON (J.Number num) = pure $ Decimal $ scientificToText num
  parseJSON (J.String num) = pure $ Decimal num
  parseJSON _ = fail "parseJSON: FromJSON Decimal failure"

instance ToJSON Decimal where
  toJSON (Decimal x) = J.toJSON x

-- | Convert 'Scientific' to 'Text'
scientificToText :: Scientific -> Text
scientificToText num = toStrict $ toLazyText $ formatScientificBuilder Fixed Nothing num

-- | BigQuery's conception of a \"big\" fixed precision decimal.
newtype BigDecimal = BigDecimal Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (NFData, Hashable)

instance FromJSON BigDecimal where
  parseJSON (J.Number num) = pure $ BigDecimal $ scientificToText num
  parseJSON (J.String num) = pure $ BigDecimal num
  parseJSON _ = fail "parseJSON: FromJSON BigDecimal failure"

instance ToJSON BigDecimal where
  toJSON (BigDecimal x) = J.toJSON x

doubleToBigDecimal :: Double -> BigDecimal
doubleToBigDecimal = BigDecimal . T.decodeUtf8 . L.toStrict . J.encode

-- | BigQuery's conception of a fixed precision decimal.
newtype Float64 = Float64 Text
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (NFData, Hashable)

instance FromJSON Float64 where parseJSON = liberalDecimalParser Float64

instance ToJSON Float64 where toJSON = liberalDecimalPrinter

doubleToFloat64 :: Double -> Float64
doubleToFloat64 = Float64 . T.decodeUtf8 . L.toStrict . J.encode

-- | A base-64 encoded binary string.
newtype Base64 = Base64
  { unBase64 :: ByteString
  }
  deriving (Show, Eq, Ord, Generic, Data, Lift)

instance FromJSON Base64 where parseJSON = fmap (Base64 . L.toStrict . base64Decode) . J.parseJSON

instance ToJSON Base64 where toJSON = J.toJSON . T.decodeUtf8 . Base64.encode . unBase64

instance NFData Base64

instance Hashable Base64

newtype Geography = Geography
  { unGeography :: Text
  }
  deriving stock (Show, Eq, Ord, Generic, Data, Lift)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (Hashable, NFData)

data ScalarType
  = StringScalarType
  | BytesScalarType
  | IntegerScalarType
  | FloatScalarType
  | BoolScalarType
  | TimestampScalarType
  | DateScalarType
  | TimeScalarType
  | DatetimeScalarType
  | GeographyScalarType
  | DecimalScalarType
  | BigDecimalScalarType
  | JsonScalarType
  | StructScalarType
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Data, Lift)
  deriving anyclass (Hashable, NFData, ToJSONKey)

-- I do not know how to make Autodocodec case-insensitive or strip out the
-- length stuff, so here we are
instance HasCodec ScalarType where
  codec =
    AC.CommentCodec
      ("A scalar type for BigQuery")
      $ placeholderCodecViaJSON

-- https://hasura.io/docs/latest/schema/bigquery/bigquery-types/
instance FromJSON ScalarType where
  parseJSON (J.String s) = parseScalarType (T.toLower s)
    where
      parseScalarType = \case
        "string" -> pure StringScalarType
        "bytes" -> pure BytesScalarType
        "integer" -> pure IntegerScalarType
        "int64" -> pure IntegerScalarType
        "float" -> pure FloatScalarType
        "float64" -> pure FloatScalarType
        "bool" -> pure BoolScalarType
        "timestamp" -> pure TimestampScalarType
        "date" -> pure DateScalarType
        "time" -> pure TimeScalarType
        "datetime" -> pure DatetimeScalarType
        "geography" -> pure GeographyScalarType
        "decimal" -> pure DecimalScalarType
        "numeric" -> pure DecimalScalarType
        "bigdecimal" -> pure BigDecimalScalarType
        "bignumeric" -> pure BigDecimalScalarType
        "json" -> pure JsonScalarType
        "struct" -> pure StructScalarType
        t ->
          -- if the type is something like `varchar(127)`, try stripping off the data length
          if T.isInfixOf "(" t
            then parseScalarType (T.takeWhile (\c -> c /= '(') t)
            else fail $ "Did not recognize scalar type '" <> T.unpack t <> "'"
  parseJSON _ = fail "expected a string"

instance ToJSON ScalarType where
  toJSON = \case
    StringScalarType -> "STRING"
    BytesScalarType -> "BYTES"
    IntegerScalarType -> "INT64"
    FloatScalarType -> "FLOAT64"
    BoolScalarType -> "BOOL"
    TimestampScalarType -> "TIMESTAMP"
    DateScalarType -> "DATE"
    TimeScalarType -> "TIME"
    DatetimeScalarType -> "DATETIME"
    GeographyScalarType -> "GEOGRAPHY"
    DecimalScalarType -> "DECIMAL"
    BigDecimalScalarType -> "BIGDECIMAL"
    JsonScalarType -> "JSON"
    StructScalarType -> "STRUCT"

instance ToTxt ScalarType where toTxt = tshow

instance ToErrorValue ScalarType where
  toErrorValue = ErrorValue.squote . tshow

--------------------------------------------------------------------------------
-- Unified table metadata

data UnifiedMetadata = UnifiedMetadata
  { tables :: [UnifiedTableMetadata]
  }
  deriving (Eq, Ord, Show)

data UnifiedTableMetadata = UnifiedTableMetadata
  { table :: UnifiedTableName,
    object_relationships :: [UnifiedObjectRelationship],
    array_relationships :: [UnifiedArrayRelationship],
    columns :: [UnifiedColumn]
  }
  deriving (Eq, Ord, Show)

data UnifiedColumn = UnifiedColumn
  { name :: Text,
    type' :: ScalarType
  }
  deriving (Eq, Ord, Show)

data UnifiedTableName = UnifiedTableName
  { schema :: Text,
    name :: Text
  }
  deriving (Eq, Ord, Show)

data UnifiedObjectRelationship = UnifiedObjectRelationship
  { using :: UnifiedUsing,
    name :: Text
  }
  deriving (Eq, Ord, Show)

data UnifiedArrayRelationship = UnifiedArrayRelationship
  { using :: UnifiedUsing,
    name :: Text
  }
  deriving (Eq, Ord, Show)

data UnifiedUsing = UnifiedUsing
  { foreign_key_constraint_on :: UnifiedOn
  }
  deriving (Eq, Ord, Show)

data UnifiedOn = UnifiedOn
  { table :: UnifiedTableName,
    column :: Text
  }
  deriving (Eq, Ord, Show)

data BooleanOperators a
  = ASTContains a
  | ASTEquals a
  | ASTTouches a
  | ASTWithin a
  | ASTIntersects a
  | ASTDWithin (DWithinGeogOp a)
  deriving stock (Eq, Generic, Foldable, Functor, Traversable, Show)

instance (NFData a) => NFData (BooleanOperators a)

instance (Hashable a) => Hashable (BooleanOperators a)

instance (ToJSON a) => J.ToJSONKeyValue (BooleanOperators a) where
  toJSONKeyValue = \case
    ASTContains a -> ("_st_contains", J.toJSON a)
    ASTEquals a -> ("_st_equals", J.toJSON a)
    ASTIntersects a -> ("_st_intersects", J.toJSON a)
    ASTTouches a -> ("_st_touches", J.toJSON a)
    ASTWithin a -> ("_st_within", J.toJSON a)
    ASTDWithin a -> ("_st_dwithin", J.toJSON a)

data FunctionName = FunctionName
  { functionName :: Text,
    -- | System functions like "unnest" don't have schema/dataset
    functionNameSchema :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Data, Lift, Ord)
  deriving anyclass (Hashable, NFData, ToJSONKey)

instance HasCodec FunctionName where
  codec =
    object "BigQueryFunctionName"
      $ FunctionName
      <$> requiredField' "name"
      .= functionName
        <*> optionalField' "dataset"
      .= functionNameSchema

instance FromJSON FunctionName where
  parseJSON =
    J.withObject
      "FunctionName"
      (\o -> FunctionName <$> o J..: "name" <*> o J..:? "dataset")

instance ToJSON FunctionName where
  toJSON FunctionName {..} = J.object ["name" J..= functionName, "dataset" J..= functionNameSchema]

instance ToTxt FunctionName where
  toTxt FunctionName {..} =
    case functionNameSchema of
      Nothing -> functionName
      Just schemaName -> schemaName <> "." <> functionName

instance ToErrorValue FunctionName where
  toErrorValue = ErrorValue.squote . toTxt

-- | The metadata required to define a computed field for a BigQuery table
data ComputedFieldDefinition = ComputedFieldDefinition
  { -- | Name of the user defined routine
    _bqcfdFunction :: FunctionName,
    -- | Name of the table which the function returns. If not provided
    -- the return table schema is inferred from the routine API metadata.
    _bqcfdReturnTable :: Maybe TableName,
    -- | A mapping context to determine argument value from table column
    _bqcfdArgumentMapping :: HashMap FunctionArgName ColumnName
  }
  deriving stock (Eq, Show, Generic, Data, Ord)
  deriving anyclass (Hashable, NFData)

instance HasCodec ComputedFieldDefinition where
  codec =
    AC.object "BigQueryComputedFieldDefinition"
      $ ComputedFieldDefinition
      <$> requiredField' "function"
      AC..= _bqcfdFunction
        <*> optionalField' "return_table"
      AC..= _bqcfdReturnTable
        <*> requiredField' "argument_mapping"
      AC..= _bqcfdArgumentMapping

instance ToJSON ComputedFieldDefinition where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}

instance FromJSON ComputedFieldDefinition where
  parseJSON = J.genericParseJSON hasuraJSON

-- | A argument expression for SQL functions
data ArgumentExp v
  = -- | Value coming from user's input through GraphQL query
    AEInput v
  | -- | For computed fields, value of column from the table
    AETableColumn ColumnName
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (Hashable v) => Hashable (ArgumentExp v)

type ComputedFieldImplicitArguments = HashMap FunctionArgName ColumnName

-- | Returning type of the function underlying a computed field
data ComputedFieldReturn
  = -- | Returns existing table, needs to be present in the metadata
    ReturnExistingTable TableName
  | -- | An arbitrary table schema specified by column name and type pairs
    ReturnTableSchema [(ColumnName, G.Name, ScalarType)]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON ComputedFieldReturn where
  toJSON =
    J.genericToJSON
      $ J.defaultOptions
        { J.constructorTagModifier = J.snakeCase,
          J.sumEncoding = J.TaggedObject "type" "info"
        }

-- | Function input argument specification
data FunctionArgument = FunctionArgument
  { -- | Argument name of a table valued function is required
    -- Ref: https://cloud.google.com/bigquery/docs/reference/standard-sql/data-definition-language#create_table_function_statement
    _faName :: FunctionArgName,
    -- | The data type of the argument
    _faType :: ScalarType
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON FunctionArgument where
  toJSON = J.genericToJSON hasuraJSON

--------------------------------------------------------------------------------
-- Backend-related stuff
--

parseScalarValue :: ScalarType -> J.Value -> Either QErr Value
parseScalarValue scalarType jValue = case scalarType of
  StringScalarType -> StringValue <$> parseJValue jValue
  BytesScalarType -> StringValue <$> parseJValue jValue
  IntegerScalarType -> IntegerValue <$> parseJValue jValue
  FloatScalarType -> FloatValue <$> parseJValue jValue
  BoolScalarType -> BoolValue <$> parseJValue jValue
  DecimalScalarType -> DecimalValue <$> parseJValue jValue
  BigDecimalScalarType -> BigDecimalValue <$> parseJValue jValue
  TimestampScalarType -> TimestampValue <$> parseJValue jValue
  DateScalarType -> DateValue <$> parseJValue jValue
  TimeScalarType -> TimeValue <$> parseJValue jValue
  DatetimeScalarType -> DatetimeValue <$> parseJValue jValue
  GeographyScalarType -> GeographyValue <$> parseJValue jValue
  JsonScalarType -> pure (JsonValue jValue)
  _ -> Left (internalError (T.pack ("Unsupported scalar type: " <> show scalarType <> ": " <> show jValue)))
  -- TODO: These types:
  -- RecordScalarType -> RecordValue <$> parseJValue jValue
  -- StructScalarType -> StructValue <$> parseJValue jValue
  where
    parseJValue :: (J.FromJSON a) => J.Value -> Either QErr a
    parseJValue = runAesonParser J.parseJSON

-- see comparable BigQuery data types in
-- https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types
-- in practice only Geography data type is not comparable
-- as ARRAY isn't a scalar type in the backend
isComparableType, isNumType :: ScalarType -> Bool
isComparableType = \case
  StringScalarType -> True
  BytesScalarType -> True
  IntegerScalarType -> True
  FloatScalarType -> True
  BoolScalarType -> True
  TimestampScalarType -> True
  DateScalarType -> True
  TimeScalarType -> True
  DatetimeScalarType -> True
  GeographyScalarType -> False
  DecimalScalarType -> True
  BigDecimalScalarType -> True
  JsonScalarType -> False
  StructScalarType -> True
isNumType =
  \case
    StringScalarType -> False
    BytesScalarType -> False
    IntegerScalarType -> True
    FloatScalarType -> True
    BoolScalarType -> False
    TimestampScalarType -> False
    DateScalarType -> False
    TimeScalarType -> False
    DatetimeScalarType -> False
    GeographyScalarType -> False
    DecimalScalarType -> True
    BigDecimalScalarType -> True
    JsonScalarType -> False
    StructScalarType -> False

getGQLTableName :: TableName -> Either QErr G.Name
getGQLTableName (TableName table schema) = do
  let textName = schema <> "_" <> table
  onNothing (G.mkName textName)
    $ throw400 ValidationFailed
    $ "cannot include "
    <> textName
    <> " in the GraphQL schema because it is not a valid GraphQL identifier"

--------------------------------------------------------------------------------
-- Liberal numeric parsers/printers (via JSON)
--
-- These are parsers/printers that go via text predominantly, except
-- where in simple cases they go via raw number representations in
-- JSON.

-- These printers may do something more clever later. See PG backend's
-- equivalent functions.
liberalIntegralPrinter :: (Coercible Text a) => a -> J.Value
liberalIntegralPrinter a = J.toJSON (coerce a :: Text)

liberalDecimalPrinter :: (Coercible a Text) => a -> J.Value
liberalDecimalPrinter a = J.toJSON (coerce a :: Text)

-- | Parse from text by simply validating it contains digits;
-- otherwise, require a JSON integer.
liberalInt64Parser :: (Text -> a) -> J.Value -> J.Parser a
liberalInt64Parser fromText json = viaText <|> viaNumber
  where
    viaText = do
      text <- J.parseJSON json
      -- Parsing scientific is safe; it doesn't normalise until we ask
      -- it to.
      case readP_to_S scientificP (T.unpack text) of
        [(sci, "")] | isInteger sci -> pure (fromText text)
        _ -> fail ("String containing integral number is invalid: " ++ show text)
    viaNumber = do
      int <- J.parseJSON json
      pure (fromText (tshow (int :: Int)))

-- | Parse either a JSON native double number, or a text string
-- containing something vaguely in scientific notation. In either
-- case, producing a wrapped Text as the final result.
liberalDecimalParser :: (Text -> a) -> J.Value -> J.Parser a
liberalDecimalParser fromText json = viaText <|> viaNumber
  where
    viaText = do
      text <- J.parseJSON json
      -- Parsing scientific is safe; it doesn't normalise until we ask
      -- it to.
      let -- See https://cloud.google.com/bigquery/docs/reference/standard-sql/conversion_functions#cast_as_floating_point
          isNonFinite =
            let noSign = case T.uncons text of
                  Just ('+', rest) -> rest
                  Just ('-', rest) -> rest
                  _ -> text
             in T.toLower noSign `elem` ["nan", "infinity", "inf"]
      case readP_to_S (scientificP <* eof) (T.unpack text) of
        [_] -> pure (fromText text)
        [] | isNonFinite -> pure (fromText text)
        _ -> fail ("String containing decimal places is invalid: " ++ show text)
    viaNumber = do
      d <- J.parseJSON json
      -- Converting a scientific to an unbounded number is unsafe, but
      -- to a double is bounded and therefore OK. JSON only supports
      -- doubles, so that's fine.
      pure (fromText (tshow (d :: Double)))

projectionAlias :: Projection -> Maybe Text
projectionAlias =
  \case
    ExpressionProjection a -> pure (aliasedAlias a)
    FieldNameProjection a -> pure (aliasedAlias a)
    AggregateProjections a -> pure (aliasedAlias a)
    AggregateProjection a -> pure (aliasedAlias a)
    StarProjection -> Nothing
    ArrayAggProjection a -> pure (aliasedAlias a)
    EntityProjection a -> pure (aliasedAlias a)
    ArrayEntityProjection _ a -> pure (aliasedAlias a)
    WindowProjection a -> pure (aliasedAlias a)

data Job = Job
  { state :: Text,
    jobId :: Text,
    location :: Text
  }
  deriving (Show)

instance FromJSON Job where
  parseJSON = J.withObject "Job" \o -> do
    kind <- o J..: "kind"
    if kind == ("bigquery#job" :: Text)
      then do
        state <- do
          status <- o J..: "status"
          status J..: "state"
        (jobId, location) <- do
          ref <- o J..: "jobReference"
          -- 'location' is needed in addition to 'jobId' to query a job's
          -- status
          (,) <$> ref J..: "jobId" <*> ref J..: "location"
        pure Job {state, jobId, location}
      else fail ("Invalid kind: " <> show kind)

instance ToJSON Job where
  toJSON Job {..} =
    J.object
      [ "id" J..= jobId,
        "location" J..= location,
        "state" J..= state
      ]

data ExecutionStatistics = ExecutionStatistics
  { _esJob :: Job
  }
  deriving stock (Generic)

instance ToJSON ExecutionStatistics where
  toJSON = J.genericToJSON hasuraJSON
