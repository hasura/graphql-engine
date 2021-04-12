{-# LANGUAGE DuplicateRecordFields #-}

-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.Backends.BigQuery.Types where

import qualified Data.Text.Encoding as T
import           Control.DeepSeq
import           Data.Aeson (ToJSONKey,FromJSONKey,ToJSON,FromJSON)
import qualified Data.Aeson as J
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.Hashable
import qualified Data.Text as T
import           Data.Text.Extended
import           Data.Vector (Vector)
import           Data.Vector.Instances ()
import           GHC.Generics
import           Hasura.Incremental.Internal.Dependency
import           Hasura.Prelude
import           Hasura.RQL.Types.Error
import qualified Language.GraphQL.Draft.Syntax as G
import           Language.Haskell.TH.Syntax

data Select = Select
  { selectTop :: !Top
  , selectProjections :: !(NonEmpty Projection)
  , selectFrom :: !From
  , selectJoins :: ![Join]
  , selectWhere :: !Where
  , selectFor :: !For
  , selectOrderBy :: !(Maybe (NonEmpty OrderBy))
  , selectOffset :: !(Maybe Expression)
  , selectGroupBy :: [FieldName]
  , selectFinalWantedFields :: !(Maybe [Text])
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Select
instance Hashable Select
instance Cacheable Select
instance ToJSON Select
instance NFData Select

data ArrayAgg = ArrayAgg
  { arrayAggProjections :: !(NonEmpty Projection)
  , arrayAggOrderBy :: !(Maybe (NonEmpty OrderBy))
  , arrayAggTop :: !Top
  , arrayAggOffset :: !(Maybe Expression)
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON ArrayAgg
instance Hashable ArrayAgg
instance Cacheable ArrayAgg
instance ToJSON ArrayAgg
instance NFData ArrayAgg

data Reselect = Reselect
  { reselectProjections :: !(NonEmpty Projection)
  , reselectFor :: !For
  , reselectWhere :: !Where
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Reselect
instance Hashable Reselect
instance Cacheable Reselect
instance ToJSON Reselect
instance NFData Reselect

data OrderBy = OrderBy
  { orderByFieldName :: FieldName
  , orderByOrder :: Order
  , orderByNullsOrder :: NullsOrder
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON OrderBy
instance Hashable OrderBy
instance Cacheable OrderBy
instance ToJSON OrderBy
instance NFData OrderBy

data Order
  = AscOrder
  | DescOrder
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Order
instance Hashable Order
instance Cacheable Order
instance ToJSON Order
instance NFData Order

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON NullsOrder
instance Hashable NullsOrder
instance Cacheable NullsOrder
instance ToJSON NullsOrder
instance NFData NullsOrder

data For
  = JsonFor ForJson
  | NoFor
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON For
instance Hashable For
instance Cacheable For
instance ToJSON For
instance NFData For

data ForJson = ForJson
  { jsonCardinality :: JsonCardinality
  , jsonRoot :: Root
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON ForJson
instance Hashable ForJson
instance Cacheable ForJson
instance ToJSON ForJson
instance NFData ForJson

data Root
  = NoRoot
  | Root Text
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Root
instance Hashable Root
instance Cacheable Root
instance ToJSON Root
instance NFData Root

data JsonCardinality
  = JsonArray
  | JsonSingleton
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON JsonCardinality
instance Hashable JsonCardinality
instance Cacheable JsonCardinality
instance ToJSON JsonCardinality
instance NFData JsonCardinality

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  | StarProjection
  | ArrayAggProjection (Aliased ArrayAgg)
  | EntityProjection (Aliased EntityAlias)
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Projection
instance Hashable Projection
instance Cacheable Projection
instance ToJSON Projection
instance NFData Projection

data Join = Join
  { joinSource :: !JoinSource
  , joinAlias :: !EntityAlias
  , joinOn :: [(FieldName,FieldName)]
  , joinProvenance :: !JoinProvenance
  , joinFieldName :: !Text
  , joinExtractPath :: !(Maybe Text)
  , joinRightTable :: !EntityAlias
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Join
instance Hashable Join
instance Cacheable Join
instance ToJSON Join
instance NFData Join

data JoinProvenance
  = OrderByJoinProvenance
  | ObjectJoinProvenance
  | ArrayAggregateJoinProvenance
  | ArrayJoinProvenance
  | MultiplexProvenance
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON JoinProvenance
instance Hashable JoinProvenance
instance Cacheable JoinProvenance
instance ToJSON JoinProvenance
instance NFData JoinProvenance

data JoinSource
  = JoinSelect Select
  -- We're not using existingJoins at the moment, which was used to
  -- avoid re-joining on the same table twice.
  -- | JoinReselect Reselect
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON JoinSource
instance Hashable JoinSource
instance Cacheable JoinSource
instance ToJSON JoinSource
instance NFData JoinSource

newtype Where =
  Where [Expression]
  deriving (NFData, Eq, Show, Generic, Data, Lift, FromJSON, ToJSON, Hashable, Cacheable)

instance Monoid Where where
  mempty = Where mempty

instance Semigroup Where where
  (Where x) <> (Where y) = Where (x <> y)

data Top
  = NoTop
  | Top Int
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Top
instance Hashable Top
instance Cacheable Top
instance ToJSON Top
instance NFData Top

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x = x
  (<>) x NoTop = x
  (<>) (Top x) (Top y) = Top (min x y)

data Expression
  = ValueExpression Value
  | InExpression Expression Value
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
  | JsonValueExpression Expression JsonPath
    -- ^ This is for getting actual atomic values out of a JSON
    -- string.
  | OpExpression Op Expression Expression
  | CastExpression Expression ScalarType
  | ConditionalProjection Expression FieldName
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Expression
instance Hashable Expression
instance Cacheable Expression
instance ToJSON Expression
instance NFData Expression

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON JsonPath
instance Hashable JsonPath
instance Cacheable JsonPath
instance ToJSON JsonPath
instance NFData JsonPath

data Aggregate
  = CountAggregate (Countable FieldName)
  | OpAggregates !Text (NonEmpty (Text, Expression))
  | OpAggregate !Text Expression
  | TextAggregate !Text
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Aggregate
instance Hashable Aggregate
instance Cacheable Aggregate
instance ToJSON Aggregate
instance NFData Aggregate

data Countable fieldname
  = StarCountable
  | NonNullFieldCountable (NonEmpty fieldname)
  | DistinctCountable (NonEmpty fieldname)
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON a => FromJSON (Countable a)
instance Hashable a => Hashable (Countable a)
instance Cacheable a => Cacheable (Countable a)
instance ToJSON a => ToJSON (Countable a)
instance NFData a => NFData (Countable a)

data From
  = FromQualifiedTable (Aliased TableName)
  deriving (Eq, Show, Generic, Data, Lift, Ord)
instance FromJSON From
instance Hashable From
instance Cacheable From
instance ToJSON From
instance NFData From

data OpenJson = OpenJson
  { openJsonExpression :: Expression
  , openJsonWith :: NonEmpty JsonFieldSpec
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON OpenJson
instance Hashable OpenJson
instance Cacheable OpenJson
instance ToJSON OpenJson
instance NFData OpenJson

data JsonFieldSpec
  = IntField Text
  | JsonField Text
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON JsonFieldSpec
instance Hashable JsonFieldSpec
instance Cacheable JsonFieldSpec
instance ToJSON JsonFieldSpec
instance NFData JsonFieldSpec

data Aliased a = Aliased
  { aliasedThing :: !a
  , aliasedAlias :: !Text
  } deriving (Eq, Show, Generic, Data, Lift, Functor)
instance FromJSON a => FromJSON (Aliased a)
instance Hashable a => Hashable (Aliased a)
instance Cacheable a => Cacheable (Aliased a)
instance ToJSON a => ToJSON (Aliased a)
instance NFData a => NFData (Aliased a)
deriving instance Ord a => Ord (Aliased a)

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  } deriving (NFData, Eq, Show, Generic, Data, Lift, FromJSON, ToJSON, Hashable, Cacheable)

data TableName = TableName
  { tableName :: Text
  , tableNameSchema :: Text
  } deriving (Eq, Show, Generic, Data, Lift, Ord)
instance FromJSON TableName where
  parseJSON =
    J.withObject
      "TableName"
      (\o -> TableName <$> o J..: "name" <*> o J..: "dataset")
instance ToJSON TableName where
  toJSON TableName{..} = J.object [ "name" J..= tableName, "dataset" J..= tableNameSchema ]
instance Hashable TableName
instance Cacheable TableName
instance ToJSONKey TableName
instance NFData TableName
instance Arbitrary TableName where arbitrary = genericArbitrary

instance ToTxt TableName where toTxt = T.pack . show

data FieldName = FieldName
  { fieldName :: Text
  , fieldNameEntity :: !Text
  } deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON FieldName
instance Hashable FieldName
instance Cacheable FieldName
instance ToJSON FieldName
instance NFData FieldName

newtype ColumnName = ColumnName
  { columnName :: Text
  } deriving (Eq, Ord, Show, Generic, Data, Lift, FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable, Cacheable, NFData, ToTxt)

data Comment = DueToPermission | RequestedSingleObject
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Comment
instance Hashable Comment
instance Cacheable Comment
instance ToJSON Comment
instance NFData Comment
instance Arbitrary ColumnName where arbitrary = genericArbitrary

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  } deriving (NFData, Eq, Show, Generic, Data, Lift, FromJSON, ToJSON, Hashable, Cacheable)

data Op
  = LessOp
  | LessOrEqualOp
  | MoreOp
  | MoreOrEqualOp
  -- | SIN
  -- | SNE
  -- | SLIKE
  -- | SNLIKE
  -- | SILIKE
  -- | SNILIKE
  -- | SSIMILAR
  -- | SNSIMILAR
  -- | SGTE
  -- | SLTE
  -- | SNIN
  -- | SContains
  -- | SContainedIn
  -- | SHasKey
  -- | SHasKeysAny
  -- | SHasKeysAll
  deriving (Eq, Show, Generic, Data, Lift)
instance FromJSON Op
instance Hashable Op
instance Cacheable Op
instance ToJSON Op
instance NFData Op

-- | Source for this represenation type:
--
-- https://developers.google.com/protocol-buffers/docs/reference/google.protobuf#google.protobuf.Value
--
-- BigQuery results come in via the REST API as one of these simply types.
--
-- TODO: This omits StructValue -- do we need it?
data Value
  = NullValue
  | IntegerValue !Int64
    -- ^ 64-bit <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#integer_type>
  | DecimalValue !Decimal
    -- ^ Fixed precision <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#decimal_types>
  | BigDecimalValue !BigDecimal
    -- ^ Fixed precision <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#decimal_types>
  | FloatValue !Float64
    -- ^ Floating point <https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#floating_point_types>
  | GeographyValue !Geography
  | StringValue !Text
  | BytesValue !Base64
  | BoolValue !Bool
  | ArrayValue !(Vector Value)
  | TimestampValue !Timestamp
  | DateValue !Date
  | TimeValue !Time
  | DatetimeValue !Datetime
  deriving (Show, Eq, Ord, Generic, Data, Lift)
instance FromJSON Value
instance Cacheable Value
instance ToJSON Value
instance NFData Value
instance Hashable Value

-- | BigQuery's conception of a timestamp.
newtype Timestamp = Timestamp Text
  deriving (Show, Eq, Ord, Generic, Data, Lift, ToJSON, FromJSON, Cacheable, NFData, Hashable)

-- | BigQuery's conception of a date.
newtype Date = Date Text
  deriving (Show, Eq, Ord, Generic, Data, Lift, ToJSON, FromJSON, Cacheable, NFData, Hashable)

-- | BigQuery's conception of a time.
newtype Time = Time Text
  deriving (Show, Eq, Ord, Generic, Data, Lift, ToJSON, FromJSON, Cacheable, NFData, Hashable)

-- | BigQuery's conception of a datetime.
newtype Datetime = Datetime Text
  deriving (Show, Eq, Ord, Generic, Data, Lift, ToJSON, FromJSON, Cacheable, NFData, Hashable)

-- | BigQuery's conception of an INTEGER/INT64 (they are the same).
newtype Int64 = Int64 Text
  deriving (Show, Eq, Ord, Generic, Data, Lift, ToJSON, FromJSON, Cacheable, NFData, Hashable)

intToInt64 :: Int -> Int64
intToInt64 = Int64 . T.pack . show

-- | BigQuery's conception of a fixed precision decimal.
newtype Decimal = Decimal Text
  deriving (Show, Eq, Ord, Generic, Data, ToJSON, FromJSON, Cacheable, NFData, Hashable, Lift)

doubleToDecimal :: Double -> Decimal
doubleToDecimal = Decimal . T.decodeUtf8 . L.toStrict . J.encode

-- | BigQuery's conception of a \"big\" fixed precision decimal.
newtype BigDecimal = BigDecimal Text
  deriving (Show, Eq, Ord, Generic, Data, ToJSON, FromJSON, Cacheable, NFData, Hashable, Lift)

doubleToBigDecimal :: Double -> BigDecimal
doubleToBigDecimal = BigDecimal . T.decodeUtf8 . L.toStrict . J.encode

-- | BigQuery's conception of a fixed precision decimal.
newtype Float64 = Float64 Text
  deriving (Show, Eq, Ord, Generic, Data, ToJSON, FromJSON, Cacheable, NFData, Hashable, Lift)

doubleToFloat64 :: Double -> Float64
doubleToFloat64 = Float64 . T.decodeUtf8 . L.toStrict . J.encode

-- | A base-64 encoded binary string.
newtype Base64 = Base64
  { unBase64 :: ByteString
  } deriving (Show, Eq, Ord, Generic, Data, Lift)
instance FromJSON Base64 where parseJSON = fmap (Base64 . L.toStrict . base64Decode) . J.parseJSON
instance ToJSON Base64 where toJSON = J.toJSON . T.decodeUtf8 . Base64.encode . unBase64
instance Cacheable Base64
instance NFData Base64
instance Hashable Base64

newtype Geography = Geography
  { unGeography :: Text
  } deriving (Show, Eq, Ord, Generic, Data, Lift, FromJSON, ToJSON)
instance Cacheable Geography
instance NFData Geography
instance Hashable Geography

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
  | StructScalarType
  deriving (Show, Eq, Ord, Generic, Data, Lift)
instance FromJSON ScalarType
instance Cacheable ScalarType
instance ToJSON ScalarType
instance ToJSONKey ScalarType
instance NFData ScalarType
instance Hashable ScalarType
instance ToTxt ScalarType where toTxt = T.pack . show

--------------------------------------------------------------------------------
-- Unified table metadata

data UnifiedMetadata = UnifiedMetadata
  { tables :: ![UnifiedTableMetadata]
  }deriving (Eq, Show)

data UnifiedTableMetadata = UnifiedTableMetadata
  { table :: !UnifiedTableName
  , object_relationships :: ![UnifiedObjectRelationship]
  , array_relationships :: ![UnifiedArrayRelationship]
  , columns :: ![UnifiedColumn]
  }deriving (Eq, Show)

data UnifiedColumn = UnifiedColumn
  { name :: !Text
  , type' :: !ScalarType
  }deriving (Eq, Show)

data UnifiedTableName = UnifiedTableName
  { schema :: !Text
  , name :: !Text
  }deriving (Eq, Show)

data UnifiedObjectRelationship = UnifiedObjectRelationship
  { using :: !UnifiedUsing
  , name :: !Text
  }deriving (Eq, Show)

data UnifiedArrayRelationship = UnifiedArrayRelationship
  { using :: !UnifiedUsing
  , name :: !Text
  }deriving (Eq, Show)

data UnifiedUsing = UnifiedUsing
  { foreign_key_constraint_on :: !UnifiedOn
  }deriving (Eq, Show)

data UnifiedOn = UnifiedOn
  { table :: !UnifiedTableName
  , column :: !Text
  }deriving (Eq, Show)

-- Copied from feature/mssql
newtype FunctionName = FunctionName Text -- TODO: Improve this type when SQL function support added
 deriving (FromJSON, ToJSON, ToJSONKey, ToTxt, Arbitrary, Show, Eq, Ord, Hashable, Cacheable, NFData)

--------------------------------------------------------------------------------
-- Backend-related stuff

parseScalarValue :: ScalarType -> J.Value -> Either QErr Value
parseScalarValue scalarType jValue = case scalarType of
  StringScalarType -> StringValue <$> parseJValue jValue
  BytesScalarType -> StringValue <$> parseJValue jValue
  IntegerScalarType -> IntegerValue <$> parseJValue jValue
  FloatScalarType -> FloatValue <$> parseJValue jValue
  BoolScalarType -> BoolValue <$> parseJValue jValue
  DecimalScalarType -> DecimalValue <$> parseJValue jValue
  BigDecimalScalarType -> BigDecimalValue <$> parseJValue jValue
  _ -> Left (internalError (T.pack ("Unsupported scalar type: " <> show scalarType <> ": " <> show jValue)))
  -- TODO: These types:
  -- TimestampScalarType -> TimestampValue <$> parseJValue jValue
  -- DateScalarType -> DateValue <$> parseJValue jValue
  -- TimeScalarType -> TimeValue <$> parseJValue jValue
  -- DatetimeScalarType -> DatetimeValue <$> parseJValue jValue
  -- GeographyScalarType -> GeographyValue <$> parseJValue jValue
  -- RecordScalarType -> RecordValue <$> parseJValue jValue
  -- StructScalarType -> StructValue <$> parseJValue jValue
  where
    parseJValue :: (J.FromJSON a) => J.Value -> Either QErr a
    parseJValue = runAesonParser J.parseJSON

isComparableType, isNumType :: ScalarType -> Bool

-- TODO: What does this mean?
isComparableType = \case
                      BoolScalarType -> True
                      BytesScalarType -> True
                      _ -> False

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
    StructScalarType -> False

getGQLTableName :: TableName -> Either QErr G.Name
getGQLTableName (TableName table schema) = do
  let textName = schema <> "_" <> table
  onNothing (G.mkName textName) $ throw400 ValidationFailed $
    "cannot include " <> textName <> " in the GraphQL schema because it is not a valid GraphQL identifier"
