-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.Backends.MSSQL.Types.Internal where

import           Data.Data
import           Data.Text.Extended (ToTxt)
import           GHC.Generics
import           Language.Haskell.TH.Syntax (Lift)
import           Prelude

import qualified Database.ODBC.SQLServer as ODBC

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)


data Select = Select
  { selectTop         :: !Top
  , selectProjections :: !(NonEmpty Projection)
  , selectFrom        :: !From
  , selectJoins       :: ![Join]
  , selectWhere       :: !Where
  , selectFor         :: !For
  , selectOrderBy     :: !(Maybe (NonEmpty OrderBy))
  , selectOffset      :: !(Maybe Expression)
  } deriving (Data, Generic, Eq, Show)

data Reselect = Reselect
  { reselectProjections :: !(NonEmpty Projection)
  , reselectFor         :: !For
  , reselectWhere       :: !Where
  } deriving (Data, Generic, Eq, Show)

data OrderBy = OrderBy
  { orderByFieldName  :: FieldName
  , orderByOrder      :: Order
  , orderByNullsOrder :: NullsOrder
  } deriving (Data, Generic, Eq, Show)

data Order
  = AscOrder
  | DescOrder
  deriving (Data, Generic, Eq, Show, Lift)

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder
  deriving (Data, Generic, Eq, Show, Lift)

data For
  = JsonFor ForJson
  | NoFor
  deriving (Data, Generic, Eq, Show)

data ForJson = ForJson
  { jsonCardinality :: JsonCardinality
  , jsonRoot        :: Root
  } deriving (Data, Generic, Eq, Show)

data Root
  = NoRoot
  | Root Text
  deriving (Data, Generic, Eq, Show)

data JsonCardinality
  = JsonArray
  | JsonSingleton
  deriving (Data, Generic, Eq, Show)

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  | StarProjection
  deriving (Data, Generic, Eq, Show)

data Join = Join
  { joinSource    :: !JoinSource
  , joinJoinAlias :: !JoinAlias
  } deriving (Data, Generic, Eq, Show)

data JoinSource
  = JoinSelect Select
  | JoinReselect Reselect
  deriving (Data, Generic, Eq, Show)

data JoinAlias = JoinAlias
  { joinAliasEntity :: Text
  , joinAliasField  :: Maybe Text
  } deriving (Data, Generic, Eq, Show)

newtype Where =
  Where [Expression]
  deriving (Data, Generic, Eq, Show)

instance Monoid Where where
  mempty = Where mempty

instance Semigroup Where where
  (Where x) <> (Where y) = Where (x <> y)

data Top
  = NoTop
  | Top Int
  deriving (Data, Generic, Eq, Show)

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x         = x
  (<>) x NoTop         = x
  (<>) (Top x) (Top y) = Top (min x y)

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
  | JsonValueExpression Expression JsonPath
    -- ^ This is for getting actual atomic values out of a JSON
    -- string.
  | OpExpression Op Expression Expression
  deriving (Data, Generic, Eq, Show)

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer
  deriving (Data, Generic, Eq, Show)

data Aggregate
  = CountAggregate Countable
  | OpAggregate !Text (NonEmpty Expression)
  | TextAggregate !Text
  deriving (Data, Generic, Eq, Show)

data Countable
  = StarCountable
  | NonNullFieldCountable (NonEmpty FieldName)
  | DistinctCountable (NonEmpty FieldName)
  deriving (Data, Generic, Eq, Show)

data From
  = FromQualifiedTable (Aliased TableName)
  | FromOpenJson (Aliased OpenJson)
  deriving (Data, Generic, Eq, Show)

data OpenJson = OpenJson
  { openJsonExpression :: Expression
  , openJsonWith       :: NonEmpty JsonFieldSpec
  } deriving (Data, Generic, Eq, Show)

data JsonFieldSpec
  = IntField Text
  | JsonField Text
  deriving (Data, Generic, Eq, Show)

data Aliased a = Aliased
  { aliasedThing :: !a
  , aliasedAlias :: !Text
  } deriving (Data, Generic, Eq, Show, Functor)

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  } deriving (Data, Generic, Eq, Show)

data TableName = TableName
  { tableName       :: Text
  , tableNameSchema :: Text
  } deriving (Data, Generic, Eq, Show, Lift, Ord)

data FieldName = FieldName
  { fieldName       :: Text
  , fieldNameEntity :: !Text
  } deriving (Data, Generic, Eq, Show)

data Comment = DueToPermission | RequestedSingleObject
  deriving (Data, Generic, Eq, Show)

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  } deriving (Data, Generic, Eq, Show)

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
  deriving (Data, Generic, Eq, Show)

-- | Column name of some database table -- this differs to FieldName
-- that is used for referring to things within a query.
newtype ColumnName = ColumnName { columnNameText :: Text }
  deriving (Data, Generic, Eq, Show, Ord, ToTxt)

-- | Derived from the odbc package.
data ScalarType
  = CharType
  | NumericType
  | DecimalType
  | IntegerType
  | SmallintType
  | FloatType
  | RealType
  | DoubleType
  | DateType
  | Ss_time2Type
  | VarcharType
  | WcharType
  | WvarcharType
  | WlongvarcharType
  | TimeType
  | TimestampType
  | LongvarcharType
  | BinaryType
  | VarbinaryType
  | BigintType
  | TinyintType
  | BitType
  | GuidType
  deriving (Data, Generic, Show, Eq, Ord)

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
  DoubleType   -> True
  BigintType   -> True
  TinyintType  -> True
  _            -> False
