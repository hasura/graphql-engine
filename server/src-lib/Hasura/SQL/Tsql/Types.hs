-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Database.ODBC.SQLServer as Odbc
import           Prelude

data Select = Select
  { selectTop :: !Top
  , selectProjections :: !(NonEmpty Projection)
  , selectFrom :: !From
  , selectJoins :: ![Join]
  , selectWhere :: !Where
  , selectFor :: !For
  , selectOrderBy :: !(Maybe (NonEmpty OrderBy))
  , selectOffset :: !(Maybe Expression)
  } deriving (Eq, Show)

data OrderBy = OrderBy
  { orderByFieldName :: FieldName
  , orderByOrder :: Order
  , orderByNullsOrder :: NullsOrder
  } deriving (Eq, Show)

data Order
  = AscOrder
  | DescOrder
  deriving (Eq, Show)

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder
  deriving (Eq, Show)

data For
  = JsonFor JsonCardinality
  | NoFor
  deriving (Eq, Show)

data JsonCardinality
  = JsonArray
  | JsonSingleton
  deriving (Eq, Show)

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  | StarProjection
  deriving (Eq, Show)

data Join = Join
  { joinSelect :: !Select
  , joinJoinAlias :: !JoinAlias
  } deriving (Eq, Show)

data JoinAlias = JoinAlias
  { joinAliasEntity :: Text
  , joinAliasField :: Maybe Text
  } deriving (Eq, Show)

newtype Where =
  Where [Expression]
  deriving (Eq, Show)

instance Monoid Where where
  mempty = Where mempty

instance Semigroup Where where
  (Where x) <> (Where y) = Where (x <> y)

data Top
  = NoTop
  | Top Int
  deriving (Eq, Show)

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x = x
  (<>) x NoTop = x
  (<>) (Top x) (Top y) = Top (min x y)

data Expression
  = ValueExpression Odbc.Value
  | AndExpression [Expression]
  | OrExpression [Expression]
  | NotExpression Expression
  | ExistsExpression Select
  | IsNullExpression Expression
  | ColumnExpression FieldName
  | EqualExpression Expression Expression
  | JsonQueryExpression Expression
  | ToStringExpression Expression
  deriving (Eq, Show)

data Aggregate
  = CountAggregate Countable
  | OpAggregate !Text (NonEmpty Expression)
  | TextAggregate !Text
  deriving (Eq, Show)

data Countable
  = StarCountable
  | NonNullFieldCountable (NonEmpty FieldName)
  | DistinctCountable (NonEmpty FieldName)
  deriving (Eq, Show)

data From =
  FromQualifiedTable (Aliased TableName)
  deriving (Eq, Show)

data Aliased a = Aliased
  { aliasedThing :: !a
  , aliasedAlias :: !Text
  } deriving (Eq, Show, Functor)

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  } deriving (Eq, Show)

data TableName = TableName
  { tableName :: Text
  , tableNameSchema :: Text
  } deriving (Eq, Show)

data FieldName = FieldName
  { fieldName :: Text
  , fieldNameEntity :: !Text
  } deriving (Eq, Show)

data Comment = DueToPermission | RequestedSingleObject
  deriving (Eq, Show)

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  } deriving (Eq, Show)
