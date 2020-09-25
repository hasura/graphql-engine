-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Database.ODBC.SQLServer as Odbc
import           Prelude

data Select = Select
  { selectTop :: !(Commented Top)
  , selectProjections :: !(NonEmpty Projection)
  , selectFrom :: !From
  , selectJoins :: ![Join]
  , selectWhere :: !Where
  , selectFor :: !For
  } deriving (Eq, Show)

data For
  = JsonFor
  | NoFor
  deriving (Eq, Show)

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  deriving (Eq, Show)

data Join = Join
  { joinSelect :: !Select
  , joinAlias :: !Text
  } deriving (Eq, Show)

data Where
  = NoWhere
  | ExpressionWhere Expression
  deriving (Eq, Show)

data Top
  = NoTop
  | Top Int
  deriving (Eq, Show)

data Expression
  = ValueExpression Odbc.Value
  | AndExpression [Expression]
  | OrExpression [Expression]
  | NotExpression Expression
  | SelectExpression Select
  | IsNullExpression Expression
  | ColumnExpression FieldName
  deriving (Eq, Show)

data Aggregate
  = CountAggregate Countable
  | OpAggregate !Text FieldName
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

data Commented a = Commented
  { commentedComment :: !(Maybe Comment)
  , commentedThing :: !a
  } deriving (Eq, Show, Functor)

data Comment = DueToPermission
  deriving (Eq, Show)

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  } deriving (Eq, Show)
